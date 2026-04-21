rm(list = ls())

# ---- Packages ----
req_pkgs <- c("shiny", "DT", "readxl")
missing <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nPlease install them first with install.packages(...).")
}

library(shiny)
library(DT)
library(readxl)

# ---- Config ----
app_title <- "Data Entry"

# ---- Helpers: parse template to panel_metadata ----
parse_choices <- function(choice_str) {
  if (is.na(choice_str) || choice_str == "") return(NULL)
  parts <- strsplit(choice_str, ";")[[1]]
  choices <- list()
  for (part in parts) {
    kv <- strsplit(trimws(part), "=")[[1]]
    if (length(kv) == 2) {
      key <- kv[1]
      value <- kv[2]
      if (value == "None") {
        value <- NA
      } else if (!is.na(suppressWarnings(as.numeric(value)))) {
        value <- as.numeric(value)
      }
      choices[[key]] <- value
    }
  }
  if (length(choices) == 0) return(NULL)
  choices
}

read_fields <- function(sheet_data) {
  lapply(seq_len(nrow(sheet_data)), function(i) {
    row <- sheet_data[i, ]
    list(
      id = row$id,
      type = row$type,
      label = row$label,
      default = if (is.na(row$default)) NULL else row$default,
      choices = parse_choices(row$choices)
    )
  })
}

build_panel_metadata_from_xlsx <- function(template_path) {
  sheet_names <- excel_sheets(template_path)
  all_sheets <- lapply(sheet_names, function(sheet) read_excel(template_path, sheet = sheet))
  names(all_sheets) <- sheet_names
  
  # Add Missing=None to all rows that have choices
  for (i in seq_along(all_sheets)) {
    idx <- !is.na(all_sheets[[i]][["choices"]])
    all_sheets[[i]][["choices"]][idx] <- paste0(all_sheets[[i]][["choices"]][idx], "; Missing=None")
  }
  
  panel_metadata <- vector("list", length(all_sheets))
  names(panel_metadata) <- names(all_sheets)
  for (i in seq_along(panel_metadata)) {
    fn <- names(panel_metadata)[i]
    panel_metadata[[fn]] <- list(title = fn, fields = read_fields(all_sheets[[fn]]))
  }
  panel_metadata
}

# ---- UI builder ----
make_ui <- function(fields, prefix = "") {
  lapply(fields, function(f) {
    inputId <- paste0(prefix, f$id)
    if (f$type == "radioButtons") {
      values <- unname(f$choices)
      labels <- names(f$choices)
      ui_labels <- paste0(labels, " (", ifelse(is.na(values), "NA", values), ")")
      radioButtons(
        inputId = inputId,
        label = paste0(f$label, " (", f$id, ")"),
        choiceNames = ui_labels,
        choiceValues = values,
        selected = character(0),
        width = "50%"
      )
    } else if (f$type == "textInput") {
      textInput(inputId, paste0(f$label, " (", f$id, ")"), value = f$default, width = "50%")
    } else if (f$type == "numericInput") {
      numericInput(inputId, paste0(f$label, " (", f$id, ")"), value = f$default, width = "50%")
    } else {
      NULL
    }
  })
}

# ---- Helpers: create/align dataframes for panels ----
empty_panel_df <- function(fields) {
  cols <- vapply(fields, `[[`, "", "id")
  out <- as.data.frame(setNames(replicate(length(cols), logical(0), simplify = FALSE), cols))
  # Force column types: start empty; we'll coerce on insert anyway
  out[0, , drop = FALSE]
}

align_df_to_fields <- function(df, fields) {
  cols <- vapply(fields, `[[`, "", "id")
  
  # Ensure data.frame
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # Add missing columns
  missing_cols <- setdiff(cols, names(df))
  for (mc in missing_cols) df[[mc]] <- NA
  
  # Drop extra columns (keeps file stable across versions)
  extra_cols <- setdiff(names(df), cols)
  if (length(extra_cols) > 0) {
    df <- df[, cols, drop = FALSE]
  } else {
    df <- df[, cols, drop = FALSE]
  }
  
  df
}

# ---- UI ----
ui <- fluidPage(
  titlePanel(app_title),
  
  div(
    class = "container-fluid",
    
    # ---- SETUP (only visible BEFORE start) ----
    conditionalPanel(
      condition = "output.ready !== true",
      
      div(
        style = "
          padding:16px;
          border:1px solid #ddd;
          border-radius:12px;
          margin-bottom:20px;
          background-color:#fafafa;
        ",
        h4("Setup", style = "margin-top:0;"),
        
        fluidRow(
          column(
            width = 6,
            tags$strong("1. Metadata template"),
            fileInput(
              "template_file",
              label = NULL,
              accept = c(".xlsx"),
              buttonLabel = "Browse…",
              placeholder = "Select metadata template (.xlsx)"
            ),
            tags$small(
              class = "text-muted",
              "This template defines the structure of the data entry forms."
            )
          ),
          
          column(
            width = 6,
            tags$strong("2. Start from existing results (.rds) (optional)"),
            fileInput(
              "existing_results_rds",
              label = NULL,
              accept = c(".rds"),
              buttonLabel = "Browse…",
              placeholder = "Optional: upload previous results (.rds)"
            ),
            tags$small(
              class = "text-muted",
              "Upload a previously downloaded results .rds to continue data entry."
            )
          )
        ),
        
        br(),
        
        div(
          style = "
            background-color:#f5f7fa;
            border:1px solid #e1e4e8;
            border-radius:6px;
            padding:8px 12px;
            font-size:90%;
          ",
          verbatimTextOutput("setup_status")
        ),
        
        hr(style = "margin:12px 0;"),
        
        div(
          style = "text-align:center;",
          actionButton(
            "btn_start",
            "Load template and (when applicable) existing results",
            class = "btn-primary",
            style = "min-width:220px;"
          )
        ),
        
        
      )
    ),
    
    # ---- AFTER START: download button ----
    conditionalPanel(
      condition = "output.ready === true",
      div(
        style = "margin-bottom:20px;",
        downloadButton(
          "download_results_rds",
          "Download results (.rds)",
          class = "btn-success",
          style = "min-width:220px;"
        )
      )
    )
  ),
  
  # ---- MAIN DATA ENTRY UI ----
  uiOutput("main_ui")
)

# ---- Server ----
server <- function(input, output, session) {
  # ---- Helpers: summarize template / results / check match ----
  
  summarize_template_xlsx <- function(path) {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
    
    sheets <- readxl::excel_sheets(path)
    per_sheet <- lapply(sheets, function(sh) {
      df <- readxl::read_excel(path, sheet = sh)
      
      # Expect columns: id, type, label, default, choices
      # Be defensive: allow missing columns without crashing
      nms <- names(df)
      ids <- if ("id" %in% nms) as.character(df$id) else character(0)
      ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
      
      types_raw <- if ("type" %in% nms) as.character(df$type) else character(0)
      types_raw <- trimws(types_raw)
      types_raw <- types_raw[!is.na(types_raw) & nzchar(types_raw)]
      
      type_table <- sort(table(types_raw))
      
      
      # duplicates within sheet
      dup_ids <- unique(ids[duplicated(ids)])
      
      # radioButtons checks
      rb_rows <- integer(0)
      rb_missing_choices <- integer(0)
      if ("type" %in% nms) {
        rb_rows <- which(trimws(as.character(df$type)) == "radioButtons")
        if (length(rb_rows) > 0) {
          if ("choices" %in% nms) {
            ch <- df$choices[rb_rows]
            missing <- is.na(ch) | !nzchar(trimws(as.character(ch)))
            rb_missing_choices <- rb_rows[missing]
          } else {
            rb_missing_choices <- rb_rows
          }
        }
      }
      
      list(
        sheet = sh,
        n_rows = nrow(df),
        n_ids = length(ids),
        ids = ids,
        dup_ids = dup_ids,
        types = names(type_table),
        type_counts = as.integer(type_table),
        type_table = type_table,
        radio_n = length(rb_rows),
        radio_missing_choices_n = length(rb_missing_choices)
      )
    })
    names(per_sheet) <- sheets
    
    list(
      sheets = sheets,
      per_sheet = per_sheet
    )
  }
  
  summarize_results_rds <- function(path) {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
    
    obj <- readRDS(path)
    if (!is.list(obj)) {
      return(list(is_list = FALSE, tabs = character(0), per_tab = list()))
    }
    
    tabs <- names(obj)
    tabs <- tabs[!is.na(tabs) & nzchar(tabs)]
    
    per_tab <- lapply(tabs, function(tb) {
      x <- obj[[tb]]
      if (is.data.frame(x)) {
        cols <- names(x)
        cols <- cols[!is.na(cols) & nzchar(cols)]
        list(tab = tb, is_df = TRUE, n_rows = nrow(x), cols = cols)
      } else {
        list(tab = tb, is_df = FALSE, n_rows = NA_integer_, cols = character(0))
      }
    })
    names(per_tab) <- tabs
    
    list(
      is_list = TRUE,
      tabs = tabs,
      per_tab = per_tab
    )
  }
  
  compare_template_results <- function(template_info, results_info) {
    # Return NULL if insufficient info
    if (is.null(template_info)) return(NULL)
    if (is.null(results_info) || !isTRUE(results_info$is_list)) {
      return(list(
        ok = TRUE,
        notes = c("No results file provided (starting from empty results)."),
        missing_tabs = character(0),
        extra_tabs = character(0),
        per_tab = list()
      ))
    }
    
    tmpl_tabs <- template_info$sheets
    res_tabs  <- results_info$tabs
    
    missing_tabs <- setdiff(tmpl_tabs, res_tabs)
    extra_tabs   <- setdiff(res_tabs, tmpl_tabs)
    
    per_tab <- list()
    for (tb in intersect(tmpl_tabs, res_tabs)) {
      # template expected ids
      exp_ids <- template_info$per_sheet[[tb]]$ids
      # results columns (only if df)
      rt <- results_info$per_tab[[tb]]
      if (!isTRUE(rt$is_df)) {
        per_tab[[tb]] <- list(
          tab = tb,
          ok = FALSE,
          note = "Tab exists in results but is not a data.frame.",
          missing_cols = exp_ids,
          extra_cols = character(0)
        )
      } else {
        have <- rt$cols
        missing_cols <- setdiff(exp_ids, have)
        extra_cols <- setdiff(have, exp_ids)
        per_tab[[tb]] <- list(
          tab = tb,
          ok = (length(missing_cols) == 0),
          note = NULL,
          missing_cols = missing_cols,
          extra_cols = extra_cols
        )
      }
    }
    
    ok_overall <- (length(missing_tabs) == 0) &&
      all(vapply(per_tab, function(x) isTRUE(x$ok), logical(1)))
    
    notes <- character(0)
    if (length(missing_tabs) > 0) notes <- c(notes, paste0("Missing tabs in results: ", paste(missing_tabs, collapse = ", ")))
    if (length(extra_tabs) > 0)   notes <- c(notes, paste0("Extra tabs in results (ignored on load): ", paste(extra_tabs, collapse = ", ")))
    if (length(notes) == 0) notes <- c("Template and results structure match (for shared tabs).")
    
    list(
      ok = ok_overall,
      notes = notes,
      missing_tabs = missing_tabs,
      extra_tabs = extra_tabs,
      per_tab = per_tab
    )
  }
  
        
        
  rv <- reactiveValues(
    template_path = NULL,
    panel_metadata = NULL,
    ready = FALSE
  )
  
  output$ready <- reactive({ isTRUE(rv$ready) })
  outputOptions(output, "ready", suspendWhenHidden = FALSE)
  
  
  # Store per-panel data as reactiveValues (one data.frame per panel)
  rv_data <- reactiveValues()
  
  # Cache structure summaries so we don't re-read files on every re-render
  template_info <- reactiveVal(NULL)
  results_info  <- reactiveVal(NULL)
  match_info    <- reactiveVal(NULL)
  
  observeEvent(input$template_file$datapath, {
    if (is.null(input$template_file$datapath)) {
      template_info(NULL)
      match_info(NULL)
      return()
    }
    ti <- summarize_template_xlsx(input$template_file$datapath)
    template_info(ti)
    
    # update match if results already present
    mi <- compare_template_results(ti, results_info())
    match_info(mi)
  }, ignoreInit = TRUE)
  
  observeEvent(input$existing_results_rds$datapath, {
    if (is.null(input$existing_results_rds$datapath)) {
      results_info(NULL)
      # match with "no results"
      mi <- compare_template_results(template_info(), NULL)
      match_info(mi)
      return()
    }
    ri <- summarize_results_rds(input$existing_results_rds$datapath)
    results_info(ri)
    
    mi <- compare_template_results(template_info(), ri)
    match_info(mi)
  }, ignoreInit = TRUE)
  
  
  # Helper to build the "results list" from current reactiveValues
  current_results_list <- reactive({
    req(rv$ready)
    pnms <- names(rv$panel_metadata)
    out <- setNames(vector("list", length(pnms)), pnms)
    for (pn in pnms) {
      out[[pn]] <- rv_data[[pn]]
    }
    out
  })
  
  # Setup status
  output$setup_status <- renderText({
    tmpl <- if (!is.null(input$template_file$name)) input$template_file$name else "(no template selected)"
    rds  <- if (!is.null(input$existing_results_rds$name)) input$existing_results_rds$name else "(no existing results)"
    started <- if (isTRUE(rv$ready)) "YES" else "NO"
    
    # existing counts per tab (only after ready)
    counts_txt <- ""
    if (isTRUE(rv$ready) && !is.null(rv$panel_metadata)) {
      pnms <- names(rv$panel_metadata)
      counts <- vapply(pnms, function(pn) {
        df <- rv_data[[pn]]
        if (is.null(df)) 0L else nrow(df)
      }, integer(1))
      
      total <- sum(counts)
      lines <- paste0(" - ", pnms, ": ", counts)
      counts_txt <- paste0(
        "\n\nRecords loaded/entered per tab:\n",
        paste(lines, collapse = "\n"),
        "\nTotal records: ", total
      )
    }
    
    # ---- NEW: template structure ----
    ti <- template_info()
    template_txt <- ""
    if (!is.null(ti)) {
      sheet_lines <- vapply(ti$per_sheet, function(s) {
        
        # type counts: textInput: 2 | numericInput: 1 | radioButtons: 1
        type_part <- if (!is.null(s$type_table) && length(s$type_table) > 0) {
          paste(
            paste0(names(s$type_table), ": ", as.integer(s$type_table)),
            collapse = " | "
          )
        } else {
          "no types found"
        }
        
        dup_part <- if (length(s$dup_ids) > 0)
          paste0(" | DUP ids: ", paste(s$dup_ids, collapse = ", "))
        else ""
        
        rb_part <- if (s$radio_n > 0)
          paste0(" | radioButtons: ", s$radio_n,
                 " (missing choices: ", s$radio_missing_choices_n, ")")
        else ""
        
        paste0(
          " - ", s$sheet, ": ",
          s$n_ids, " ids | ",
          type_part,
          dup_part,
          rb_part
        )
      }, character(1))
      
      
      template_txt <- paste0(
        "\n\nTemplate structure:\n",
        "Tabs (sheets): ", paste(ti$sheets, collapse = ", "),
        "\nIDs per tab:\n", paste(sheet_lines, collapse = "\n")
      )
    }
    
    # ---- NEW: results structure ----
    ri <- results_info()
    results_txt <- ""
    if (!is.null(ri)) {
      if (!isTRUE(ri$is_list)) {
        results_txt <- "\n\nResults structure:\n - Uploaded .rds is not a list (expected a list of data.frames)."
      } else {
        tab_lines <- vapply(ri$per_tab, function(t) {
          if (!isTRUE(t$is_df)) {
            paste0(" - ", t$tab, ": NOT a data.frame")
          } else {
            paste0(" - ", t$tab, ": ", length(t$cols), " columns (", t$n_rows, " rows)")
          }
        }, character(1))
        
        results_txt <- paste0(
          "\n\nResults structure:\n",
          "Tabs: ", paste(ri$tabs, collapse = ", "),
          "\nColumns per tab:\n", paste(tab_lines, collapse = "\n")
        )
      }
    }
    
    # ---- NEW: match check ----
    mi <- match_info()
    match_txt <- ""
    if (!is.null(mi) && !isTRUE(rv$ready)) {
      status <- if (isTRUE(mi$ok)) "OK" else "CHECK NEEDED"
      match_txt <- paste0(
        "\n\nTemplate ↔ Results check: ", status,
        "\n", paste0(" - ", mi$notes, collapse = "\n")
      )
      
      # If there are column mismatches, list only the first few for readability
      if (!isTRUE(mi$ok) && length(mi$per_tab) > 0) {
        problems <- vapply(mi$per_tab, function(x) {
          if (isTRUE(x$ok)) return(NA_character_)
          miss <- if (length(x$missing_cols) > 0) paste0("missing: ", paste(head(x$missing_cols, 10), collapse = ", ")) else NULL
          extra <- if (length(x$extra_cols) > 0) paste0("extra: ", paste(head(x$extra_cols, 10), collapse = ", ")) else NULL
          paste0("   * ", x$tab, " → ", paste(c(miss, extra), collapse = " | "))
        }, character(1))
        problems <- problems[!is.na(problems)]
        if (length(problems) > 0) {
          match_txt <- paste0(match_txt, "\nColumn mismatches:\n", paste(problems, collapse = "\n"))
        }
      }
    }
    
    paste0(
      "Template: ", tmpl,
      "\nExisting results: ", rds,
      "\nStarted: ", started,
      counts_txt,
      template_txt,
      results_txt,
      match_txt
    )
  })
  
  
  
  # Start: build panel_metadata + initialize data (empty or from uploaded rds)
  observeEvent(input$btn_start, {
    req(input$template_file$datapath)
    
    loaded <- NULL   # ✅ IMPORTANT FIX
    
    pm <- build_panel_metadata_from_xlsx(input$template_file$datapath)
    
    rv$template_path <- input$template_file$datapath
    rv$panel_metadata <- pm
    
    # 1) Initialize empty dfs for all panels
    for (pn in names(pm)) {
      rv_data[[pn]] <- empty_panel_df(pm[[pn]]$fields)
    }
    
    # 2) If existing results provided, load & align
    if (!is.null(input$existing_results_rds$datapath)) {
      loaded <- readRDS(input$existing_results_rds$datapath)
      
      validate(need(is.list(loaded), "Uploaded .rds must contain a LIST (one element per tab)."))
      
      # Align per panel name; ignore extras; fill missing with empty
      for (pn in names(pm)) {
        if (!is.null(loaded[[pn]])) {
          rv_data[[pn]] <- align_df_to_fields(loaded[[pn]], pm[[pn]]$fields)
        } else {
          rv_data[[pn]] <- empty_panel_df(pm[[pn]]$fields)
        }
      }
    }
    
    # --- OPTIONAL: warn if template/results structure mismatch (does not block) ---
    mi <- compare_template_results(template_info(), results_info())
    if (!is.null(mi) && !isTRUE(mi$ok)) {
      showNotification(
        "Template/results structure mismatch detected. See Setup status for details.",
        type = "warning"
      )
    }
    
    
    if (!is.null(input$existing_results_rds$datapath)) {
      loaded <- readRDS(input$existing_results_rds$datapath)
      found <- intersect(names(loaded), names(pm))
      showNotification(
        paste0("Loaded existing results for ", length(found), " / ", length(names(pm)), " tabs."),
        type = "message"
      )
    }
    
    if (!is.null(loaded)) {
      extra_tabs <- setdiff(names(loaded), names(pm))
      if (length(extra_tabs) > 0) {
        showNotification(
          paste0(
            "Note: ignored extra tabs in uploaded .rds: ",
            paste(extra_tabs, collapse = ", ")
          ),
          type = "warning"
        )
      }
    }
    
    
    
    
    rv$ready <- TRUE
    showNotification("Template loaded. You can start entering data.", type = "message")
    
  }, ignoreInit = TRUE)
  
  # Download results as ONE .rds list
  output$download_results_rds <- downloadHandler(
    filename = function() {
      paste0("data_entry_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      res <- current_results_list()
      saveRDS(res, file)
    }
  )
  
  # Render main UI dynamically
  output$main_ui <- renderUI({
    req(rv$ready)
    req(rv$panel_metadata)
    
    do.call(tabsetPanel, c(
      id = "main_tabs",
      lapply(names(rv$panel_metadata), function(panel_name) {
        panel <- rv$panel_metadata[[panel_name]]
        tabPanel(
          panel$title,
          actionButton(paste0("reset_", panel_name), "Start New Entry",
                       style = "color: white; background-color: blue; border-color: darkblue"),
          do.call(tagList, make_ui(panel$fields, prefix = paste0(panel_name, "_"))),
          actionButton(paste0("submit_", panel_name), "Submit",
                       style = "color: white; background-color: green; border-color: darkgreen"),
          DTOutput(paste0("table_", panel_name)),
          actionButton(paste0("delete_selected_", panel_name), "Delete selected record",
                       style = "color: white; background-color: red; border-color: darkred")
        )
      })
    ))
  })
  
  # Create per-panel observers ONCE after start
  observeEvent(rv$ready, {
    req(rv$panel_metadata)
    
    for (panel_name in names(rv$panel_metadata)) {
      local({
        pn <- panel_name
        fields <- rv$panel_metadata[[pn]]$fields
        table_id <- paste0("table_", pn)
        submit_id <- paste0("submit_", pn)
        reset_id <- paste0("reset_", pn)
        delete_id <- paste0("delete_selected_", pn)
        
        output[[table_id]] <- renderDT({
          rv_data[[pn]]
        }, selection = "single")
        
        observeEvent(input[[submit_id]], {
          # Build new row
          new_entry <- as.list(setNames(
            lapply(fields, function(f) {
              val <- input[[paste0(pn, "_", f$id)]]
              if (f$type %in% c("numericInput", "radioButtons")) {
                as.numeric(val)
              } else {
                val
              }
            }),
            vapply(fields, `[[`, "", "id")
          ))
          
          df <- rv_data[[pn]]
          df <- rbind(as.data.frame(new_entry, stringsAsFactors = FALSE), df)
          df <- align_df_to_fields(df, fields)  # keep schema stable
          rv_data[[pn]] <- df
        }, ignoreInit = TRUE)
        
        observeEvent(input[[reset_id]], {
          for (f in fields) {
            inputId <- paste0(pn, "_", f$id)
            if (f$type == "textInput") {
              updateTextInput(session, inputId, value = f$default)
            } else if (f$type == "numericInput") {
              updateNumericInput(session, inputId, value = f$default)
            } else if (f$type == "radioButtons") {
              updateRadioButtons(session, inputId, selected = character(0))
            }
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[delete_id]], {
          selected <- input[[paste0(table_id, "_rows_selected")]]
          if (!is.null(selected) && length(selected) > 0) {
            df <- rv_data[[pn]]
            if (nrow(df) >= selected) {
              df <- df[-selected, , drop = FALSE]
              rv_data[[pn]] <- df
            }
          }
        }, ignoreInit = TRUE)
      })
    }
  }, ignoreInit = TRUE, once = TRUE)
}

shinyApp(ui = ui, server = server)