# Data Entry Shiny App (browser‑friendly)

This Shiny application provides a generic **data entry interface based on an Excel metadata template**.  
It supports **saving and resuming work via a single `.rds` file**, making it suitable for both:

- classic Shiny (local R / server)
- browser‑only deployment using **Shinylive**

---

## What this app does

- Reads a metadata template (`.xlsx`)
- Creates one input tab **per sheet**
- Allows adding, viewing, and deleting records per tab
- Saves **all results into a single `.rds` file**
- Allows resuming work by uploading a previously saved `.rds`

---

## Data format

Downloaded results are stored as:

## Metadata template

The application requires a metadata template to define the structure of the data entry forms.

The **official template** is available here:

➡️ [`templates/metadata_template.xlsx`](templates/metadata_template.xlsx)

Users must not modify the structure of the template unless explicitly instructed.


```r
list(
  TabName1 = data.frame(...),
  TabName2 = data.frame(...),
  ...
)
``