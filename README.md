# Data Entry Shiny App (browser‑friendly)

## 🔗 Online application
The application is available here:  
👉 **https://sylvieremy.github.io/data-entry-shiny/**

No installation or login is required — the app runs entirely in your web browser.

---

## What this app does

This Shiny application provides a generic **data entry interface driven by an Excel metadata template**.  
It supports **saving and resuming work via a single `.rds` file**, and can be used both:

- locally (classic Shiny, running in R)
- browser‑only, via **Shinylive**

Main functionality:
- Reads a metadata template (`.xlsx`)
- Creates one input tab **per sheet**
- Allows adding, viewing, and deleting records per tab
- Stores all results in **one `.rds` file**
- Allows resuming work by uploading a previously saved `.rds`

---

## What happens to your data?

### Short answer
**All data stays in your own web browser.**

Nothing is sent to a server, stored centrally, or shared with others unless **you explicitly download it yourself**.

- Data is entered locally
- Data exists only during your browser session
- Closing or refreshing the page resets the app
- Saving is explicit, via a manual `.rds` download

This makes the app suitable for sensitive or preliminary data entry.

---

### Technical background (for interested users)

The app is built using **Shiny + Shinylive**:

- The application is served as a **static website** (GitHub Pages)
- All logic runs **client‑side** in your browser
- R code is executed via **WebAssembly (webR)**
- There is **no R server**, database, or API backend

Data:
- exists only in browser memory
- is not written to disk unless you download it
- is never transmitted automatically

There are:
- ❌ no background uploads  
- ❌ no cookies used for data storage  
- ❌ no logging of user input  

---

## Data persistence and reuse

- Data is **not saved automatically**
- To keep your work:
  - download the results as a `.rds` file
  - reload that file later using **“Start from existing results”**

This explicit workflow ensures transparency and user control.

---


## Metadata template

The application requires a metadata template to define the structure of the data entry forms.

The **official template** is available here:

➡️ [`templates/metadata_template.xlsx`](templates/metadata_template.xlsx)

Users must not modify the structure of the template unless explicitly instructed.

---

## Data format

Downloaded results are stored as a **named list of data frames**, one per tab:

```r
list(
  TabName1 = data.frame(...),
  TabName2 = data.frame(...),
  ...
)
``