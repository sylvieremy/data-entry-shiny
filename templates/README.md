# Metadata template

This folder contains the **official metadata template** used by the Data Entry app.

## Purpose

The structure of the template:
- defines the input tabs shown in the app
- determines which fields can be entered
- is required for correct loading of existing results

Users must always use **this template** (or a compatible version) when:
- starting a new data entry session
- resuming work from a previously saved `.rds` file

## Structure rules

Each sheet in the Excel file:
- becomes one tab in the application
- must follow the expected column structure (`id`, `type`, `label`, …)

The app will:
- ignore unknown tabs when loading an `.rds`
- ignore unknown columns
- add missing columns with `NA`

## Versioning

If the template changes:
- existing `.rds` files may need realignment
- users should download the updated template from this repository

For transparency and reproducibility, **the template distributed here is the source of truth**.
