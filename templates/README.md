# Metadata template (guidance)

The structure of the data entry interface is fully defined by a **metadata template (`.xlsx`)**.

## Tabs and app behaviour

- **Each sheet in the Excel metadata template becomes one tab in the app**
- Each tab represents a **separate data table**
- Records entered in different tabs are stored independently
- When results are downloaded, all tabs are stored together in **one `.rds` file**
- When a previously saved `.rds` file is uploaded, data is restored **per tab**

In short:

> **Excel sheet → App tab → One data frame in the output**

## Required columns in the template

Each sheet in the metadata template must contain the following columns:

| Column     | Required | Description |
|-----------|----------|-------------|
| `id`      | ✅ Yes | Internal variable name (used as column name in the data) |
| `type`    | ✅ Yes | Type of input control to generate |
| `label`   | ✅ Yes | Label shown to the user in the UI |
| `default` | Optional | Default value for new entries |
| `choices` | Conditional | Required for `radioButtons` |

## Column details

### `id`

- Must be **unique within a sheet**
- Must be a valid R variable name
- Used as input identifier and column name in the output

Example:

```
age
sex
country
```

### `type`

Defines which input control is generated. The app currently supports:

- `textInput` — free text input
- `numericInput` — numeric values
- `radioButtons` — predefined categorical choices

Any other value is ignored by the app.

### `label`

Text displayed to the user next to the input field.

Use clear, descriptive labels and avoid internal codes.

### `default`

Optional default value when starting a new entry.

- `textInput`: text value
- `numericInput`: numeric value
- `radioButtons`: typically left empty

If empty or missing, no default value is applied.

### `choices` (required for `radioButtons`)

Defines selectable options for radio buttons.

**Required when `type = radioButtons`; ignored otherwise.**

Format:

```
Label1=Value1; Label2=Value2; Label3=Value3
```

Example:

```
Male=1; Female=2; Other=3
```

- Labels are shown to the user
- Values are stored in the data (numeric or text)
- An explicit `Missing=None` option is automatically added by the app

## Example (simplified)

```
id        | type          | label            | default | choices
--------------------------------------------------------------
age       | numericInput  | Age              |         |
sex       | radioButtons  | Sex              |         | Male=1; Female=2
comments  | textInput     | Additional notes |         |
```

## Important notes

- id order defines input order in the UI
- The metadata template defines the **data schema**
- Incorrect metadata may result in invalid or unusable data
- Changes may break compatibility with older `.rds` files

