# BioMathR 0.9.0

## Excel toolkit moved from {openxlsx} to {openxlsx2}

`create_wb()`, `add_sheet()`, `cond_format()` and `save_wb()` (and thus also
`desc_tabs()` and the Excel export of `get_teams_attendance()`) are now built on
[{openxlsx2}](https://janmarvin.github.io/openxlsx2/), the actively developed
successor of {openxlsx}. {openxlsx} is no longer a dependency. BioMathR now
requires {openxlsx2} >= 1.28 and R >= 4.1.0.

Workbooks are still modified in place, so existing code keeps working:

``` r
wb <- create_wb()
add_sheet(wb, mtcars, "cars")
save_wb(wb, "out.xlsx", overwrite = TRUE)
```

The workbook is an {openxlsx2} `wbWorkbook` object, so `openxlsx::*()` functions
(e.g. `openxlsx::addWorksheet()`, `openxlsx::saveWorkbook()`) can no longer be
applied to it. Use the `openxlsx2::wb_*()` equivalents instead.

## Renamed arguments

Arguments were renamed to snake_case. The old names still work but warn:

* `create_wb()`: `fontSize` -> `font_size`, `fontName` -> `font_name`,
  `infosheetlabel` -> `infosheet_label`
* `add_sheet()`: `sheetName` -> `sheet_name`, `colWidth` -> `col_width`,
  `colWidthMin` -> `col_width_min`, `colWidthMax` -> `col_width_max`,
  `gridLines` -> `grid_lines`, `freezefirstRow` -> `freeze_first_row`,
  `freezefirstCol` -> `freeze_first_col`, `addFilters` -> `add_filters`,
  `rowheight` -> `row_height`, `textwrap` -> `text_wrap`,
  `dateformat` -> `date_format`, `datetimeformat` -> `datetime_format`
* `cond_format()`: `sheetName` -> `sheet`, `colourScale` -> `colour_scale`

## Breaking changes

* `cond_format()` no longer accepts an `openxlsx::createStyle()` object in
  `style` - {openxlsx2} needs a registered dxf style instead. Use the new
  arguments `font_colour` and `bg_fill` (defaults are unchanged: white on
  `#ad0000`), or register a style via `openxlsx2::wb_add_dxfs_style()` and pass
  its name. Passing a `createStyle()` object now raises an informative error.
  For `type = "colorScale"`, `style` still takes a vector of 2-3 colours.
* `save_wb()` no longer has a `...` argument, because `openxlsx2::wb_save()`
  takes no further arguments.

## Other changes

* `add_sheet()` now returns the workbook invisibly, so `wb <- add_sheet(wb, x)`
  and pipe chains work as documented.
* `add_sheet()` errors when a sheet of that name (case-insensitive) already
  exists in the workbook. {openxlsx2} would otherwise silently create
  `"name (1)"` and the data would end up in the *existing* sheet. If
  {openxlsx2} cleans the name (illegal characters, more than 31 characters),
  `add_sheet()` uses the cleaned name.
* `add_sheet()` now actually applies `col_width`; the previous version
  documented the argument but always used `"auto"`. Numeric widths are clamped
  to `col_width_min`/`col_width_max` as well.
* `add_sheet()` gained `na`, defaulting to `NULL`, which leaves cells of `NA`
  values empty. Borders are still drawn around those cells.
* `add_sheet()` no longer errors on a zero-row data frame.
* `add_sheet()` formats all `POSIXt` columns with `datetime_format`, also those
  without a `tzone` attribute and those containing `NA`.
* `cond_format()` accepts the type names of {openxlsx} (e.g. `"colourScale"`,
  `"databar"`, `"contains"`, `"notContains"`), matches them case-insensitively
  and translates them. `"dataBar"` and `"iconSet"` rules no longer get a dxf
  style attached.
* `cond_format()` issues one call per contiguous block of rows and columns.
  This works around {openxlsx2} 1.28 writing the colours of a `"colorScale"`
  as numeric thresholds into every block after the first when a single call
  spans non-consecutive cells.
* `cond_format()` errors when `columns` are not found in the sheet or when
  numeric indices are outside `1:ncol` (instead of formatting nothing or cells
  beyond the data), and does nothing on a sheet without data rows instead of
  putting the rule on the header row.
* `save_wb()` returns the file path invisibly and only switches the active sheet
  when there is a sheet after `"info"`. That sheet is now also the *selected*
  one, so Excel no longer opens the file with two grouped sheet tabs.
* The info sheet of `create_wb()` now sets the column widths of the columns it
  actually writes into.

## ggplot

* `theme_BioMath()` draws facet strip labels in `base_color` instead of
  `facette_box_color`. The old colour (`#C0BCB5` on white) failed the WCAG AA
  contrast requirement for text.
