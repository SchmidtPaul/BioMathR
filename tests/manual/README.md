# Manual Tests for BioMathR

This directory contains manual/visual tests that are **not** run automatically during `devtools::test()` or `R CMD check`.

## Purpose

Some functions (like `smart_fit()`) require **visual inspection** in actual output documents (Word, Excel, etc.) to verify they work correctly. These tests generate output files that you must review manually.

## Available Tests

### `visual_test_smart_fit.R`

Tests `smart_fit()` and `docx_tab()` by creating a Word document with 20 different table layouts (10 standard flextables + 10 with `docx_tab()` styling).

**How to run:**
```r
# From R console:
source("tests/manual/visual_test_smart_fit.R")

# Or from terminal:
Rscript tests/manual/visual_test_smart_fit.R
```

**What it does:**
1. Creates 20 test tables (10 standard + 10 with `docx_tab()`)
2. Applies `smart_fit()` with verbose output
3. Generates `tests/manual/smart_fit_visual_test.docx`
4. Automatically opens the document

**What to check:**
- All tables fit within page margins (no overflow)
- No unnecessary line breaks in column names or cells
- Column widths appear balanced and appropriate
- Verbose output shows correct logic for each step
- Tests 1-7: portrait, Tests 8-10: landscape
- Tests 11-20: same layouts with `docx_tab()` styling

**Test cases covered (Tests 1-10 standard, 11-20 with docx_tab):**
1. Single column with short name
2. Single column with very long name
3. Three columns with short names
4. Six columns with short names
5. Six columns with long names
6. Six columns with mixed name lengths
7. Six columns with very long cell content
8. Ten columns (landscape)
9. Five columns with constrained width (landscape)
10. Four columns forcing auto-scale (landscape)

## Output Files

All output files are generated in this directory and are **git-ignored** (see `.gitignore`).

## Notes

- These tests are excluded from the R package build (see `.Rbuildignore`)
- Run these tests whenever you modify functions that produce visual output
- Keep this directory clean - delete old test output files periodically
