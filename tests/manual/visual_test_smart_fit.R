# Visual Test for smart_fit()
# =============================
# This script creates a Word document with 10 test tables to visually inspect
# how smart_fit() handles different edge cases.
#
# Usage: source("tests/manual/visual_test_smart_fit.R")
#
# The script will:
# 1. Create 10 different test tables
# 2. Apply smart_fit() with verbose output
# 3. Create a Word document with one page per table
# 4. Each page shows: table description, the fitted table, and verbose output
# 5. Automatically open the document for inspection

library(flextable)
library(officer)
library(dplyr)
library(BioMathR)

cat("\n")
cat("=========================================\n")
cat("  smart_fit() Visual Test Generator\n")
cat("=========================================\n\n")

# Create output document
doc <- officer::read_docx()
output_file <- "tests/manual/smart_fit_visual_test.docx"

# Helper function to capture verbose output
capture_verbose <- function(expr) {
  output <- utils::capture.output({
    result <- expr
  })
  list(result = result, output = paste(output, collapse = "\n"))
}

# Helper function to add a test table to the document
add_test_table <- function(doc, test_num, description, df, width_param = "A4",
                          landscape = FALSE, custom_width = NULL,
                          prev_landscape = FALSE) {

  cat(sprintf("  ✓ Table %d: %s\n", test_num, description))

  # Add section break with orientation based on PREVIOUS test
  if (test_num > 1) {
    if (prev_landscape) {
      doc <- doc %>%
        officer::body_end_section_landscape()
    } else {
      doc <- doc %>%
        officer::body_end_section_portrait()
    }
  }

  # Add title
  doc <- doc %>%
    officer::body_add_par(sprintf("Test %d: %s", test_num, description),
                         style = "heading 1") %>%
    officer::body_add_par(" ")

  # Create flextable (with docx_tab for tests 11-20)
  if (test_num >= 11) {
    ft <- df %>% BioMathR::docx_tab()
  } else {
    ft <- flextable::flextable(df)
  }

  # Apply docx_tab() for tests 11-20
  if (test_num >= 11) {
    ft <- ft %>% BioMathR::docx_tab()
  }

  # Capture verbose output and apply smart_fit
  if (!is.null(custom_width)) {
    captured <- capture_verbose({
      ft_fitted <- ft %>% smart_fit(width = custom_width, verbose = TRUE)
    })
  } else {
    captured <- capture_verbose({
      ft_fitted <- ft %>% smart_fit(width = width_param,
                                    landscape = landscape,
                                    verbose = TRUE)
    })
  }

  # Add the fitted table
  doc <- doc %>%
    flextable::body_add_flextable(captured$result) %>%
    officer::body_add_par(" ") %>%
    officer::body_add_par("Verbose Output:", style = "heading 2") %>%
    officer::body_add_par(" ")

  # Add verbose output as code block
  verbose_lines <- strsplit(captured$output, "\n")[[1]]
  for (line in verbose_lines) {
    doc <- doc %>%
      officer::body_add_par(line, style = "Normal")
  }

  return(doc)
}

cat("Creating 20 test tables (10 standard + 10 with docx_tab)...\n")

# ==============================================================================
# PORTRAIT TESTS (A4 Portrait)
# ==============================================================================

# Test 1: Single column, short name
test1_df <- data.frame(
  ID = 1:5
)
doc <- add_test_table(doc, 1, "1 column, short name", test1_df,
                     prev_landscape = FALSE)

# Test 2: Single column, very long name
test2_df <- data.frame(
  `This is an extremely long column name that should definitely wrap` = 1:5
)
names(test2_df) <- "This is an extremely long column name that should definitely wrap"
doc <- add_test_table(doc, 2, "1 column, very long name", test2_df,
                     prev_landscape = FALSE)

# Test 3: Three columns, all short names
test3_df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15
)
doc <- add_test_table(doc, 3, "3 columns, all short names", test3_df,
                     prev_landscape = FALSE)

# Test 4: Six columns, all short names
test4_df <- data.frame(
  A = 1:5,
  B = 2:6,
  C = 3:7,
  D = 4:8,
  E = 5:9,
  F = 6:10
)
doc <- add_test_table(doc, 4, "6 columns, all short names (2-3 chars)", test4_df,
                     prev_landscape = FALSE)

# Test 5: Six columns, all long names
test5_df <- data.frame(
  `Very Long Column Name One` = 1:5,
  `Another Extremely Long Name` = 2:6,
  `Third Long Column Header` = 3:7,
  `Fourth Extended Name Here` = 4:8,
  `Fifth Long Name Column` = 5:9,
  `Sixth And Final Long Name` = 6:10
)
names(test5_df) <- c("Very Long Column Name One", "Another Extremely Long Name",
                     "Third Long Column Header", "Fourth Extended Name Here",
                     "Fifth Long Name Column", "Sixth And Final Long Name")
doc <- add_test_table(doc, 5, "6 columns, all long names (20+ chars)", test5_df,
                     prev_landscape = FALSE)

# Test 6: Six columns, mixed name lengths
test6_df <- data.frame(
  ID = 1:5,
  `Very Long Name` = letters[1:5],
  Nm = 1:5,
  `Medium Length Name` = 1:5,
  X = letters[6:10],
  `Another Long Column Name Here` = 1:5
)
names(test6_df) <- c("ID", "Very Long Name", "Nm", "Medium Length Name",
                     "X", "Another Long Column Name Here")
doc <- add_test_table(doc, 6, "6 columns, mixed name lengths", test6_df,
                     prev_landscape = FALSE)

# Test 7: Six columns, one very long cell value
test7_df <- data.frame(
  ID = 1:5,
  Name = c("Short", "Medium length", "antidisestablishmentarianism", "Normal", "Text"),
  Value = 10:14,
  Category = c("A", "B", "C", "D", "E"),
  Status = c("OK", "OK", "OK", "OK", "OK"),
  Notes = c("Fine", "Good", "Excellent performance here", "OK", "Great")
)
doc <- add_test_table(doc, 7, "6 columns, one VERY long cell value", test7_df,
                     prev_landscape = FALSE)

# ==============================================================================
# LANDSCAPE TESTS (A4 Landscape)
# ==============================================================================

# Test 8: Ten columns, short names (landscape)
test8_df <- data.frame(matrix(1:50, nrow = 5, ncol = 10))
names(test8_df) <- paste0("C", 1:10)
doc <- add_test_table(doc, 8, "10 columns, short names (landscape)", test8_df,
                     landscape = TRUE, prev_landscape = FALSE)

# Test 9: Five columns, constrained custom width (landscape)
test9_df <- data.frame(
  Product = c("Widget A", "Widget B", "Widget C", "Widget D", "Widget E"),
  Price = c(19.99, 29.99, 39.99, 49.99, 59.99),
  Stock = c(100, 250, 50, 75, 125),
  Supplier = c("ABC Corp", "XYZ Ltd", "ABC Corp", "DEF Inc", "XYZ Ltd"),
  Rating = c(4.5, 4.8, 4.2, 4.7, 4.6)
)
doc <- add_test_table(doc, 9, "5 columns, custom width = 15 cm (landscape)",
                     test9_df, landscape = TRUE, custom_width = 15,
                     prev_landscape = TRUE)

# Test 10: Four columns, content already too wide (forcing auto-scale)
test10_df <- data.frame(
  `First Very Long Column Name That Takes Up Space` =
    c("Some extremely long content here", "Short", "Medium length text",
      "Another very long piece of text that should wrap", "Brief"),
  `Second Incredibly Long Column Header` =
    c("More long content", "Text", "Data values here", "Information", "Content"),
  `Third Extended Column Name` =
    c("Lorem ipsum dolor sit amet", "Short", "Text", "More data", "Values"),
  `Fourth And Final Long Name` =
    c("Additional information here", "Brief", "Data", "Text values", "Info")
)
names(test10_df) <- c("First Very Long Column Name That Takes Up Space",
                      "Second Incredibly Long Column Header",
                      "Third Extended Column Name",
                      "Fourth And Final Long Name")
doc <- add_test_table(doc, 10,
                     "4 columns, very wide content forcing auto-scale (landscape)",
                     test10_df, landscape = TRUE, prev_landscape = TRUE)

# ==============================================================================
# TESTS WITH docx_tab() - PORTRAIT
# ==============================================================================

# Test 11: Same as Test 1, but with docx_tab()
test11_df <- data.frame(
  ID = 1:5
)
doc <- add_test_table(doc, 11, "1 column, short name (with docx_tab)", test11_df,
                     prev_landscape = TRUE)

# Test 12: Same as Test 2, but with docx_tab()
test12_df <- data.frame(
  `This is an extremely long column name that should definitely wrap` = 1:5
)
names(test12_df) <- "This is an extremely long column name that should definitely wrap"
doc <- add_test_table(doc, 12, "1 column, very long name (with docx_tab)", test12_df,
                     prev_landscape = FALSE)

# Test 13: Same as Test 3, but with docx_tab()
test13_df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15
)
doc <- add_test_table(doc, 13, "3 columns, all short names (with docx_tab)", test13_df,
                     prev_landscape = FALSE)

# Test 14: Same as Test 4, but with docx_tab()
test14_df <- data.frame(
  A = 1:5,
  B = 2:6,
  C = 3:7,
  D = 4:8,
  E = 5:9,
  F = 6:10
)
doc <- add_test_table(doc, 14, "6 columns, all short names (2-3 chars) (with docx_tab)", test14_df,
                     prev_landscape = FALSE)

# Test 15: Same as Test 5, but with docx_tab()
test15_df <- data.frame(
  `Very Long Column Name One` = 1:5,
  `Another Extremely Long Name` = 2:6,
  `Third Long Column Header` = 3:7,
  `Fourth Extended Name Here` = 4:8,
  `Fifth Long Name Column` = 5:9,
  `Sixth And Final Long Name` = 6:10
)
names(test15_df) <- c("Very Long Column Name One", "Another Extremely Long Name",
                     "Third Long Column Header", "Fourth Extended Name Here",
                     "Fifth Long Name Column", "Sixth And Final Long Name")
doc <- add_test_table(doc, 15, "6 columns, all long names (20+ chars) (with docx_tab)", test15_df,
                     prev_landscape = FALSE)

# Test 16: Same as Test 6, but with docx_tab()
test16_df <- data.frame(
  ID = 1:5,
  `Very Long Name` = letters[1:5],
  Nm = 1:5,
  `Medium Length Name` = 1:5,
  X = letters[6:10],
  `Another Long Column Name Here` = 1:5
)
names(test16_df) <- c("ID", "Very Long Name", "Nm", "Medium Length Name",
                     "X", "Another Long Column Name Here")
doc <- add_test_table(doc, 16, "6 columns, mixed name lengths (with docx_tab)", test16_df,
                     prev_landscape = FALSE)

# Test 17: Same as Test 7, but with docx_tab()
test17_df <- data.frame(
  ID = 1:5,
  Name = c("Short", "Medium length", "antidisestablishmentarianism", "Normal", "Text"),
  Value = 10:14,
  Category = c("A", "B", "C", "D", "E"),
  Status = c("OK", "OK", "OK", "OK", "OK"),
  Notes = c("Fine", "Good", "Excellent performance here", "OK", "Great")
)
doc <- add_test_table(doc, 17, "6 columns, one VERY long cell value (with docx_tab)", test17_df,
                     prev_landscape = FALSE)

# ==============================================================================
# TESTS WITH docx_tab() - LANDSCAPE
# ==============================================================================

# Test 18: Same as Test 8, but with docx_tab()
test18_df <- data.frame(matrix(1:50, nrow = 5, ncol = 10))
names(test18_df) <- paste0("C", 1:10)
doc <- add_test_table(doc, 18, "10 columns, short names (landscape) (with docx_tab)", test18_df,
                     landscape = TRUE, prev_landscape = FALSE)

# Test 19: Same as Test 9, but with docx_tab()
test19_df <- data.frame(
  Product = c("Widget A", "Widget B", "Widget C", "Widget D", "Widget E"),
  Price = c(19.99, 29.99, 39.99, 49.99, 59.99),
  Stock = c(100, 250, 50, 75, 125),
  Supplier = c("ABC Corp", "XYZ Ltd", "ABC Corp", "DEF Inc", "XYZ Ltd"),
  Rating = c(4.5, 4.8, 4.2, 4.7, 4.6)
)
doc <- add_test_table(doc, 19, "5 columns, custom width = 15 cm (landscape) (with docx_tab)",
                     test19_df, landscape = TRUE, custom_width = 15,
                     prev_landscape = TRUE)

# Test 20: Same as Test 10, but with docx_tab()
test20_df <- data.frame(
  `First Very Long Column Name That Takes Up Space` =
    c("Some extremely long content here", "Short", "Medium length text",
      "Another very long piece of text that should wrap", "Brief"),
  `Second Incredibly Long Column Header` =
    c("More long content", "Text", "Data values here", "Information", "Content"),
  `Third Extended Column Name` =
    c("Lorem ipsum dolor sit amet", "Short", "Text", "More data", "Values"),
  `Fourth And Final Long Name` =
    c("Additional information here", "Brief", "Data", "Text values", "Info")
)
names(test20_df) <- c("First Very Long Column Name That Takes Up Space",
                      "Second Incredibly Long Column Header",
                      "Third Extended Column Name",
                      "Fourth And Final Long Name")
doc <- add_test_table(doc, 20,
                     "4 columns, very wide content forcing auto-scale (landscape) (with docx_tab)",
                     test20_df, landscape = TRUE, prev_landscape = TRUE)

# End the final section as landscape
doc <- doc %>%
  officer::body_end_section_landscape()

# ==============================================================================
# Save and open document
# ==============================================================================

cat("\nSaving document...\n")
print(doc, target = output_file)

cat(sprintf("✓ Document created: %s\n\n", output_file))
cat("Opening document for inspection...\n")

# Open the document
BioMathR::open_file(output_file)

cat("\n")
cat("=========================================\n")
cat("  Visual inspection complete!\n")
cat("  Review the Word document and check:\n")
cat("  - All tables fit within page margins\n")
cat("  - No unnecessary line breaks\n")
cat("  - Column widths look balanced\n")
cat("  - Verbose output shows correct logic\n")
cat("  - Tests 1-10: Standard flextables\n")
cat("  - Tests 11-20: With docx_tab() styling\n")
cat("=========================================\n\n")
