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
                          landscape = FALSE, custom_width = NULL) {

  cat(sprintf("  ✓ Table %d: %s\n", test_num, description))

  # Change page orientation if needed
  if (landscape) {
    doc <- doc %>%
      officer::body_end_section_landscape()
  } else if (test_num > 1) {
    # Add page break before new table (except first)
    doc <- doc %>%
      officer::body_add_break()
  }

  # Add title
  doc <- doc %>%
    officer::body_add_par(sprintf("Test %d: %s", test_num, description),
                         style = "heading 1") %>%
    officer::body_add_par(" ")

  # Create flextable
  ft <- flextable::flextable(df)

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
    officer::body_add_flextable(captured$result) %>%
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

cat("Creating 10 test tables...\n")

# ==============================================================================
# PORTRAIT TESTS (A4 Portrait)
# ==============================================================================

# Test 1: Single column, short name
test1_df <- data.frame(
  ID = 1:5
)
doc <- add_test_table(doc, 1, "1 column, short name", test1_df)

# Test 2: Single column, very long name
test2_df <- data.frame(
  `This is an extremely long column name that should definitely wrap` = 1:5
)
names(test2_df) <- "This is an extremely long column name that should definitely wrap"
doc <- add_test_table(doc, 2, "1 column, very long name", test2_df)

# Test 3: Three columns, all short names
test3_df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15
)
doc <- add_test_table(doc, 3, "3 columns, all short names", test3_df)

# Test 4: Six columns, all short names
test4_df <- data.frame(
  A = 1:5,
  B = 2:6,
  C = 3:7,
  D = 4:8,
  E = 5:9,
  F = 6:10
)
doc <- add_test_table(doc, 4, "6 columns, all short names (2-3 chars)", test4_df)

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
doc <- add_test_table(doc, 5, "6 columns, all long names (20+ chars)", test5_df)

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
doc <- add_test_table(doc, 6, "6 columns, mixed name lengths", test6_df)

# Test 7: Six columns, one very long cell value
test7_df <- data.frame(
  ID = 1:5,
  Name = c("Short", "Medium length", "antidisestablishmentarianism", "Normal", "Text"),
  Value = 10:14,
  Category = c("A", "B", "C", "D", "E"),
  Status = c("OK", "OK", "OK", "OK", "OK"),
  Notes = c("Fine", "Good", "Excellent performance here", "OK", "Great")
)
doc <- add_test_table(doc, 7, "6 columns, one VERY long cell value", test7_df)

# ==============================================================================
# LANDSCAPE TESTS (A4 Landscape)
# ==============================================================================

# Test 8: Ten columns, short names (landscape)
test8_df <- data.frame(matrix(1:50, nrow = 5, ncol = 10))
names(test8_df) <- paste0("C", 1:10)
doc <- add_test_table(doc, 8, "10 columns, short names (landscape)", test8_df,
                     landscape = TRUE)

# Test 9: Five columns, constrained custom width (landscape)
test9_df <- data.frame(
  Product = c("Widget A", "Widget B", "Widget C", "Widget D", "Widget E"),
  Price = c(19.99, 29.99, 39.99, 49.99, 59.99),
  Stock = c(100, 250, 50, 75, 125),
  Supplier = c("ABC Corp", "XYZ Ltd", "ABC Corp", "DEF Inc", "XYZ Ltd"),
  Rating = c(4.5, 4.8, 4.2, 4.7, 4.6)
)
doc <- add_test_table(doc, 9, "5 columns, custom width = 15 cm (landscape)",
                     test9_df, landscape = TRUE, custom_width = 15)

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
                     test10_df, landscape = TRUE)

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
cat("=========================================\n\n")
