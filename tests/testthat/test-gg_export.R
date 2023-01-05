library(ggplot2)

p <- ggplot(data = PlantGrowth) +
  aes(y = weight, x = group) +
  geom_point()


# Export nothing ----------------------------------------------------------
gg_export(
  plot_obj = p,
  file_name = "t1",
  folder_path = here::here(),
  width_cm = 12,
  height_cm = 8,
  png_dpi = c(96, 300),
  png = "none",
  pdf = "none",
  svg = "none"
)

test_that("Nothing was exported", {
  expect_equal(length(list.files(pattern = "t1.png|t1.pdf|t1.svg")), 0)
})

# TODO: Temporarily create files for tests

# # Export 4 plots ----------------------------------------------------------
# gg_export(
#   plot_obj = p,
#   file_name = "t2",
#   folder_path = here::here(),
#   width_cm = 12,
#   height_cm = 8,
#   png_dpi = c(96, 300),
#   png = "create",
#   pdf = "create",
#   svg = "create"
# )
#
# test_that("All test files created", {
#   expect_equal(length(list.files(pattern = "t2_96dpi.png|t2_300dpi.png")), 2)
#   expect_equal(length(list.files(pattern = "t2.pdf")), 1)
#   expect_equal(length(list.files(pattern = "t2.svg")), 1)
# })
#
# test_that("All files > 8 KB", {
#   expect_true(all(file.size(list.files(pattern = "t2_96dpi.png|t2_300dpi.png|t2.pdf|t2.svg")) > 8000))
# })
#
#
# # Export via png_from_pdf -------------------------------------------------
# gg_export(
#   plot_obj = p,
#   file_name = "t3",
#   folder_path = here::here(),
#   width_cm = 12,
#   height_cm = 8,
#   png_dpi = 96,
#   png = "create",
#   pdf = "none",
#   svg = "none",
#   png_from_pdf = TRUE
# )
#
# test_that("Two pngs but no pdf/svg created", {
#   expect_equal(length(list.files(pattern = "t3.png")), 1)
#   expect_equal(length(list.files(pattern = "t3.pdf")), 0)
#   expect_equal(length(list.files(pattern = "t3.svg")), 0)
# })
#
#
# # Export via svg_device ---------------------------------------------------
# gg_export(
#   plot_obj = p,
#   file_name = "t4",
#   folder_path = here::here(),
#   width_cm = 12,
#   height_cm = 8,
#   png_dpi = 96,
#   png = "none",
#   pdf = "none",
#   svg = "create",
#   svg_device = "svg"
# )
#
# test_that("Only one svg created", {
#   expect_equal(length(list.files(pattern = "t4.png")), 0)
#   expect_equal(length(list.files(pattern = "t4.pdf")), 0)
#   expect_equal(length(list.files(pattern = "t4.svg")), 1)
# })
#
#
# # remove all test files ---------------------------------------------------
# Sys.sleep(0.5)
# file.remove(list.files(pattern = ".png|.pdf|.svg"))
#
# test_that("All test files deleted again", {
#   expect_equal(length(list.files(pattern = ".png|.pdf|.svg")), 0)
# })
#
