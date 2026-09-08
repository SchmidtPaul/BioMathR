test_that("theme_BioMath draws strip text in base_color, not in the faint box color", {
  # Since 2026-09-02: facet strip labels carry information and must meet contrast
  # requirements. The old behaviour coloured them like the panel border (#C0BCB5 on
  # white, contrast ~1.9:1), which fails WCAG AA (4.5:1).
  th <- theme_BioMath(base_color = "#001509", facette_box_color = "#C0BCB5")
  expect_equal(th$strip.text.x$colour, "#001509")
  expect_equal(th$strip.text.y$colour, "#001509")

  th_box <- theme_BioMath(base_color = "#001509", facette_box_color = "#C0BCB5", facette_box = TRUE)
  expect_equal(th_box$strip.text.x$colour, "#001509")
  expect_equal(th_box$strip.text.y$colour, "#001509")
  expect_equal(th_box$panel.border$colour, "#C0BCB5")   # the box itself keeps its colour

  th_custom <- theme_BioMath(base_color = "red")
  expect_equal(th_custom$strip.text.x$colour, "red")
})

test_that("theme_BioMath keeps its defaults: no grid, axis lines in base_color", {
  th <- theme_BioMath()
  expect_s3_class(th$panel.grid, "element_blank")
  expect_equal(th$axis.line$colour, "#001509")
})
