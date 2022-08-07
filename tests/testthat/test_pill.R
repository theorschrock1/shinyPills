test_that("pill", {
  local_edition(3)
  expect_snapshot(pill(id = "id", label = "ds", type = "none", data_icon = NULL, class = NULL), cran = TRUE)
})
