library(dplyr)

wcgs <- wcgs %>% filter(!is.na(arcus))

test_that("Checking for simple single rr calculation", {
  # manual
  dt11 <- wcgs %>%
    count(smoke, chd69) %>%
    group_by(smoke) %>%
    summarise(
      risk = n[2] / sum(n)
    ) %>%
    summarise(
      rr = risk[2] / risk[1]
    )

  dt12 <- wcgs %>%
    count(smoke, chd69) %>%
    summarise (
      rr = calc_rr(smoke, chd69, n, "No", "No")
    )

  dt13 <- wcgs %>%
    dcalc_rr(smoke, chd69, "No", "No")

  expect_identical(dt11, dt12)
  expect_identical(dt11, dt13)
  expect_identical(dt12, dt13)
})


test_that("Checking for grouped rr calculation", {
  # manual
  dt21 <- wcgs %>%
    group_by(arcus) %>%
    count(chd69, smoke) %>%
    summarise(
      rr = calc_rr(smoke, chd69, n, "No", "No")
    )

  dt22 <- wcgs %>%
    dcalc_rr(smoke, chd69, "No", "No", group = arcus)

  expect_identical(dt21, dt22)
})


test_that("Checking for calculation if ref level changed (single value)", {
  # manual
  dt31 <- wcgs %>%
    count(smoke, chd69) %>%
    mutate(
      smoke = if_else(smoke == "No", "Yes", "No"),
      chd69 = if_else(chd69 == "No", "Yes", "No")
    ) %>%
    group_by(smoke) %>%
    summarise(
      risk = n[2] / sum(n)
    ) %>%
    summarise(
      rr = risk[2] / risk[1]
    )

  dt32 <- wcgs %>%
    count(smoke, chd69) %>%
    summarise (
      rr = calc_rr(smoke, chd69, n, "Yes", "Yes")
    )

  dt33 <- wcgs %>%
    dcalc_rr(smoke, chd69, "Yes", "Yes")

  expect_identical(dt31, dt32)
  expect_identical(dt31, dt33)
  expect_identical(dt32, dt33)
})


test_that("Checking for calculation if ref level changed (grouped value)", {
  # manual
  dt41 <- wcgs %>%
    group_by(arcus) %>%
    count(chd69, smoke) %>%
    summarise(
      rr = calc_rr(smoke, chd69, n, "Yes", "Yes")
    )

  dt42 <- wcgs %>%
    dcalc_rr(smoke, chd69, "Yes", "Yes", group = arcus)

  expect_identical(dt41, dt42)
})


test_that("Expecting errors in calc_rr", {
  expect_error(
    wcgs %>%
      count(smoke, chd69) %>%
      summarise (
        rr = calc_rr(smoke, chd69, n)
      ),
    "Could not determine the treatment reference level"
  )

  expect_error(
    wcgs %>%
      count(smoke, chd69) %>%
      summarise (
        rr = calc_rr(smoke, chd69, n, y_ref_lvl = "No")
      ),
    "Could not determine the treatment reference level"
  )

  expect_error(
    wcgs %>%
      count(smoke, chd69) %>%
      summarise (
        rr = calc_rr(smoke, chd69, n, "No")
      ),
    "Could not determine the y reference level"
  )
})


test_that("Expecting errors in dcalc_rr", {
  expect_error(
    wcgs %>%
      dcalc_rr(smoke, chd69),
    "Could not determine the treatment reference level"
  )

  expect_error(
    wcgs %>%
      dcalc_rr(smoke, chd69, y_ref_lvl = "No"),
    "Could not determine the treatment reference level"
  )

  expect_error(
    wcgs %>%
      dcalc_rr(smoke, chd69, "No"),
    "Could not determine the y reference level"
  )
})

test_that("Expecting errors in calc_rr", {
  expect_error(
    wcgs %>%
      count(chol, chd69) %>%
      summarise (
        rr = calc_rr(chol, chd69, n, "No")
      )
  )
})
