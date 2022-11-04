library(dplyr)

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
    filter(!is.na(arcus)) %>%
    group_by(arcus) %>%
    count(chd69, smoke) %>%
    summarise(
      rr = calc_rr(smoke, chd69, n, "No", "No")
    )

  dt22 <- wcgs %>%
    filter(!is.na(arcus)) %>%
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
    filter(!is.na(arcus)) %>%
    group_by(arcus) %>%
    count(chd69, smoke) %>%
    summarise(
      rr = calc_rr(smoke, chd69, n, "Yes", "Yes")
    )

  dt42 <- wcgs %>%
    filter(!is.na(arcus)) %>%
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

test_that("Expecting errors in rr functions for treatment not being binary", {
  expect_error(
    wcgs %>%
      count(agec, chd69) %>%
      summarise (
        rr = calc_rr(agec, chd69, n, "No", "No")
      )
  )

  expect_error(
    wcgs %>%
      dcalc_rr(agec, chd69, "No", "No")
  )

})

test_that("Expecting errors in rr functions for outcome not being binary", {
  expect_error(
    wcgs %>%
      count(agec, smoke) %>%
      summarise (
        rr = calc_rr(smoke, agec, n, "No", "No")
        # This doesn't make sense much IKR,
        # but we need to test anyway
      )
  )

  expect_error(
    wcgs %>%
      dcalc_rr(agec, smoke, "No", "No")
    # Yeah again, This doesn't make sense much IKR,
    # but we need to test anyway
  )
})

test_that("Expecting errors in calc_rr for NA values in treatment `chol`", {
  expect_error(
    wcgs %>%
      count(chol, chd69) %>%
      summarise (
        rr = calc_rr(chol, chd69, n, "No", "No")
      ),
    "There are missing values in treatment"
    )

  expect_error(
    wcgs %>%
      dcalc_rr(chol, chd69, "No", "No"),
    "There are missing values in treatment"
    )
  }
)


test_that("Expecting errors in calc_rr for NA values in outcome `chol`", {
  expect_error(
    wcgs %>%
      count(chol, smoke) %>%
      summarise (rr = calc_rr(smoke, chol, n, "No", "No"))
    )

  expect_error(
    wcgs %>%
      dcalc_rr(smoke, chol, "No", "No")
    )
  }
)

test_that("Expecting erros in dcalc_rr for NA values in group variable `arcus`", {

  expect_error(
    wcgs %>%
      dcalc_rr(smoke, chd69, "No", "No", arcus),
    "There are missing values in group variable"
    )
  }
)
