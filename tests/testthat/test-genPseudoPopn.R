library(dplyr)

test_that("checking for simple risk calculation from pseudo popn", {
  dt11 <- wcgs %>%
    filter(!is.na(arcus)) %>%
    count(arcus, chd69) %>%
    group_by(arcus) %>%
    mutate(n_trt = sum(n)) %>%
    ungroup() %>%
    mutate(
      p = n_trt / sum(n),
      wt = 1 / p,
      ns = wt * n
    ) %>%
    group_by(arcus) %>%
    summarise(
      risk = ns[2] / sum(ns)
    )

  dt12 <- wcgs %>%
    filter(!is.na(arcus)) %>%
    gen_pseudo_popn(arcus, chd69, "No", "No") %>%
    group_by(arcus) %>%
    summarise(
      risk = calc_risk(ns)
    )

  expect_identical(dt11, dt12)

})


test_that("checking for simple risk calculation from pseudo popn", {
  dt21 <- wcgs %>%
    filter(!is.na(arcus)) %>%
    count(smoke, arcus, chd69) %>%
    group_by(smoke, arcus) %>%
    mutate(n_trt = sum(n)) %>%
    ungroup() %>%
    group_by(smoke) %>%
    mutate(
      p = n_trt / sum(n),
      wt = 1 / p,
      ns = wt * n,
      group_total = sum(n)
    ) %>%
    ungroup() %>%
    group_by(smoke, arcus) %>%
    summarise (
      risk = calc_risk(ns),
      .groups = "drop"
    )

  dt22 <- wcgs %>%
    filter(!is.na(arcus)) %>%
    gen_pseudo_popn(arcus, chd69, "No", "No", smoke) %>%
    group_by(smoke, arcus) %>%
    summarise (
      risk = calc_risk(ns), .groups = "drop"
    )

  expect_identical(dt21, dt22)

})


test_that("checking for whether reference level is given", {

  expect_error(
    wcgs %>%
      filter(!is.na(arcus)) %>%
      gen_pseudo_popn(arcus, chd69),
    "Could not determine the treatment reference level"
  )

  expect_error(
    wcgs %>%
      filter(!is.na(arcus)) %>%
      gen_pseudo_popn(arcus, chd69, y_ref_lvl = "No"),
    "Could not determine the treatment reference level"
  )

  expect_error(
    wcgs %>%
      filter(!is.na(arcus)) %>%
      gen_pseudo_popn(arcus, chd69, "No"),
    "Could not determine the y reference level"
  )
})

