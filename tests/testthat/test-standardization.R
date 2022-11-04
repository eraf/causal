test_that("standardized risk ratio", {
  dt11 = wcgs %>%
    group_by(dibpat) %>%
    count(smoke, chd69) %>%
    mutate(pL = sum(n) / total) %>%
    group_by(dibpat, smoke) %>%
    summarise(pr = unique(calc_risk(n) * pL)) %>%
    group_by(smoke) %>%
    summarise(sums = sum(pr)) %>%
    summarise(st_rr = sums[2] / sums[1])
  dt12 = stdd_rr(wcgs, smoke, chd69, dibpat)
  expect_equal(dt11, dt12)
})


