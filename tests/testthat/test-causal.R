# load(here::here("tests/testthat/data/wcgs.Rdata"))
#
#
# library(dplyr)
#
# wcgs <- dplyr::as_tibble(wcgs)
#
# wcgs %>%
#   filter(!is.na(arcus)) -> wcgs
#
#
# # calculating RR ----------------------------------------------------------
#
# # manual
# a <- wcgs %>%
#   filter(!is.na(smoke)) %>%
#   count(smoke, chd69) %>%
#   group_by(smoke) %>%
#   summarise(
#     risk = n[2] / sum(n)
#   ) %>%
#   summarise(
#     RR = risk[2] / risk[1]
#   )
#
# # rr test 1
# wcgs %>%
#   filter(!is.na(arcus)) %>%
#   group_by(smoke) %>%
#   count(chd69, arcus) %>%
#   summarise(
#     RR = calc_rr(arcus, chd69, n, 0, "No")
#   )
#
# ## rr test 2
# wcgs %>%
#   filter(!is.na(smoke)) %>%
#   group_by(agec) %>%
#   count(chd69, smoke) %>%
#   summarise(
#     RR = calc_rr(smoke, chd69, n, "No", "No")
#   )
#
# ## rr test 3 (should give error)
# wcgs %>%
#   filter(!is.na(arcus)) %>%
#   group_by(agec) %>%
#   count(chd69, arcus) %>%
#   summarise(
#     RR = calc_rr(arcus, chd69, n)
#   )
#
# ## rr test 4
# wcgs %>%
#   filter(!is.na(arcus)) %>%
#   dcalc_rr(arcus, chd69, group = agec)
#
# wcgs %>%
#   filter(!is.na(arcus)) %>%
#   group_by(smoke) %>%
#   count(arcus, chd69) %>%
#   summarise(
#     RR = calc_rr(arcus, chd69, n, 0, "No")
#   )
#
# wcgs %>%
#   dcalc_rr(arcus, chd69, 0, "No", group = smoke)
#
#
# # calculating or ----------------------------------------------------------
#
#
#
# wcgs %>%
#   dcalc_or(arcus, chd69)
#
# wcgs %>%
#   filter(!is.na(arcus)) %>%
#   group_by(agec) %>%
#   count(chd69, arcus) %>%
#   summarise(
#     RR = calc_rr(arcus, chd69, n)
#   )
#
# wcgs %>%
#   filter(!is.na(arcus)) %>%
#   group_by(agec) %>%
#   count(arcus, chd69) %>%
#   summarise(
#     or = calc_or(arcus, chd69, n),
#     rr = calc_rr(arcus, chd69, n)
#   )
#
#
# # creating the pseudo population ------------------------------------------
#
# # IPW
#
# wcgs %>%
#   filter(!is.na(arcus)) %>%
#   count(arcus, chd69) %>%
#   group_by(arcus) %>%
#   mutate(n_trt = sum(n)) %>%
#   ungroup() %>%
#   mutate(
#     p = n_trt / sum(n),
#     wt = 1 / p,
#     ns = wt * n
#   ) %>%
#   group_by(arcus) %>%
#   summarise(
#     risk = calc_risk(ns)
#   )
#
#
#
# wcgs %>%
#   gen_pseudo_popn(arcus, chd69, smoke)
#
# # stratum specific
#
# wcgs %>%
#   count(smoke, arcus, chd69) %>%
#   group_by(smoke, arcus) %>%
#   mutate(n_trt = sum(n)) %>%
#   ungroup() %>%
#   group_by(smoke) %>%
#   mutate(
#     p = n_trt / sum(n),
#     wt = 1 / p,
#     ns = wt * n,
#     group_total = sum(n)
#   ) %>%
#   ungroup() %>%
#   group_by(smoke, arcus) %>%
#   mutate(
#     risk = calc_risk(ns)
#   )
