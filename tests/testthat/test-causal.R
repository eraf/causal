load(here::here("tests/testthat/data/wcgs.Rdata"))


wcgs <- dplyr::as_tibble(wcgs)

wcgs %>% dcalc_rr(arcus, chd69, agec)

wcgs %>%
  filter(!is.na(arcus)) %>%
  group_by(agec) %>%
  count(chd69, arcus) %>%
  summarise(
    RR = calc_rr(arcus, chd69, n)
  )


wcgs %>%
  dcalc_or(arcus, chd69, group = agec)

wcgs %>%
  dcalc_or(arcus, chd69)

wcgs %>%
  filter(!is.na(arcus)) %>%
  group_by(agec) %>%
  count(chd69, arcus) %>%
  summarise(
    RR = calc_rr(arcus, chd69, n)
  )

wcgs %>%
  filter(!is.na(arcus)) %>%
  group_by(agec) %>%
  count(arcus, chd69) %>%
  summarise(
    or = calc_or(arcus, chd69, n),
    rr = calc_rr(arcus, chd69, n)
  )


# creating the pseudo population ------------------------------------------

# IPW

wcgs %>%
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
    causal_eff = ns[2] / sum(ns)
  )


# stratum specific

wcgs %>%
  filter(!is.na(arcus)) %>%
  # group_by(smoke) %>%
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
  mutate(
    risk = ns[2] / sum(ns)
  ) %>%
  filter(chd69 == "Yes")

