library(tidyverse)
library(frasyr)
library(viridis)
library(patchwork)
source("./R/script/calc_brp.R")

devtools::load_all()

# Conduct VPA with M 0.01 scale from 0 to 1.5

# Define M
mlist <- seq(0, 1.5, 0.01)

# Run VPA for each M
raw_res_fine <- run_vpa_mlist(res_vpa_example, mlist)

usethis::use_data(raw_res_fine, overwrite = TRUE)

# Plot the sensitivity analysis
g_b <- raw_res_fine %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "biomass")
g_s <- raw_res_fine %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "ssb")
g_f <- raw_res_fine %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "meanF")
g_n <- raw_res_fine %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "number")

g_b + g_s + g_f + g_n + plot_layout(ncol = 2)

# Extract the terminal year data
last_year_res <-
  raw_res_fine %>% spread_result() %>% tidy_result() %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::filter(Year == max(Year))%>%
  dplyr::group_by(M, Year) %>%
  dplyr::summarise(biomass = sum(baa),
                   ssb = sum(ssb),
                   meanF = mean(faa),
                   number = sum(naa)) %>%
  dplyr::mutate(density = density_m(M, amax = 5)) %>%
  dplyr::filter(M <= 2)

g_b_ly <-
  last_year_res %>%
  ggplot() +
  geom_line(aes(biomass, density)) +
  geom_point(aes(biomass, density, color = M, group = M)) +
  scale_x_log10() +
  scale_color_viridis(option = "H", discrete = FALSE)


g_s_ly <-
  last_year_res %>%
  ggplot() +
  geom_line(aes(ssb, density))  +
  geom_point(aes(ssb, density, color = M, group = M)) +
  scale_x_log10() +
  scale_color_viridis(option = "H", discrete = FALSE)

g_f_ly <-
  last_year_res %>%
  ggplot() +
  geom_line(aes(meanF, density)) +
  geom_point(aes(meanF, density, color = M, group = M)) +
  scale_color_viridis(option = "H", discrete = FALSE)

g_n_ly <-
  last_year_res %>%
  ggplot() +
  geom_line(aes(number, density))  +
  geom_point(aes(number, density, color = M, group = M)) +
  scale_x_log10() +
  scale_color_viridis(option = "H", discrete = FALSE)

#Plot terminal year density plots
g_b_ly + g_s_ly + g_f_ly + g_n_ly + plot_layout(ncol=  2)

# RUN SIMULATIONS FOR BRPs

## HS
source("./R/script/load_HS_param_example_data.R")
MSY_nsim <- 100
res_hs100lite <- #nsim100
  raw_res_fine %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))

usethis::use_data(res_hs100lite, overwrite = TRUE)

res_hs100_tidy <-
  res_hs100lite %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result[[1]]$SSB[1]),
                fmsy = purrr::map2(brp, res, function(x,y) x$result[[1]]$`Fref/Fcur`[1] * y$Fc.mean),
                fcur = purrr::map(res, function(x) x$Fc.mean),
                sb2017 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>%
  tidyr::unnest(sbmsy, fmsy, fcur, sb2017) %>%
  dplyr::mutate(sbratio = sb2017/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result[[1]]$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result[[1]]$SSB[1] / x$result[[1]]$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0) %>%
  dplyr::mutate(perSPR = purrr::map(brp, function(x) x$result[[1]]$perSPR[1])) %>%
  tidyr::unnest(perSPR)

usethis::use_data(res_hs100_tidy, overwrite = TRUE)


# res_hs100lite$brp[[1]]$result[[1]] # BRP
# res_hs100lite$brp[[1]]$result[[2]] # SR data

## BH
source("./R/script/load_BH_param_example_data.R")
MSY_nsim <- 100
res_bh100lite <- #nsim100
  raw_res_fine %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))

usethis::use_data(res_bh100lite, overwrite = TRUE)

res_bh100_tidy <-
  res_bh100lite %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result[[1]]$SSB[1]),
                fmsy = purrr::map2(brp, res, function(x,y) x$result[[1]]$`Fref/Fcur`[1] * y$Fc.mean),
                fcur = purrr::map(res, function(x) x$Fc.mean),
                sb2017 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>%
  tidyr::unnest(sbmsy, fmsy, fcur, sb2017) %>%
  dplyr::mutate(sbratio = sb2017/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result[[1]]$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result[[1]]$SSB[1] / x$result[[1]]$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0) %>%
  dplyr::mutate(perSPR = purrr::map(brp, function(x) x$result[[1]]$perSPR[1])) %>%
  tidyr::unnest(perSPR)

usethis::use_data(res_bh100_tidy, overwrite = TRUE)

## RI
source("./R/script/load_RI_param_example_data.R")
MSY_nsim <- 100
res_ri100lite <- #nsim100
  raw_res_fine %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))

usethis::use_data(res_ri100lite, overwrite = TRUE)

res_ri100_tidy <-
  res_ri100lite %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result[[1]]$SSB[1]),
                fmsy = purrr::map2(brp, res, function(x,y) x$result[[1]]$`Fref/Fcur`[1] * y$Fc.mean),
                fcur = purrr::map(res, function(x) x$Fc.mean),
                sb2017 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>%
  tidyr::unnest(sbmsy, fmsy, fcur, sb2017) %>%
  dplyr::mutate(sbratio = sb2017/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result[[1]]$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result[[1]]$SSB[1] / x$result[[1]]$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0) %>%
  dplyr::mutate(perSPR = purrr::map(brp, function(x) x$result[[1]]$perSPR[1])) %>%
  tidyr::unnest(perSPR)

usethis::use_data(res_ri100_tidy, overwrite = TRUE)


# Extracted result on BRPs
res_100_tidy <-
  dplyr::bind_rows(
    res_hs100_tidy %>% dplyr::mutate(SR = "HS"),
    res_bh100_tidy %>% dplyr::mutate(SR = "BH"),
    res_ri100_tidy %>% dplyr::mutate(SR = "RI")
  )

usethis::use_data(res_100_tidy, overwrite = TRUE)

res_100_tidy %>%
  ggplot(aes(M, sbmsy)) +
  geom_point(aes(shape = SR, color = M, group = M)) +
  geom_line(aes(linetype = SR)) +
  scale_color_viridis(option = "H") +
  coord_cartesian(ylim = c(1, 1e6)) +theme_bw() +
  labs(y = "SBmsy (ton)", title = "SBmsy for different Ms and SR models")

ggsave("./figs/SBmsy_M.png", width = 7, height= 5)

res_100_tidy %>%
  ggplot(aes(M, fmsy)) +
  geom_point(aes(shape = SR, color = M, group = M)) +
  geom_line(aes(linetype = SR)) +
  scale_color_viridis(option = "H") +
  coord_cartesian(ylim = c(0,1)) +theme_bw() +
  labs(y = "Fmsy", title = "Fmsy for different Ms and SR models")

ggsave("./figs/Fmsy_M.png", width = 7, height= 5)

res_100_tidy %>%
  ggplot(aes(sbmsy, fmsy)) +
  geom_point(aes(shape = SR, color = M, group = M)) +
  geom_path(aes(linetype = SR)) +
  scale_color_viridis(option = "H") +
  coord_cartesian(ylim = c(0,1), xlim = c(0, 3E5)) +theme_bw() +
  labs(y = "Fmsy", x = "SBmsy (ton)", title = "SBmsy vs Fmsy for different Ms and SR models") +
  facet_wrap(~SR)

ggsave("./figs/SBmsy_Fmsy_facet.png", width = 7, height= 5)

res_100_tidy %>%
  ggplot(aes(sbmsy, fmsy)) +
  geom_point(aes(shape = SR, color = M, group = M)) +
  geom_path(aes(linetype = SR)) +
  scale_color_viridis(option = "H") +
  coord_cartesian(ylim = c(0,1), xlim = c(0, 3E5)) +theme_bw() +
  labs(y = "Fmsy", x = "SBmsy (ton)", title = "SBmsy vs Fmsy for different Ms and SR models")

ggsave("./figs/SBmsy_Fmsy.png", width = 7, height= 5)

res_100_tidy %>%
  ggplot(aes(sbmsy, fmsy)) +
  geom_point(aes(shape = SR, color = M, group = M)) +
  geom_path(aes(linetype = SR)) +
  scale_color_viridis(option = "H") +
  coord_cartesian(ylim = c(0.001,1)) +theme_bw() +
  labs(y = "Fmsy", x = "SBmsy (ton)", title = "SBmsy vs Fmsy for different Ms and SR models") +
  scale_x_log10()

ggsave("./figs/SBmsy_Fmsy_logx.png", width = 7, height= 5)

res_100_tidy %>%
  ggplot(aes(sbmsy, fmsy)) +
  geom_point(aes(shape = SR, color = M, group = M)) +
  geom_path(aes(linetype = SR)) +
  scale_color_viridis(option = "H") +
  coord_cartesian(ylim = c(0.001,1)) +theme_bw() +
  labs(y = "Fmsy", x = "SBmsy (ton)", title = "SBmsy vs Fmsy for different Ms and SR models") +
  scale_x_log10() +facet_wrap(~SR, scales = "free_x")

ggsave("./figs/SBmsy_Fmsy_logx_facet.png", width = 7, height= 5)

# SR plots


res_ri100lite %>%
  dplyr::mutate(srdat = purrr::map(brp, function(x) x$result[[2]]$pred)) %>%
  dplyr::mutate(Rsr = purrr::map(srdat, function(x) x$R),
                SSBsr = purrr::map(srdat, function(x) x$SSB)) %>%
  dplyr::select(M, Rsr, SSBsr) %>%
  tidyr::unnest(Rsr, SSBsr) %>%
  dplyr::filter(M < 1.5) %>%
  ggplot() +
  geom_line(aes(y = Rsr, x = SSBsr, color=M, group = M)) +
  scale_color_viridis(option = "H") +
  labs(x = "SSB", y = "Recruitment", title = "Ricker_SR at each M") +
  theme_bw()

ggsave("./figs/ri_sr_plots.png", width = 8, height= 6)

res_bh100lite %>%
  dplyr::mutate(srdat = purrr::map(brp, function(x) x$result[[2]]$pred)) %>%
  dplyr::mutate(Rsr = purrr::map(srdat, function(x) x$R),
                SSBsr = purrr::map(srdat, function(x) x$SSB)) %>%
  dplyr::select(M, Rsr, SSBsr) %>%
  tidyr::unnest(Rsr, SSBsr) %>%
  dplyr::filter(M < 1.5) %>%

  ggplot() +
  geom_line(aes(y = Rsr, x = SSBsr, color=M, group = M)) +
  scale_color_viridis(option = "H") +
  labs(x = "SSB", y = "Recruitment", title = "Beverton_Holt_SR at each M") +
  theme_bw()

ggsave("./figs/bh_sr_plots.png", width = 8, height= 6)

res_hs100lite %>%
  dplyr::mutate(srdat = purrr::map(brp, function(x) x$result[[2]]$pred)) %>%
  dplyr::mutate(Rsr = purrr::map(srdat, function(x) x$R),
                SSBsr = purrr::map(srdat, function(x) x$SSB)) %>%
  dplyr::select(M, Rsr, SSBsr) %>%
  tidyr::unnest(Rsr, SSBsr) %>%
  dplyr::filter(M < 1.5) %>%

  ggplot() +
  geom_line(aes(y = Rsr, x = SSBsr, color=M, group = M)) +
  scale_color_viridis(option = "H") +
  labs(x = "SSB", y = "Recruitment", title = "Hockey Stick_SR at each M") +
  theme_bw()

ggsave("./figs/hs_sr_plots.png", width = 8, height= 6)

# HS oreten
# res_hs100lite$brp[[1]]$result[[2]]$pars
res_hs100lite %>% dplyr::mutate(a = purrr::map(brp, function(x) x$result[[2]]$pars$a),
                                b= purrr::map(brp, function(x) x$result[[2]]$pars$b)) %>%
  tidyr::unnest(a, b) %>%
  dplyr::mutate(break_pt = a * b) %>% ggplot() +
  geom_point(aes(M, break_pt, color = M, group = M)) +
  geom_line(aes(M, break_pt)) +
  scale_color_viridis(option = "H") +theme_bw() +
  labs(x = "M", y  ="HS b (break point SSB)", title = "Hockey-Stick break point SSB (b)")

ggsave("./figs/hs_breakpt.png", width = 6, height=  4)

# steepness h for bh
cond_init_bh <-
  res_bh100lite %>% dplyr::mutate(h = purrr::map(brp, function(x) x$result[[2]]$steepness$h),
                                SPR0= purrr::map(brp, function(x) x$result[[2]]$steepness$SPR0),
                                SB0 = purrr::map(brp, function(x) x$result[[2]]$steepness$SB0),
                                B0 = purrr::map(brp, function(x) x$result[[2]]$steepness$B0),
                                R0 = purrr::map(brp, function(x) x$result[[2]]$steepness$R0)) %>%
  tidyr::unnest(h, SPR0, SB0, B0, R0) %>%
  dplyr::select(-res, -brp) %>%
  dplyr::mutate(SR=  "BH")

cond_init_ri <-
  res_ri100lite %>% dplyr::mutate(h = purrr::map(brp, function(x) x$result[[2]]$steepness$h),
                                  SPR0= purrr::map(brp, function(x) x$result[[2]]$steepness$SPR0),
                                  SB0 = purrr::map(brp, function(x) x$result[[2]]$steepness$SB0),
                                  B0 = purrr::map(brp, function(x) x$result[[2]]$steepness$B0),
                                  R0 = purrr::map(brp, function(x) x$result[[2]]$steepness$R0)) %>%
  tidyr::unnest(h, SPR0, SB0, B0, R0) %>%
  dplyr::select(-res, -brp) %>%
  dplyr::mutate(SR=  "RI")

cond_init_hs <-
  res_hs100lite %>% dplyr::mutate(h = purrr::map(brp, function(x) x$result[[2]]$steepness$h),
                                  SPR0= purrr::map(brp, function(x) x$result[[2]]$steepness$SPR0),
                                  SB0 = purrr::map(brp, function(x) x$result[[2]]$steepness$SB0),
                                  B0 = purrr::map(brp, function(x) x$result[[2]]$steepness$B0),
                                  R0 = purrr::map(brp, function(x) x$result[[2]]$steepness$R0)) %>%
  tidyr::unnest(h, SPR0, SB0, B0, R0) %>%
  dplyr::select(-res, -brp) %>%
  dplyr::mutate(SR=  "HS")

ggplot(cond_init_bh) +
  geom_point(aes(M, h, color = M, group = M))


cond_init_all <- dplyr::bind_rows(cond_init_hs, cond_init_bh, cond_init_ri)

ggplot(cond_init_all) +
  geom_line(aes(M, SB0, linetype = SR, color =SR)) + scale_y_log10() +xlim(1, 1.5)
