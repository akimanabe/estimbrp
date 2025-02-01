# Run multi vpa for sensitivity analysis on M

# Width of M
mlist <- seq(0, 2, 0.1)

# VPA for each M
raw_res <- run_vpa_mlist(res_vpa_example, mlist)

library(patchwork)

g_b <- raw_res %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "biomass")
g_s <- raw_res %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "ssb")
g_f <- raw_res %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "meanF")
g_n <- raw_res %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "number")

g_b + g_s + g_f + g_n + plot_layout(ncol = 2)

last_year_res <-
  raw_res %>% spread_result() %>% tidy_result() %>%
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

g_b_ly + g_s_ly + g_f_ly + g_n_ly + plot_layout(ncol=  2)

raw_res

raw_res$res[[1]]

MSY_nsim <- 100 # nsim50 = 21.267sec, nsim100 = 30.652sec, nsim2 = 10sec
MSY_seed <- 2
system.time(calc_brp(res_vpa_example))

res_brp <-
  raw_res %>%dplyr::filter(M <= 1.0) %>%
  dplyr::mutate(brp = purrr::map(.x = res, function(x) calc_brp(x)))

res_brp2 <- #nsim100
  raw_res %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))

res_brp2$brp[[1]]$result$`Fref/Fcur`[1] * res_brp2$res[[1]]$Fc.mean # 0.01845

mean(c(0.00979, 0.0198, 0.0221, 0.0221))
res_brp2$res[[1]]$Fc.mean

mean(c(0.7978153,1.6159007,1.8028372,1.8028372))

res_brp2 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  ggplot() +
  geom_path(aes(sbmsy, fmsy)) +
  geom_point(aes(sbmsy, fmsy, color = M, group = M)) +
  scale_color_viridis(option = "H", discrete = FALSE) +
  scale_x_log10()
usethis::use_data(res_brp2)

#nsim1000
MSY_nsim <- 1000
res_brp3 <- #nsim100
  raw_res %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))

res_brp3 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  ggplot() +
  geom_path(aes(sbmsy, fmsy)) +
  geom_point(aes(sbmsy, fmsy, color = M, group = M)) +
  scale_color_viridis(option = "H", discrete = FALSE) +
  scale_x_log10()


usethis::use_data(res_brp3, overwrite = TRUE)

res_brp3 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) %>%
  ggplot() +
  geom_path(aes(sbratio, fratio)) +
  geom_point(aes(sbratio, fratio, group = M, color = M)) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_color_viridis(option = "H")

res_brp3 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) %>%
  ggplot() +
  geom_path(aes(diff_m, fratio))

# M 0.01 de yaru
# Width of M
mlist <- seq(0, 1.5, 0.01)

raw_res_fine <- run_vpa_mlist(res_vpa_example, mlist)

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

library(viridis)
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

library(patchwork)
g_b_ly + g_s_ly + g_f_ly + g_n_ly + plot_layout(ncol=  2)

MSY_nsim <- 100

source("./R/script/calc_brp.R")
res_brp4 <- #nsim100
  raw_res_fine %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))


usethis::use_data(res_brp4, overwrite = TRUE)

res_brp_dat <-
  res_brp4 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) #%>% dplyr::arrange(desc(fmsy)) # M=0.66が一番高い。なぜか。

  ggplot(res_brp_dat) +
  geom_path(aes(sbmsy, fmsy), linetype = "dotted", color=  "black") +
  geom_point(aes(sbmsy, fmsy, fill = M, group = M), pch = 21, size =3) +
  scale_fill_viridis(option = "H") +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1000000)) + theme_classic() +
  labs(x = "SBmsy (ton)", y = "Fmsy", title = "Sensitivity analsys on HSL2AR1 Estimation")

res_brp_dat

MSY_nsim <- 1000

source("./R/script/calc_brp.R")
res_brp5 <- #nsim100
  raw_res_fine %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))


usethis::use_data(res_brp5, overwrite = TRUE)

res_brp_dat <-
  res_brp5 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result$SSB[1] / x$result$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0) %>%
  dplyr::mutate(perSPR = purrr::map(brp, function(x) x$result$perSPR[1])) %>%
  tidyr::unnest(perSPR)

# SBmsy to Fmsy
ggplot(res_brp_dat) +
  geom_path(aes(sbmsy, fmsy), linetype = "dotted", color=  "black") +
  geom_point(aes(sbmsy, fmsy, fill = M, group = M), pch = 21, size =2) +
  scale_fill_viridis(option = "H") +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1000000)) + theme_classic() +
  labs(x = "SBmsy (ton)", y = "Fmsy", title = "Sensitivity analsys on HSL2AR1 Estimation")

# kobe terminal
ggplot(res_brp_dat) +
  geom_path(aes(sbratio, fratio), linetype = "dotted", color=  "black") +
  geom_point(aes(sbratio, fratio, fill = M, group = M), pch = 21, size =2) +
  scale_fill_viridis(option = "H") +
  coord_cartesian(ylim = c(0, 10), xlim = c(0, 1)) + theme_classic() +
  labs(x = "SBmsy (ton)", y = "Fmsy", title = "Terminal year on kobe plot")

ggplot(res_brp_dat) +
  geom_path(aes(M, msy))

ggplot(res_brp_dat) +
  geom_path(aes(M, fmsy))

ggplot(res_brp_dat) +
  geom_path(aes(M, sb2023))

ggplot(res_brp_dat, aes(M, perSPR)) +
  geom_path() +
  geom_point(aes(group = M, color = M))

res_brp5 %>%
  dplyr::filter(M<1.5) %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result$SSB[1] / x$result$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0)%>% select(M, sbmsy, fmsy, sb2023, fcur,sbratio, fratio, diff_m, msy, b0) %>% dplyr::mutate(M=factor(M))  %>% pairs()

res_brp_dat_hs <-
  res_brp5 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result$SSB[1] / x$result$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0) %>%
  dplyr::mutate(perSPR = purrr::map(brp, function(x) x$result$perSPR[1])) %>%
  tidyr::unnest(perSPR)

# WHAT IF I ASSUME BH

source("./R/script/load_BH_param_example_data.R")

source("./R/script/calc_brp.R")
MSY_nsim <- 100
res_brp_bh100 <- #nsim100
  raw_res_fine %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))


usethis::use_data(res_brp_bh100, overwrite = TRUE)

res_brp_dat_bh <-
  res_brp_bh100 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result$SSB[1] / x$result$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0) %>%
  dplyr::mutate(perSPR = purrr::map(brp, function(x) x$result$perSPR[1])) %>%
  tidyr::unnest(perSPR) #%>% select(M, sbmsy, fmsy, sb2023, fcur,sbratio, fratio, diff_m, msy, b0) %>% dplyr::mutate(M=factor(M))  %>% pairs()

ggplot(res_brp_dat_bh) +
  geom_path(aes(sbmsy, fmsy), linetype = "dotted", color=  "black") +
  geom_point(aes(sbmsy, fmsy, fill = M, group = M), pch = 21, size =2) +
  scale_fill_viridis(option = "H") +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0, 1000000)) + theme_classic() +
  labs(x = "SBmsy (ton)", y = "Fmsy", title = "Sensitivity analsys on BHL2AR1 Estimation")


# WHAT IF I ASSUME RI

source("./R/script/load_RI_param_example_data.R")

source("./R/script/calc_brp.R")
MSY_nsim <- 100
res_brp_ri100 <- #nsim100
  raw_res_fine %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))


usethis::use_data(res_brp_ri100, overwrite = TRUE)

res_brp_dat_ri <-
  res_brp_ri100 %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2017` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  dplyr::mutate(diff_m = M-0.5) %>%
  dplyr::mutate(msy = purrr::map(brp, function(x) x$result$Catch[1])) %>%
  dplyr::mutate(b0 = purrr::map(brp, function(x) x$result$SSB[1] / x$result$SSB2SSB0[1])) %>%
  tidyr::unnest(msy, b0) %>%
  dplyr::mutate(perSPR = purrr::map(brp, function(x) x$result$perSPR[1])) %>%
  tidyr::unnest(perSPR) #%>% select(M, sbmsy, fmsy, sb2023, fcur,sbratio, fratio, diff_m, msy, b0) %>% dplyr::mutate(M=factor(M))  %>% pairs()

ggplot(res_brp_dat_ri) +
  geom_path(aes(sbmsy, fmsy), linetype = "dotted", color=  "black") +
  geom_point(aes(sbmsy, fmsy, fill = M, group = M), pch = 21, size =2) +
  scale_fill_viridis(option = "H") +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 250000)) + theme_classic() +
  labs(x = "SBmsy (ton)", y = "Fmsy", title = "Sensitivity analsys on RIL2AR1 Estimation")

res_brp_dat_bh %>% select(M, sbmsy, fmsy, sb2023, fcur, sbratio, fratio, msy, b0, perSPR) %>% dplyr::mutate(z = fcur + M) %>% pairs()
res_brp_dat_ri %>% select(M, sbmsy, fmsy, sb2023, fcur, sbratio, fratio, msy, b0, perSPR) %>% dplyr::mutate(z = fcur + M) %>% pairs()


ggplot(res_brp_dat_bh, aes(M, sbmsy)) +
  geom_path() +
  geom_point(aes(fill = M, group = M), pch = 21, size = 2) +
  scale_fill_viridis(option = "H")

res_brp_dat_bh


res_brp_all <-
  dplyr::bind_rows(
    res_brp_dat_bh %>% dplyr::mutate(SR = "BH"),
    res_brp_dat_ri %>% dplyr::mutate(SR = "RI"),
    res_brp_dat_hs %>% dplyr::mutate(SR = "HS")
  )

object.size(res_brp_all)

usethis::use_data(res_brp_all, overwrite = TRUE)

res_brp_all %>%
  ggplot(aes(sbmsy, fmsy)) +
  geom_path(aes(group = SR, linetype = SR)) +
  geom_point(aes(group = M, fill = M, shape = SR), size = 2, alpha = 0.3) +
  scale_shape_manual(values = c(21, 22, 23)) +
  scale_fill_viridis(option = "H")+coord_cartesian(xlim = c(0, 1E6))
