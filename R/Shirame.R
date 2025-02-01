# Seto hirame example
# load hirame vpa data (res_vpa_24_b)
hiramedat <- load("./data-raw/ヒラメS_frasyr/res_vpa2024.rda")

res_vpa_24_b
mlist <- seq(0, 2, 0.1)

# VPA for each M
hirame_res <- run_vpa_mlist(res_vpa_24_b, mlist)

g_b <- hirame_res %>% spread_result() %>% tidy_result() %>% plot_timeline(dat = ., plot = "biomass") +scale_y_log10()
g_s <- hirame_res %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "ssb")+scale_y_log10()
g_f <- hirame_res %>% spread_result() %>% tidy_result() %>% dplyr::filter(M < 1) %>% plot_timeline(dat = ., plot = "meanF")+scale_y_log10()
g_n <- hirame_res %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "number")+scale_y_log10()

g_b + g_s + g_f + g_n + plot_layout(ncol = 2)
#
# run_vpa_mlist
#
# hoge <- run_vpa(res_vpa_24_b, M_alt = c(0.1, rep(0.2, 5)))
#
# hoge$input$dat$M
#
# Ms <-
#
# run_vpa_mlist(res_vpa_24_b, Ms)
#
# tibble::tibble(Ms = Ms)
# run_vpa_mlist2 <- function(res_vpa, M) {
#   assertthat::assert_that(is.numeric(M), msg = "M must be given as number(s)")
#   assertthat::assert_that(class(res_vpa)=="vpa",
#                           msg = "res_vpa must be vpa result")
#   assertthat::assert_that(!is.null(res_vpa), msg = "res_vpa must be provided")
#   assertthat::assert_that(!is.null(M), msg = "M must be provided")
#
#   tibble::tibble(M = M) %>%
#     dplyr::mutate(dat = list(res_vpa)) %>%
#     dplyr::mutate(res = purrr::map2(dat, M, function(x, y) run_vpa(x, unlist(y)))) %>%
#     dplyr::select(M, res)
# }
#
# hirame_res2 <- run_vpa_mlist2(res_vpa_24_b, Ms)

mlist_hirame <- c(seq(0, 2, 0.1), 0.3125)
hirame_ms <- lapply(mlist_hirame, function(x) c(x / 4, rep(x, 5))) #longevity = 8


hirame_vpa_mlist <-
  function(dat){
    tibble::tibble(M = hirame_ms) %>%
  dplyr::mutate(dat = list(dat)) %>%
  dplyr::mutate(res = purrr::map2(dat, M, function(x, y) run_vpa(x, unlist(y)))) %>%
  dplyr::select(M, res)
  }

# M with age 0 = 0.
hirame_res <-
  hirame_vpa_mlist(res_vpa_24_b)

hirame_res_m <-
  hirame_res %>% dplyr::mutate(Mlist = M) %>% mutate(M = purrr::map(M, function(x)x[2])) %>% tidyr::unnest(M) %>% dplyr::arrange(M)


g_b <- hirame_res_m %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "biomass") +scale_y_log10()
g_s <- hirame_res_m %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "ssb")+scale_y_log10()
g_f <- hirame_res_m %>% spread_result() %>% tidy_result() %>% dplyr::filter(M < 1) %>% plot_timeline(dat = ., plot = "meanF") + coord_cartesian(ylim = c(0, 1))
g_n <- hirame_res_m %>% spread_result() %>% tidy_result() %>% dplyr::filter(M <= 1) %>% plot_timeline(dat = ., plot = "number")+scale_y_log10()

g_b + g_s + g_f + g_n + plot_layout(ncol = 2)

hirame_res_m %>% spread_result() %>% tidy_result %>% ggplot() + geom_line(aes(Year, saa, group = M, color = M)) + facet_wrap(~Age) + scale_color_viridis(option = "H") + theme_bw()

hirame_res_m %>% spread_result() %>% tidy_result %>% ggplot() + geom_line(aes(Age, saa, group = M, color = M)) + facet_wrap(~Year) + scale_color_viridis(option = "H") + theme_bw()

MSY_nsim <- 100
hirame_brp <- #nsim100
  hirame_res_m %>%
  dplyr::mutate(brp = purrr::map(.x = res, calc_brp_safe))

hirame_brp %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  ggplot() +
  geom_path(aes(sbmsy, fmsy)) +
  geom_point(aes(sbmsy, fmsy, color = M, group = M)) +
  scale_color_viridis(option = "H", discrete = FALSE) +
  scale_x_log10()

hirame_brp$res[[1]]$ssb$`2023` %>% sum()


hirame_brp %>%
  dplyr::mutate(sbmsy = purrr::map(brp, function(x) x$result$SSB[1])) %>%
  dplyr::mutate(fmsy = purrr::map2(brp, res, function(x,y) x$result$`Fref/Fcur`[1] * y$Fc.mean)) %>% unnest(sbmsy) %>% unnest(fmsy) %>%
  dplyr::mutate(fcur = purrr::map(res, function(x) x$Fc.mean)) %>%
  tidyr::unnest(fcur) %>%
  dplyr::mutate(sb2023 = purrr::map(res, function(x) x$ssb$`2023` %>% sum())) %>% tidyr::unnest(sb2023) %>%
  dplyr::mutate(sbratio = sb2023/sbmsy,
                fratio = fcur/fmsy) %>%
  ggplot() +
  geom_path(aes(sbratio, fratio))
