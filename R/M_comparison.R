# Estim multiple Ms
library(tidyverse)
dat <- readr::read_csv("./M_biopar.csv")

Mdat <-
  dat %>%
pivot_longer(-c(Species, Stock, M_method, Longevity, Mmax, Tmat, Wmat, Sex, Linf, K, t0),
             names_to = "Type", values_to ="value") %>%
  dplyr::mutate(Categ = stringr::str_extract(Type, "W|L"),
                Age = stringr::str_extract(Type, "\\d+")) %>%
  select(-Type) %>%
  tidyr::pivot_wider(names_from = Categ, values_from = value) %>%
  dplyr::mutate(Lorenzen_mat = 3.69*Wmat^-0.305) %>%
  dplyr::mutate(Lorenzen = 3.69*W^-0.305) %>%
  dplyr::mutate(Cope = 5.4/Longevity) %>%
  dplyr::mutate(Charnov = (L/Linf)^-1.5*K) %>%
  dplyr::mutate(Jensen = 1.753*K) %>%
  dplyr::mutate(Japan = Mmax) %>%
  dplyr::mutate(Hewitt = 4.966/Longevity) %>%
  tidyr::pivot_longer(-c(Species, Stock, M_method, Longevity, Mmax, Tmat, Wmat, Sex, Linf, K, t0, Age, W, L),
names_to = "Mtype", values_to = "M") %>%
  dplyr::mutate(Age = as.double(Age)) %>%
  dplyr::mutate(SPST = stringr::str_c(Species, Stock)) %>%
  dplyr::mutate(SPSTSEX = stringr::str_c(Species, Stock, Sex))

Mdat_annual <- Mdat%>%
  dplyr::filter(Mtype != "Charnov") %>%
  dplyr::filter(Mtype != "Lorenzen")

Mdat_annual %>% dplyr::select(Species, Stock, SPSTSEX, Longevity, Mmax, Mtype, M) %>% unique() %>% dplyr::mutate(diff = M-Mmax) %>%
  tidyr::drop_na(diff) %>% dplyr::arrange(diff)

Mdat_annual %>% dplyr::select(Species, Stock, SPSTSEX, Longevity, Mmax, Mtype, M) %>% unique() %>% dplyr::mutate(diff = M-Mmax) %>%
  tidyr::drop_na(diff) %>%
  dplyr::mutate(st_diff = diff/M) %>%
  dplyr::filter(Mtype != "Japan") %>%
  ggplot() +
  geom_point(aes(Mmax,  st_diff, color = Species, group = Species, shape = Mtype)) +
  facet_wrap(~Species, scales = "free_y") +
  geom_hline(aes(yintercept = 0), color = "red4", linetype = "dashed") + theme_bw() +
  labs(x = "Current M", y = "Scaled difference", title = "Scaled difference of current M and estimated M")

ggsave("./figs/scaled_diff_M_species.png",width = 10, height = 7)

Mdat_annual %>% dplyr::select(Species, Stock, SPSTSEX, Longevity, Mmax, Mtype, M) %>% unique() %>%
  dplyr::filter(Mtype != "Japan") %>% #dplyr::filter(Mtype == "Jensen") %>% print(n=50)
  ggplot() +
  geom_point(aes(x = Mmax, y = M, shape = Mtype, color = Species, group = Species), size = 2)+
  stat_function(fun = function(x) x, color = "red4", linetype = "dashed") +
  theme_bw() +
  labs(x = "Current M" ,y = "Estimated other M", title = "Current M vs M by other estimators")

ggsave("./figs/MvsotehrM.png",width = 8, height = 5)

Mdat_annual %>% dplyr::select(Species, Stock, SPSTSEX, Longevity, Mmax, Mtype, M) %>% unique() %>%
  dplyr::filter(Mtype != "Japan") %>% #dplyr::filter(Mtype == "Jensen") %>% print(n=50)
  ggplot() +
  geom_point(aes(x = Mmax, y = M, shape = Mtype, color = Species, group = Species), size = 2)+
  stat_function(fun = function(x) x, color = "red4", linetype = "dashed") +
  theme_bw() +
  labs(x = "Current M" ,y = "Estimated other M", title = "Current M vs M by other estimators") +scale_y_log10()

ggsave("./figs/MvsotehrM_logy.png",width = 8, height = 5)

Mdat_annual %>% dplyr::select(Species, Stock, SPSTSEX, Longevity, Mmax, Mtype, M) %>% unique() %>%
  dplyr::filter(Mtype != "Japan") %>% #dplyr::filter(Mtype == "Jensen") %>% print(n=50)
  ggplot() +
  geom_point(aes(x = Mmax, y = M, shape = Mtype, color = Species, group = Species), size = 2)+
  stat_function(fun = function(x) x, color = "red4", linetype = "dashed") +
  theme_bw() +
  labs(x = "Current M" ,y = "Estimated other M", title = "Current M vs M by other estimators") +scale_y_log10() +
  facet_wrap(~Mtype)

ggsave("./figs/MvsotehrM_logy_facet_model.png",width = 10, height = 6)

Mcomp <- Mdat_annual %>% dplyr::select(Species, Stock, SPSTSEX, Longevity, Mmax, Mtype, M) %>% unique() %>%
  tidyr::pivot_wider(names_from = Mtype, values_from = M)

copejensen <- ggplot(Mcomp) + geom_point(aes(Cope, Jensen, color = Species), size = 2)+
  stat_function(fun = function(x) x, color = "red4", linetype = "dashed") +
  theme_bw() +
  labs(title = "Cope vs Jensen") +scale_y_log10() +
  guides(color=guide_legend(ncol=4))
copeloren <- ggplot(Mcomp) + geom_point(aes(Cope, Lorenzen_mat, color = Species), size = 2)+
  stat_function(fun = function(x) x, color = "red4", linetype = "dashed") +
  theme_bw() +
  labs(title = "Cope vs Lorenzen mat") +scale_y_log10()+
  guides(color=guide_legend(ncol=4))
jensenloren <- ggplot(Mcomp) + geom_point(aes(Jensen, Lorenzen_mat, color = Species), size = 2)+
  stat_function(fun = function(x) x, color = "red4", linetype = "dashed") +
  theme_bw() +
  labs(title = "Jensen vs Lorenzen mat") +scale_y_log10()+
  guides(color=guide_legend(ncol=4))
copejensen + copeloren + jensenloren + guide_area() + plot_layout(ncol = 2, guides = "collect")

ggsave("./figs/compare_each_estimators.png",width = 8, height = 6)


ggsave("./figs/M_multi.png", width = 8, height = 5)

ggplot() +
  geom_line(dat = Mdat %>% dplyr::filter(Mtype == "Lorenzen"),aes(Age, M, color = SPSTSEX, group = SPSTSEX)) +
  geom_line(dat = Mdat %>% dplyr::filter(Mtype == "Japan"),aes(Age, M, color = SPSTSEX, group = SPSTSEX), linetype = "dashed", size = 1) +
  facet_wrap(~Species, scales = "free") +theme_bw()+
  theme(legend.position = "none")

ggplot() +
  geom_line(dat = Mdat %>% dplyr::filter(Mtype == "Charnov"),aes(Age, M, color = SPSTSEX, group = SPSTSEX)) +
  geom_line(dat = Mdat %>% dplyr::filter(Mtype == "Japan"),aes(Age, M, color = SPSTSEX, group = SPSTSEX), linetype = "dashed", size = 1) +
  facet_wrap(~Species, scales = "free") +theme_bw()+
  theme(legend.position = "none")

Mdat2 <-
  Mdat %>% dplyr::filter(Species != "スルメイカ")%>% dplyr::filter(Species != "ズワイガニ")
ggplot() +
  geom_line(dat = Mdat2 %>% dplyr::filter(Mtype == "Lorenzen"),aes(Age, M, group = SPSTSEX), color = "red4", size = 0.6) +
  geom_line(dat = Mdat2 %>% dplyr::filter(Mtype == "Charnov"),aes(Age, M, group = SPSTSEX), color = "blue4", size = 0.6) +
  geom_line(dat = Mdat2 %>% dplyr::filter(Mtype == "Japan"),aes(Age, M, , group = SPSTSEX), color = "green4", size = 0.7) +
  facet_wrap(~Species, scales = "free") +theme_bw()+
  theme(legend.position = "none") +
  scale_y_log10()

ggsave("./figs/age_dep_M.png", width = 8, height = 6)
