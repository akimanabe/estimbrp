# Estim multiple Ms

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
  tidyr::pivot_longer(-c(Species, Stock, M_method, Longevity, Mmax, Tmat, Wmat, Sex, Linf, K, t0, Age, W, L),
names_to = "Mtype", values_to = "M") %>%
  dplyr::mutate(Age = as.double(Age)) %>%
  dplyr::mutate(SPST = stringr::str_c(Species, Stock)) %>%
  dplyr::mutate(SPSTSEX = stringr::str_c(Species, Stock, Sex))

Mdat_annual <- Mdat%>%
  dplyr::filter(Mtype != "Charnov") %>%
  dplyr::filter(Mtype != "Lorenzen")

ggplot()+
  geom_point(dat = Mdat_annual %>% dplyr::filter(Mtype != "Japan"), aes(Longevity, M, color = Species, shape = Mtype),size = 2) +
  geom_point(dat= Mdat_annual %>% dplyr::filter(Mtype == "Japan"), aes(Longevity, M, color = Species),
             size = 3, pch=21)+
  # scale_x_log10() +
  ylim(0, 6) +
  theme_bw()

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
