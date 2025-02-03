res_100_tidy %>%
  ggplot() +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, alpha = 1, fill = "khaki1", color = "black") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 1, ymax = 5, alpha = 1, fill = "khaki1", color = "black") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 1, ymax = 5, alpha = 1, fill = "indianred1", color = "black") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 1, alpha = 1, fill = "olivedrab2", color = "black") +
  geom_point(aes(sbratio, fratio, fill = M, group = M), size = 4, pch = 21, alpha = 0.6) +
  # geom_line(aes(sbratio, fratio)) +
  # coord_cartesian(ylim = c(0, 5), xlim = c(0, 2)) +
  # geom_hline(aes(yintercept = 1), color = "black") +
  # geom_vline(aes(xintercept = 1), color = "black") +
  ylim(0, 5) +
  scale_fill_viridis(option = "H") +
  facet_wrap(~SR) +
  coord_cartesian(ylim = c(0, 5), xlim = c(0, 2)) +
  theme_SH()

ggsave("./figs/kobe.png", width = 10, height = 5)
