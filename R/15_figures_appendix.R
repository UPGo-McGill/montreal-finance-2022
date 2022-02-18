adjacency_plot <- 
  data_model |> 
  st_as_sf() |>
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(fill = 'white', color = 'grey', alpha=1) +
  geom_sf(data = queen_adj_sf, 
          color = "#FF6600", 
          alpha=0.4,
          show.legend = TRUE) +
  scale_fill_stepsn(name= "Financialized rental units", 
                    colors = col_palette[c(4, 1, 2, 9)],
                    breaks = c(0.15, 0.30, 0.45, 0.60),
                    #values = c(0.2, 0.4, 0.6),
                    na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    labels = scales::percent) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7))

adjacency_plot

# Save plots for paper ---------------------------------------------------------

adjplot_fn <- "output/figures/BYM_adjacency_map"
ggsave(
  adjplot_fn,
  device = "png",
  plot = adjacency_plot)
