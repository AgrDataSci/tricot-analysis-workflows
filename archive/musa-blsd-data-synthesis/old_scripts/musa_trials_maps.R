library(sf)
library(ggplot2)
library(tmap)


data("World")


#trial locations
musa_trials_time_loc <- read.csv("data/processed/musa_trials_time_loc.csv", row.names = 1, check.names = FALSE)

musa_sf <- st_as_sf(x = musa_trials_time_loc, coords = c("lon", "lat"), crs = 4326)

row.names(musa_sf)




ggplot() + 
  geom_sf(data = World) + 
  geom_sf(data = musa_sf) +
  coord_sf(xlim = c(180, -162), ylim = c(82, -55), expand = T) 
  
ggsave("output/figures/trial_locations.png", dpi = 300, width = 16, height = 8)


ca_trials_map <- ggplot() +
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_sf(data = rbca_countries, fill = "white", col = "slategray4") +
  geom_sf(data = tricot_data_sf,
          aes(color = trial_name, shape = org),
          size = sym_size) +
  coord_sf(xlim = c(-91, -82), ylim = c(8, 16), expand = T) +
  ggtitle("") +
  labs(color = "Trial ID", shape = "Organization") +
  scale_color_manual(values = trials_pal) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  geom_text(data = ca_countries_ctrds, aes(label = GID_0,  x = x2, y = y2)) +
  theme(panel.background = element_rect(fill = "aliceblue",
                                        colour = "aliceblue",
                                        size = 0.5,
                                        linetype = "solid"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
