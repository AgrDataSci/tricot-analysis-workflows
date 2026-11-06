library(ggplot2)
library(tmap)
library(terra)
library(sf)
library(colorspace)
library(patchwork)
library(reshape2)
library(RColorBrewer)


RColorBrewer::brewer.pal(12, "Set3")
display.brewer.pal(n = 12, name = "Paired")[1:2]

display.brewer.pal(n = 9, name = "Set1")
display.brewer.pal(n = 8, name = "Dark2")

RColorBrewer::brewer.pal(9, "Set1")

clim_colpal <- c(RColorBrewer::brewer.pal(9, "Set1"),
                 RColorBrewer::brewer.pal(8, "Dark2"),
                 brewer.pal(n = 12, name = "Paired")[1:2])

data("World")

musa_trials_loc <- read.csv("data/processed/trial_info_ext.csv", 
                                 check.names = FALSE)

imtp_locs <- read.csv("data/agtrials_banana/clean/imtp/imtp_dates.csv")

musa_trials_loc_sf <- sf::st_as_sf(musa_trials_loc,
                                   coords = c("lon", "lat"),
                                   crs = 4326)

musa_trials_loc_sf$trial <- row.names(musa_trials_time_loc_sf)

banana_spam <- terra::rast("D:/GIS/DataSources/MapSpam/spam2010V2r0_global/geoTIFF/spam2010V2r0_global_A_BANA_A.tif")

plantain_spam <- terra::rast("D:/GIS/DataSources/MapSpam/spam2010V2r0_global/geoTIFF/spam2010V2r0_global_A_PLNT_A.tif")

banana_spam_df <- terra::as.data.frame(banana_spam, xy = TRUE)

plantain_spam_df <- terra::as.data.frame(plantain_spam, xy = TRUE)

terra::plot(banana_spam > 10)

terra::plot(plan > 10)


ggplot() + 
  geom_sf(data = World) + 
  geom_sf(data = musa_trials_loc_sf) + 
  coord_sf(xlim = c(-175, 155), ylim = c(-55, 80), expand = T) +
  geom_sf_text(data = musa_trials_loc_sf, aes(label = location_name), check_overlap = F)


ggsave("output/figures/musa_trial_locations.png", dpi = 300, width = 16, height = 12)


unq_locs <- musa_trials_loc_sf[!duplicated(musa_trials_loc$lon), ]

unq_locs <- unq_locs[!duplicated(unq_locs$location_name), ]

nrow(unq_locs_sf)

unq_locs_sf$nrow <- 1:nrow(unq_locs_sf)

unq_locs_sf <- sf::st_as_sf(unq_locs)

unq_locs_sf$id <- seq(1:nrow(unq_locs_sf))

unq_locs_sf$x <- st_coordinates(unq_locs_sf)[,1]

unq_locs_sf$y <- st_coordinates(unq_locs_sf)[,2]

merge(unq_locs_sf, imtp_locs, by.x = c("x","y"), by.y = c("longitude","latitude"))

unq_locs_sf <- unq_locs_sf[!unq_locs_sf$location_name == "Dole", ]

write.csv(unq_locs_sf, file = "data/processed/unique_locations.csv")


trials_p <- ggplot() + 
  geom_sf(data = World, fill = NA) + 
  # geom_raster(data = banana_spam_df[banana_spam_df$spam2010V2r0_global_A_BANA_A > 1, ],
  #             aes(x = x, y = y, fill = spam2010V2r0_global_A_BANA_A)) +
  # scale_fill_distiller(palette = "Spectral") +
  geom_sf(data = unq_locs_sf, aes(color = unq_locs_sf$location_name),
          size = 3) +
  geom_sf_text(data = unq_locs_sf, 
               label = unq_locs_sf$location_name,
               fontface = "bold",
               size = 6,
               # vjust = .25,
               # hjust = -.25,
               nudge_x = c(-10, 12, 10, -10, 8.5, -7.5, -8.5, 8, 8, 8.5,
                           8.5, -15, -12, 9, 10, 11, -10, 10, 8),
               nudge_y = c(0, 1.5, 0, 0, 1.75, -.5, 1.5, 0, 0, 0,
                           0, 0, 2.75, 0, 0, 2, 0, 0, 0)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-55, 80), expand = F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_manual(values = clim_colpal)

trials_p

ggsave("output/figures/musa_trial_locations.png", dpi = 600, width = 20, height = 12)


#Temperture
temp_files <- list.files("data/ecv_ERA5/temp/", full.names = TRUE)

temp_stack <- terra::rast(temp_files)

temp_stack <- temp_stack - 273.15

dim(temp_stack)

temp_stack <- terra::rotate(temp_stack)

temp_trials <- terra::extract(temp_stack, terra::vect(unq_locs_sf), list = F)

temp_trials <- merge(x = unq_locs_sf, y = temp_trials, by.x = "id", by.y = "ID")

colnames(temp_trials)[10:21] <- month.abb#c("id", month.abb)

temp_trials_df <- as.data.frame(temp_trials)

sel_cols <- c("id", "country", "location_name", "study", month.abb)

temp_trials_df <- temp_trials_df[sel_cols]


which(duplicated(temp_trials_df$location_name))

temp_trials_df_l <- melt(temp_trials_df, 
                         id.vars = c("location_name"), 
                         measure.vars = month.abb)
temp_trials_df_l

temp_p <- ggplot() + 
  geom_line(data = temp_trials_df_l, aes(x = variable,
                                         y = value,
                                         col = as.factor(location_name),
                                         group = as.factor(location_name)
                                         ), size = 1.35) + 
  # geom_point(data = temp_trials_df_l, aes(x = variable,
  #                                         y = value,
  #                                         col = as.factor(id))) +
  labs(x = "",
       y = "°C",
       title = "Average temperature 1990 - 2020",
       color = "Location name") +
  scale_color_manual(values = clim_colpal)

temp_p

#prec 

prec_files <- list.files("data/ecv_ERA5/prec/", full.names = TRUE)

prec_stack <- terra::rast(prec_files)

prec_stack <- prec_stack * 1000 * 30

dim(prec_stack)

prec_stack <- terra::rotate(prec_stack)

prec_trials <- terra::extract(prec_stack, terra::vect(unq_locs_sf), list = F)

prec_trials <- merge(x = unq_locs_sf, y = prec_trials, by.x = "id", by.y = "ID")

colnames(prec_trials)

colnames(prec_trials)[10:21] <- month.abb#c("id", month.abb)

prec_trials_df <- as.data.frame(prec_trials)

sel_cols <- c("id", "country", "location_name", "study", month.abb)

prec_trials_df <- prec_trials_df[sel_cols]


which(duplicated(prec_trials_df$location_name))

prec_trials_df_l <- melt(prec_trials_df, 
                         id.vars = c("location_name"), 
                         measure.vars = month.abb)
prec_trials_df_l

prec_p <- ggplot() + 
  geom_line(data = prec_trials_df_l, aes(x = variable,
                                         y = value,
                                         col = as.factor(location_name),
                                         group = as.factor(location_name)
  ), size = 1.35) + 
  # geom_point(data = prec_trials_df_l, aes(x = variable,
  #                                         y = value,
  #                                         col = as.factor(id))) +
  labs(x = "",
       y = "mm",
       title = "Average precipitation 1990 - 2020",
       color = "Location name") +
  scale_color_manual(values = clim_colpal)

prec_p

#rhum

rhum_files <- list.files("data/ecv_ERA5/rhum/", full.names = TRUE)

rhum_stack <- terra::rast(rhum_files)

rhum_stack <- rhum_stack

dim(rhum_stack)

rhum_stack <- terra::rotate(rhum_stack)

rhum_trials <- terra::extract(rhum_stack, terra::vect(unq_locs_sf), list = F)

rhum_trials <- merge(x = unq_locs_sf, y = rhum_trials, by.x = "id", by.y = "ID")
colnames(rhum_trials)


colnames(rhum_trials)[10:21] <- month.abb#c("id", month.abb)

rhum_trials_df <- as.data.frame(rhum_trials)

sel_cols <- c("id", "country", "location_name", "study", month.abb)

rhum_trials_df <- rhum_trials_df[sel_cols]


which(duplicated(rhum_trials_df$location_name))

rhum_trials_df_l <- melt(rhum_trials_df, 
                         id.vars = c("location_name"), 
                         measure.vars = month.abb)
rhum_trials_df_l

rhum_p <- ggplot() + 
  geom_line(data = rhum_trials_df_l, aes(x = variable,
                                         y = value,
                                         col = as.factor(location_name),
                                         group = as.factor(location_name)
  ), size = 1.35) + 
  # geom_point(data = rhum_trials_df_l, aes(x = variable,
  #                                         y = value,
  #                                         col = as.factor(id))) +
  labs(x = "",
       y = "%",
       title = "Average relative humidity 1990 - 2020",
       color = "Location name")

rhum_p



(temp_p / prec_p / rhum_p) + plot_layout(guides = 'collect') +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

ggsave("output/figures/trials_clim.png", dpi = 600, width = 18, height = 10)

trials_p |  (temp_p / prec_p / rhum_p) + plot_layout(guides = 'collect',
                                                     ncol = 2,
                                                     #heights = c(2, 1, 1, 1),
                                                     widths = c(1,2,2,2))


trials_p #/  (temp_p / prec_p / rhum_p) + plot_layout(guides = 'collect')

ggsave("output/figures/trials_locations.png", dpi = 600, width = 16, height = 12)


#############################################################

temp_mean <- terra::mean(temp_stack)

temp_mean <- temp_mean - 273.15







#terra::plot(temp_mean)

#transform from 360 to 180
#https://confluence.ecmwf.int/display/CKB/ERA5%3A+What+is+the+spatial+reference
temp_mean <- terra::rotate(temp_mean)

temp_mean <- terra::mask(temp_mean, mask = terra::vect(World))

temp_mean_df <- terra::as.data.frame(x = temp_mean, xy = TRUE)

temp_map <- ggplot() + 
  geom_raster(data = temp_mean_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = World, fill = NA) + 
  geom_sf(data = musa_trials_time_loc_sf, color = "red") +
  coord_sf(xlim = c(-155, 155), ylim = c(-55, 80), expand = T) +
  scale_fill_distiller(palette = "Spectral") +
  #scale_fill_viridis_c(option = "D") +
  labs(fill = "Temperature (Â°C)", y = "lat") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank() )

temp_map

ggsave("output/figures/musa_trial_locations_temp.png", dpi = 300, width = 16, height = 12)

#Precipitation
prec_files <- list.files("data/ecv_ERA5/prec/", full.names = TRUE)

prec_stack <- terra::rast(prec_files)

prec_mean <- terra::mean(prec_stack)

#meters to mm
prec_mean <- prec_mean * 1000

prec_mean <- terra::rotate(prec_mean)

prec_mean <- terra::mask(prec_mean, mask = terra::vect(World))

#terra::plot(prec_mean)

prec_mean_df <- terra::as.data.frame(x = prec_mean, xy = TRUE)

#hist(prec_mean_df$mean)

prec_map <- ggplot() + 
  geom_raster(data = prec_mean_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = World, fill = NA) + 
  geom_sf(data = musa_trials_time_loc_sf, color = "red") +
  coord_sf(xlim = c(-155, 155), ylim = c(-55, 80), expand = T) +
  #scale_fill_continuous_diverging(palette = "Vik", mid = 50, rev = TRUE) +
  scale_fill_continuous_sequential(palette = "Blue-Yellow", rev = F) +
  #scale_fill_distiller(palette = "Blues", direction = -1, begin = 20) + 
  labs(fill = "Precipitation (mm)", y = "lat") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank() )

prec_map

ggsave("output/figures/musa_trial_locations_prec.png", dpi = 300, width = 16, height = 12)


#relative humidity
rhum_files <- list.files("data/ecv_ERA5/rhum/", full.names = TRUE)

rhum_stack <- terra::rast(rhum_files)

rhum_mean <- terra::mean(rhum_stack)

rhum_mean <- terra::rotate(rhum_mean)

rhum_mean <- terra::mask(rhum_mean, mask = terra::vect(World))

rhum_mean_df <- terra::as.data.frame(x = rhum_mean, xy = TRUE)

rhum_map <- ggplot() + 
  geom_raster(data = rhum_mean_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = World, fill = NA) + 
  geom_sf(data = musa_trials_time_loc_sf, aes(color = "Trials")) +
  coord_sf(xlim = c(-155, 155), ylim = c(-55, 80), expand = T) +
  scale_fill_viridis_c(option = "E", direction = -1) + 
  labs(fill = "Relative humidity (%)", x = "long", y = "lat")

rhum_map

ggsave("output/figures/musa_trial_locations_rhum.png", dpi = 300, width = 16, height = 12)


temp_map / prec_map / rhum_map + plot_annotation(tag_levels = "a")

hcl_palettes(plot = T)

ggsave("output/figures/musa_trial_locations_climate.png", dpi = 600, width = 14, height = 16)


palette.colors()


