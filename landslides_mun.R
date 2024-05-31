# This code makes a map of landslides in Colombia using the UNGRD database 1998-2022

# Load libraries ------
libs <- c(
  "here", # file path management
  "sf", # vector data operations
  "dplyr", # data wrangling
  "lubridate", # Date object handling
  "ggplot2", # plotting
  "ggthemes", # ggplot2 themes
  "patchwork" # ggplot2 patchwork
)

# install missing libraries 
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}
  
# Load libraries
invisible(lapply(libs, library, character.only = T))

# Load data ------
#load the following csv file: landslides.csv
landslides <- read.csv(
"/Users/couleauanabelle/Library/CloudStorage/Dropbox/Data_Science/01_Portfolio/Landslides_Colombia/landslides.csv")
head(landslides)
tail(landslides)

# sum number of landslides by muncipalities between 1998 and 2022
landslides_mun <- landslides %>%
  group_by(cod_mun) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
landslides_mun

landslides_mun = landslides_mun %>% 
  rename(
    ADM2_PCODE = cod_mun,
    landslides_num = n
  )

# load colombia municipality shp file boundaries. Source: https://data.humdata.org/dataset/cod-ab-col?
colombia_mun <- st_read(
  dsn = "/Users/couleauanabelle/Library/CloudStorage/Dropbox/Data_Science/01_Portfolio/Landslides_Colombia/col-administrative-divisions-shapefiles/col_admbndp_admALL_mgn_itos_20200416.shx"
)
class(colombia_mun)
head(colombia_mun)

colombia_poly <- st_read(
  dsn = "/Users/couleauanabelle/Library/CloudStorage/Dropbox/Data_Science/01_Portfolio/Landslides_Colombia/col-administrative-divisions-shapefiles/col_admbnda_adm2_mgn_20200416.shx"
)
class(colombia_poly)
head(colombia_poly)



# Check CRS
st_crs(colombia_poly) 


# convert admin code to numeric to merge with landslide databse
colombia_poly$ADM2_PCODE = as.numeric(substring(colombia_poly$ADM2_PCODE, 3,7))

# merge
colombia_landslides <- left_join(colombia_poly, landslides_mun, by = "ADM2_PCODE")


# replace colombia_landslides$landslides_num NA values with 0
colombia_landslides$landslides_num[is.na(colombia_landslides$landslides_num)] <- 0

# plot landslides from colombia_landslides$geometry using ggplot

pmap <- ggplot(colombia_landslides$geometry) +
  geom_sf() +
  geom_sf(aes(fill = colombia_landslides$landslides_num), lwd = 0.05, color = "white") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(5, 180, 20),
    direction = 1,
    palette = "YlGnBu"
  ) +
  labs(
    title = "Landslides in Colombia 1998-2022",
    subtitle = "Number of landslides",
    caption = "Source: UNGRD, author's calculations"
  ) +
  theme_map() +
  theme(
    # Legend
    legend.position = "top",
    legend.justification = 0.5,
    legend.key.size = unit(1.25, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 12),
    legend.margin = margin(),
    # Increase size and horizontal alignment of the both the title and subtitle
    plot.title = element_text(size = 28, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

pmap

# Save plot
ggsave(
  filename = here("landslides_mun.png"),
  plot = pmap,
  width = 12,
  height = 12,
  dpi = 300
)


# do the same but for landslides only affecting roads: 
# This code makes a map of landslides in Colombia using the UNGRD database 1998-2022

# Load libraries ------
libs <- c(
  "here", # file path management
  "sf", # vector data operations
  "dplyr", # data wrangling
  "lubridate", # Date object handling
  "ggplot2", # plotting
  "ggthemes", # ggplot2 themes
  "patchwork" # ggplot2 patchwork
)

# install missing libraries 
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# Load libraries
invisible(lapply(libs, library, character.only = T))

# Load data ------
#load the following csv file: landslides_roads.csv
landslides_roads <- read.csv(
  "/Users/couleauanabelle/Library/CloudStorage/Dropbox/Data_Science/01_Portfolio/Landslides_Colombia/landslides_road.csv")
head(landslides_roads)
tail(landslides_roads)

# sum number of landslides by muncipalities between 1998 and 2022
landslides_mun <- landslides_roads %>%
  group_by(cod_mun) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
landslides_mun

landslides_mun = landslides_mun %>% 
  rename(
    ADM2_PCODE = cod_mun,
    landslides_num = n
  )

# load colombia municipality shp file boundaries. Source: https://data.humdata.org/dataset/cod-ab-col?
colombia_mun <- st_read(
  dsn = "/Users/couleauanabelle/Library/CloudStorage/Dropbox/Data_Science/01_Portfolio/Landslides_Colombia/col-administrative-divisions-shapefiles/col_admbndp_admALL_mgn_itos_20200416.shx"
)
class(colombia_mun)
head(colombia_mun)

colombia_poly <- st_read(
  dsn = "/Users/couleauanabelle/Library/CloudStorage/Dropbox/Data_Science/01_Portfolio/Landslides_Colombia/col-administrative-divisions-shapefiles/col_admbnda_adm2_mgn_20200416.shx"
)
class(colombia_poly)
head(colombia_poly)



# Check CRS
st_crs(colombia_poly) 


# convert admin code to numeric to merge with landslide databse
colombia_poly$ADM2_PCODE = as.numeric(substring(colombia_poly$ADM2_PCODE, 3,7))

# merge
colombia_landslides <- left_join(colombia_poly, landslides_mun, by = "ADM2_PCODE")


# replace colombia_landslides$landslides_num NA values with 0
colombia_landslides$landslides_num[is.na(colombia_landslides$landslides_num)] <- 0

# plot landslides from colombia_landslides$geometry using ggplot

pmap <- ggplot(colombia_landslides$geometry) +
  geom_sf() +
  geom_sf(aes(fill = colombia_landslides$landslides_num), lwd = 0.05, color = "white") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(5, 45, 5),
    direction = 1,
    palette = "YlGnBu"
  ) +
  labs(
    title = "Landslides (on roads) in Colombia 1998-2022",
    subtitle = "Number of landslides affecting roads only",
    caption = "Source: UNGRD, author's calculations"
  ) +
  theme_map() +
  theme(
    # Legend
    legend.position = "top",
    legend.justification = 0.5,
    legend.key.size = unit(1.25, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 12),
    legend.margin = margin(),
    # Increase size and horizontal alignment of the both the title and subtitle
    plot.title = element_text(size = 28, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

pmap

# Save plot
ggsave(
  filename = here("landslides_mun_roads.png"),
  plot = pmap,
  width = 12,
  height = 12,
  dpi = 300
)

