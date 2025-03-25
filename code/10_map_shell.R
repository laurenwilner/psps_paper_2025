#-------------------------------------------------
# PSPS: ZCTA map shell
# March 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse, sf, tigris, MetBrewer, leaflet)

# load data -------------------------------------------------
zctas <- c(90001:90008, 90011:90041, 94102:94158) # FILL IN WITH ZCTAS FROM HCAI! 
ca_shp <- tigris::states(cb = TRUE, year = 2020) %>% 
    filter(STUSPS == "CA") %>% 
    select(geometry)  %>% 
    st_transform(epsg = 3310)
zcta_shp <- tigris::zctas(cb = TRUE, year = 2020) %>% 
    rename(zcta = ZCTA5CE20) %>%
    st_transform(epsg = 3310) %>% 
    select(zcta, geometry) %>% 
    # filter to those that intersect with CA
    st_intersection(ca_shp) %>%
    select(zcta, geometry) %>% 
    mutate(fill_flag = zcta %in% zctas)

# plot -------------------------------------------------
pal <- met.brewer(name = "Hokusai2", n=5)
color_mapping <- c(`TRUE` = pal[3], `FALSE` = "white")

ggplot() +
  geom_sf(data = ca_shp, fill = "white", color = alpha("black", 0.2), stroke = 0.1) +
  geom_sf(data = zcta_shp, aes(fill = fill_flag), color = alpha("black", 0.2), stroke = 0.1) +
  scale_fill_manual(values = color_mapping) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

  


