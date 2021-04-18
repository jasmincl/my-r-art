library(dplyr)
library(ggplot2)
library(jasmines)
library(colortools)
library(viridis)
library(scales)
# install.packages('devtools')
library(devtools)
(devtools::install_github("cutterkom/generativeart"))
install.packages(c("mapproj", "ggforce", "Rcpp"))
devtools::install_github("marcusvolz/mathart")
devtools::install_github("marcusvolz/ggart")

# create palette -----------------
jazzy_colors <- c(
  
"#a34814"
,"#c86932"
,"#ffc784"
,"#ffddaa"
,"#fffff0" # ivory
# ,"#c7cb82" # lint (heller)
,"#a7b688" 
,"#6C6C6C"
,"#28382b"
,"#455B46"
,"#013a20" # forest green
,"#0B3B24"
,"#0A2A22"
,"#071910"
,"#0A1B2A"

                  )
show_col(jazzy_colors)


analogous("#B8C76F")
analogous("#B8C76F")
analogous("#68372B")
analogous("#70A4B2")
analogous("#588D43")
analogous("#352879")
analogous("#6F4F25")
analogous("#433900")
analogous("#9A6759")
analogous("#6C6C6C")
analogous("#9AD284")
analogous("#6C5EB5")

## orange
analogous("#F7631C")
analogous("#ED5005")
analogous("#c86932")

## dunkelgrün
analogous("#2a312a")
analogous("#28382b")
analogous("#455B46")
analogous("#013a20") # forest green
analogous("#bacc81") # lint (hell)
analogous("#cdd193") # lint (heller)


analogous("#0A2A22")
analogous("#071910")
analogous("#0B3B24")
analogous("#0A1B2A")
complementary("#0A2A22")
complementary("#071910")
complementary("#0B3B24")
complementary("#0A1B2A")
analogous("")
analogous("")
analogous("")

wheel("#0A2A22", num = 12)
sequential("#071910")
sequential("#0B3B24")
sequential("#0A1B2A")

splitComp("#0A2A22")
tetradic("#0A2A22")
square("#0A2A22")


# create art
use_seed(1) %>%
  entity_circle(grain = 500) %>%
  unfold_tempest(iterations = 10) %>%
  style_ribbon(background = "oldlace",  
               palette = palette_manual(jazzy_colors) )


jasmine2_colors <- c(
  
  "#a34814" # orange
  ,"#c86932"
  ,"#ffc784"
  ,"#ffddaa"
  ,"#fffff0" # ivory
  # ,"#c7cb82" # lint (heller)
  #,"#a7b688" # hellgrün
  #,"#6C6C6C" grau
  ,"#28382b"
  ,"#455B46"
  ,"#013a20" # forest green
  ,"#0B3B24"
  #,"#0A2A22" # blaugrün
  ,"#071910"
  #,"#0A1B2A" # blue
  
) 
jasmine2_colors <- data.frame("mycolors" = c(
  
  "#a34814" # orange
  ,"#0A2A22"
  ) )
show_col(jasmine2_colors)



# mathart ####
library(mathart)
library(ggart)
library(ggforce)
library(Rcpp)
library(tidyverse)


df3 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.039, d2 = 0.006, d3 = 0, d4 = 0.0045,
                    f1 = 10, f2 = 3, f3 = 1, f4 = 2,
                    p1 = 0, p2 = 0, p3 = pi/2, p4 = 0) %>% mutate(id = 3)


# df <- rbind(df1, df2, df3, df4)

ggplot() +
  geom_path(aes(x, y), df3, alpha = 0.9, size = 0.2, col = "#a34814") +
  coord_equal() +
  # facet_wrap(~id, nrow = 2) +
  theme_blankcanvas(margin_cm = 0)



df2 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.0085, d2 = 0, d3 = 0.065, d4 = 0,
                    f1 = 2.01, f2 = 3, f3 = 3, f4 = 2,
                    p1 = 0, p2 = 7*pi/16, p3 = 0, p4 = 0) %>% mutate(id = 2)


ggplot() +
  geom_path(aes(x, y), df2, alpha = 0.9, size = 0.2, col = "#0B3B24") +
  coord_equal() +
  # facet_wrap(~id, nrow = 2) +
  theme_blankcanvas(margin_cm = 0)





use_seed(1) %>%
  scene_discs(
    rings = 3, points = 4000, size = 1
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 1,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 20,
    scale = .01
  ) %>%
  style_ribbon(
    palette = palette_manual(),
    colour = "mycolors",
    alpha = c(.1,.1),
    background = "oldlace"
  )


## Karte Uhlandstraße zu G7
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                    "ggspatial", "rnaturalearth", "rnaturalearthdata"))
# install.packages("geosphere")
# install.packages("sf")
# install.packages("osrm")
# install.packages("osmdata")
# install.packages("eurostat")
# install.packages("lwgeom")
library(sf)
library(lwgeom)
library(eurostat)  # eurostat data
library(osrm)

library(geosphere)
monnem <- data.frame("name" = c("G7", "Uhland"),
                        "lat" =c(49.492646,49.496237),
                        "lon"=c(8.459733,8.481518))

monnem_sf <- st_as_sf(monnem, coords=c("lon", "lat"),crs=4258)
route <- osrmRoute(src=monnem_sf[1,],dst=monnem_sf[2,],
                   overview = "full", returnclass = "sf")

# city data
library(osmdata)

min_lon <- 8.453531; max_lon <- 8.486018
min_lat <- 49.481948; max_lat <- 49.497643
bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
colnames(bbx) <- c("min","max")

highways <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

streets <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

river <- bbx %>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()



ggplot() + 
  geom_sf(data = river)

river

color_roads <- rgb(0.42,0.449,0.488)
ggplot() +
  geom_sf(data = streets$osm_lines,
          col = "#0B3B24",
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          col = "#0B3B24",
          size = .6,
          alpha = .8)+
  # geom_sf(data=river, 
  #         col="blue") +
  coord_sf(xlim = c(min_lon,max_lon),
           ylim = c(min_lat,max_lat),
           expand = FALSE)+
  theme(legend.position = F) + theme_void() +
  geom_sf(data = route, size = 1.2, col = "#a34814") +
  geom_sf(data=monnem_sf, col = "#0B3B24")

#a34814
#0B3B24
# Puntke aller Wohnorte


addressen <- data.frame("name" = c("g7", "uhland", "berlin_moabit", "hamburg_barmbek", "hamburg_stgeorg","arlington", "prag", "exeter", "konstanz"),
                        "lat" =c(49.492646,49.496237,52.523511,53.587013,53.556130,38.888619,50.074115,50.714608,47.681011),
                        "lon"=c(8.459733,8.481518,13.336907,10.057730,10.007014,-77.147468,14.475896,-3.533535, 9.156252),
                        "dauer" = c(38,50,4,5,5,12,7,4,31))


#NUTS2 regions
eu_nuts2_sf <- get_eurostat_geospatial(output_class = 'sf',
                                       resolution = '60', nuts_level = 2)
#NUTS1 regions
eu_nuts1_sf <- get_eurostat_geospatial(output_class = 'sf',
                                       resolution = '60', nuts_level = 1)

#Replace NUTS2 regions with NUTS1 for the countries that don't have data on NUTS2 level
nuts2.filt <- filter(eu_nuts2_sf, !(CNTR_CODE %in% c("FR", "DE", "UK")))
nuts1.filt <- filter(eu_nuts1_sf, CNTR_CODE %in% c("FR", "DE", "UK"))
nuts <- rbind(nuts2.filt, nuts1.filt)
nuts

addressen %>% 
  filter(name != "arlington" & name != "hamburg_stgeorg") %>% 
ggplot(aes(lon,lat,label=name)) +
  geom_point(aes(size=dauer)) +
  geom_text(aes(label = name))

asf <- st_as_sf(addressen, coords=c("lon", "lat"),crs=4258)
asf <- asf %>% 
  filter(name != "arlington" & name != "hamburg_stgeorg") 



ggplot(data = addressen) +
  geom_sf(data = nuts, fill="grey98",color=NA) +
  # geom_text(aes(lon, lat, l
  geom_point(aes(lon,lat,size=dauer))
