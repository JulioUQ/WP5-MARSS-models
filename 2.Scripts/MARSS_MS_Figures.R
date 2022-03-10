## ---------------------------
##
## Script name: MARSS_MS_FIGURES.
##
## Purpose of script: Visualize time-series and mapping surveys areas 
##
## Author: Julio Ubeda Quesada // Aristotle University of Thessaloniki
## Email: julioubedaquesada@gmail.com
##
## Date Created: 2022-02-01
##
##--------------Here I am working in with relative paths, refering to sub-directories in my main directory (home_file)

home_file  = getwd()
data_loc   = paste0(home_file,"/1.Data/")
fig_loc    = paste0(home_file,"/3.Figures_Graphs/")
Output_loc = paste0(home_file, "/4.Outputs/")

#> Libraries
Packages <- c("tidyverse", "ggpubr", "sf", "ggOceanMaps", "ggnewscale", "sfheaders")
lapply(Packages, library, character.only = TRUE)

rm(list=ls())

#> CATCH DATA
catch = data.frame(read.table("~/Desktop/WP5- MARSS model/3.Figures_Graphs /Covariates/Commercial catches/Cov_catch.csv", 
                              header=TRUE, sep = ";"))
catch$year = 1996:2019
head(catch)
catch <- catch %>% rename("Northwest_Atlantic_Stock" = "DivA0.ABCD1",
                          "West_Nordic_Stock" = "Area5a.5b.12.14b",
                          "Northeast_Arctic_Stock" ="Subarea1.2")

## 1. COMMERCIAL CATCHES PLOT
catch_plot <- ggplot(catch, aes(x = year)) +
  
  geom_point(aes(y = Northwest_Atlantic_Stock, colour = "Northwest_Atlantic_Stock"), size = 2) +
  geom_line(aes(y = Northwest_Atlantic_Stock, colour = "Northwest_Atlantic_Stock"), size = 1.5) + 
  geom_point(aes(y = West_Nordic_Stock, colour = "West_Nordic_Stock"), size = 2) +
  geom_line(aes(y = West_Nordic_Stock, colour = "West_Nordic_Stock"), size = 1.5) + 
  geom_point(aes(y = Northeast_Arctic_Stock, colour = "Northeast_Arctic_Stock"), size = 2) +
  geom_line(aes(y = Northeast_Arctic_Stock, colour = "Northeast_Arctic_Stock"), size = 1.5) + 
  
  labs(y = "Nominal catch (tons)", x = "Years", face = "bold") +
  scale_x_continuous(breaks = 1996:2019) +
  theme(legend.position = c(.5, .93),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(colour = "black", size = 30, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(t =10)),
        axis.text.x = element_text(colour = "black", size = 25, angle = 90, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y.left = element_text(colour = "black", size = 30, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.y.left = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"))

catch_plot + scale_color_manual(labels = c("NEAS", "NWAS", "WNS"),
                              values = c("red", "blue", "green"))

setwd("~/Desktop/WP5- MARSS model/3.Figures_Graphs /Covariates/Commercial catches")
ggsave("Commercial catches of GHL in North Atlantic_1996-2019.png", dpi = 300, units = "cm", height = 25, width = 40) 

## 2. NAO INDEX DATA
rm(list = ls())

nao <- read_csv("Cov_NAO.csv")
nao$year <- 1996:2019
head(nao, 3)

nao1 <- mutate(nao, type = case_when(
  NAO < 0 ~ "NEGATIVE",
  NAO > 0 ~ "POSITIVE"
))

nao_plot <- ggplot(data=nao1, aes(year, NAO)) +
  geom_bar(stat="identity", aes(fill=type)) +
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
  scale_fill_manual(values = c("POSITIVE"="red", "NEGATIVE"="deepskyblue3")) +
  guides(fill="none") + 
  xlab("Year") + ylab("NAO index") +
  scale_x_continuous(breaks=1996:2019) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.title.x = element_text(colour = "black", size = 30, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.text.x = element_text(colour = "black", size = 25, angle = 90, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y = element_text(colour = "black", size = 30, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.y = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"))
nao_plot

setwd("/Users/macbookairjulio/Desktop/MARSS model/MS Figures")
ggsave("NAO_index_plot.png", dpi = 300, units = "cm", height = 20, width = 40)

## 3. BIOMASS AND ABUNDANCE DATA 
rm(list=ls())

ice_a = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/Iceland Abundance times-series.csv') 
egr_a = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/East Greenland Abundance times-series.csv')
wgr_a = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/West Greenland abundance times-series.csv') 
nas_a = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/Norway slope Abundance times-series.csv') 

ice_b = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/Iceland Biomass times-series.csv')
egr_b = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/East Greenland Biomass times-series.csv') 
wgr_b = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/Greenland & Canada combined biomass times-series.csv') 
nas_b = read_csv('/Users/macbookairjulio/Documents/MARSS models/3. Figures_Graphs/Abundance and Biomass trends/Norway slope Biomass times-series.csv')

#> ICELAND
Ice = merge(ice_a, ice_b, by = "year")
Ice[16, c("N","B")] = 0
Ice[Ice == 0] <- NA

# PLOT ICELAND
ICE_plot <- ggplot(Ice, aes(x = year)) +
  geom_point(aes(y = N, colour = "Abundance"), size = 2) +
  geom_point(aes(y = B, colour = "Biomass"), size = 2) +
  geom_line(aes(y = N, colour = "Abundance"), size = 1.5) + 
  geom_line(aes(y = B, colour = "Biomass"), size = 1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Biomass (tons)",breaks = c(15000, 30000, 45000, 60000)), breaks = c(15000, 30000, 45000, 60000)) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Abundance ('000s)", x = "Years", colour = "Index", face = "bold") +
  scale_x_continuous(breaks = 1996:2019) +
  ggtitle("C)") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key = element_blank(),
        legend.position = c(.5, .93),
        plot.title = element_text(size = 20, vjust = -10, hjust = .05),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"))
ICE_plot

#> EAST GREENLAND
Egr = merge(egr_a, egr_b, by = "year")
Egr_1 <- select(Egr, -Region)
Egr_1[Egr_1 == 0] <- NA

# PLOT EAST GREENLAND
EGR_plot <- ggplot(Egr_1, aes(x = year)) +
  geom_point(aes(y = N, colour = "Abundance"), size = 2) +
  geom_point(aes(y = B, colour = "Biomass"), size = 2) +
  geom_line(aes(y = N, colour = "Abundance"), size = 1.5) + 
  geom_line(aes(y = B, colour = "Biomass"), size = 1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Biomass (tons)",breaks = c(6000, 12000, 18000, 24000)), breaks = c(6000, 12000, 18000, 24000)) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Abundance ('000s)", x = "Years", colour = "Index", face = "bold") +
  scale_x_continuous(breaks = 1996:2019) +
  ggtitle("B)") +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = c(.5, .93),
        plot.title=element_text(size=20, vjust = -10, hjust = .05),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_text(colour = "black", size = 25, angle = 0, hjust = 0, vjust = 0, face = "bold"),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"))
EGR_plot

#> WEST GREENLAND
Wgr_1 <- merge(wgr_a, wgr_b, by = "year")
Wgr_1 <- select(Wgr_1, -Region.x, -Region.y)
Wgr_1[Wgr_1 == 0] <- NA

# PLOT WEST GREENLAND
WGR_plot <- ggplot(Wgr_1, aes(x = year)) +
  geom_point(aes(y = N, colour = "Abundance"), size = 2) +
  geom_point(aes(y = B, colour = "Biomass"), size = 2) +
  geom_line(aes(y = N, colour = "Abundance"), size = 1.5) + 
  geom_line(aes(y = B, colour = "Biomass"), size = 1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Biomass (tonnes)")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Abundance ('000s)", x = "Years", colour = "Index", face = "bold") +
  ggtitle("A)") +
  scale_x_continuous(breaks = 1996:2019) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key = element_blank(),
        legend.position = c(.5, .93),
        plot.title=element_text(size=20, vjust = -10, hjust = .05),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"))
WGR_plot

#> NORWEGIAN SLOPE SURVEY
Nas <- merge(nas_a, nas_b)
Nas[Nas == 0] <- NA

# PLOT NORWEGIAN SLOPE
NAS_plot <- ggplot(Nas, aes(x = year)) +
  geom_point(aes(y = N, colour = "Abundance"), size = 2) +
  geom_point(aes(y = B, colour = "Biomass"), size = 2) +
  geom_line(aes(y = N, colour = "Abundance"), size = 1.5) + 
  geom_line(aes(y = B, colour = "Biomass"), size = 1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Biomass (tonnes)",breaks = c(25000, 50000, 75000, 100000)), breaks = c(25000, 50000, 75000, 100000)) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Abundance ('000s)", x = "Years", colour = "Index", face = "bold") +
  ggtitle("D)") +
  scale_x_continuous(breaks = 1996:2019) +
  theme(legend.text = element_text(size = 10),
        legend.position = c(.5, .93),
        legend.key = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(colour = "black", size = 30, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.text.x = element_text(colour = "black", size = 25, angle = 90, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_text(colour = "black", size = 25, angle = 0, hjust = .5, vjust = 0, face = "bold"))
NAS_plot

plot_1 <- ggarrange(WGR_plot, EGR_plot, ICE_plot, NAS_plot,
                    nrow = 4, 
                    common.legend = TRUE)                           
plot_1 

setwd("/Users/macbookairjulio/Desktop/MARSS model/MS Figures")
ggsave("Greenland halibut Abundance and Biomass trends in the North Atlantic_plot.png", dpi = 300, units = "cm", height = 60, width = 40) 

## 3. MAPPING SURVEY AREAS WITH ggOceanMaps
rm(list=ls())

ca <- read_sf("~/Desktop/WP5- MARSS model/1.Data/CAN GIS 0A sampling survey/NAFO_0A_Index_Strata_Julio.shp")
en <- read_sf("~/Desktop/WP5- MARSS model/1.Data/NOR GIS continental slope survey/EggaN_strata_for_Julio.shp")
ice_egr <- read.csv("~/Desktop/WP5- MARSS model/1.Data/ICE GIS sampling survey/iceland_greenland_strata.csv") %>% 
  arrange(stratum, order) %>%
  sfheaders::sf_polygon(., x = "lon", y = "lat", polygon_id = "stratum") %>%
  st_set_crs(4326)

head(ca)
head(en)
head(ice_egr)

# Turn off the s2 mode
s2_mode <- sf::sf_use_s2()
suppressMessages(sf::sf_use_s2(FALSE))

#> CANADA SURVEY AREA MAP
x1 <- st_union(ca) %>%
  st_make_valid() %>%
  st_as_sf() %>%
  # smoothr::drop_crumbs(., units::set_units(1e4, "km^2", mode = "standard")) %>% # remove small fragments
  nngeo::st_remove_holes() %>% # to remove holes
  add_column(survey = "Canadian 0A")  %>%  
  rename("x" = "geom") %>%
  suppressMessages() %>%
  suppressWarnings()

ggOceanMaps::qmap(x1, fill = I("red"), alpha = I(0.5), rotate = TRUE, labs(fill= "survey")) # requires the newest version of ggOceanMaps

#> NORWAY SURVEY AREA MAP
x2 <- sf::st_union(en) %>%
  sf::st_make_valid() %>%
  sf::st_as_sf() %>%
  tibble::add_column(survey = "Norwegian Slope") %>% 
  suppressMessages()

ggOceanMaps::qmap(x2, fill = I("blue"), alpha = I(0.5), rotate = TRUE, labs(fill= "survey")) # requires the newest version of ggOceanMaps

#> ICELAND SURVEY AREA MAP
ice1 <- ice_egr[ice_egr$stratum %in% c(9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28), ]

x3 <- sf::st_make_valid(ice1) %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  smoothr::drop_crumbs(., units::set_units(1e4, "km^2", mode = "standard")) %>% # remove small fragments
  nngeo::st_remove_holes() %>% # to remove holes
  tibble::add_column(survey = "Iceland") %>%
  rename("x" = "geom") %>%
  suppressMessages() %>%
  suppressWarnings()

ggOceanMaps::qmap(x3, fill = I("green"), alpha = I(0.5), rotate = TRUE, labs(fill= "survey")) # requires the newest version of ggOceanMaps

#> WEST GREENLAND SURVEY AREA MAP
devtools::install_github("DeepWaterIMR/RstoxStrata")
library(RstoxStrata)

link <- "~/Desktop/gebco_2021/GEBCO_2021.nc" # Link to bathymetry data

geostrata.df <- data.frame(
  lon.min = c(-58, -58),
  lon.max = c(-51, -51),
  lat.min = c(64.2, 62.4),
  lat.max = c(66.2, 64.2)
)

depths.vec <- c(400, 1500)

boundary.vec <- c(-58, -51, 62, 67)

pols <- strataPolygon( ## In case you have problems obtaining this object, you can find it in the repository
  bathy = link,
  depths = depths.vec,
  boundary = boundary.vec,
  geostrata = geostrata.df,
  fragment.area = 1000
)

x4 <- sf::st_union(pols$strata) %>%
  sf::st_make_valid() %>%
  sf::st_as_sf() %>%
  tibble::add_column(survey = "West Greenland") %>% 
  suppressMessages()

ggOceanMaps::qmap(x4, fill = I("purple"), alpha = I(0.5), rotate = TRUE, labs(fill= "survey")) # requires the newest version of ggOceanMaps

#> EAST GREENLAND SURVEY AREA MAP
egr1 <- ice_egr[ice_egr$stratum %in% c(1, 2, 3, 4, 5, 6, 7, 8), ]

x5 <- sf::st_make_valid(egr1) %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  #smoothr::drop_crumbs(., units::set_units(1e4, "km^2", mode = "standard")) %>% # remove small fragments
  #nngeo::st_remove_holes() %>% # to remove holes
  tibble::add_column(survey = "East Greenland") %>%
  #rename("x" = "geom") %>%
  suppressMessages() %>%
  suppressWarnings()

ggOceanMaps::qmap(x5, fill = I("yellow"), alpha = I(0.5), rotate = TRUE, labs(fill= "survey"))

#> Combining Canadian and West Greenland survey
x_WGR_CAN <-rbind(x1, x4)
x_WGR_CAN[c(1,2), "survey"] = "West Greenland and Canada"
#> Combine the survey sampling in a spatial file
x_combined <- rbind(x2, x3, x5, x_WGR_CAN)
class(x_combined)

#> MAP WITH ALL SURVEY AREAS 
survey_plot <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                     bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top",
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(t =5)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 5)),
        axis.text.y = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))

survey_plot + annotation_scale(location = "br")  
ggsave("survey_plot.png", dpi = 300, units = "cm", height = 20, width = 40)

#> HYPOTHESES PLOTS (1--->9)
#> Hipotheses 1: (1) EG-ICE-WGR-NEA
x_combined[c(1,2,3,4,5), "survey"] = "Panmixia"

H1 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                                    bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 5)),
        axis.text.y = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))
H1 <- H1 + scale_fill_discrete(name = "Hypothesis 1", labels = "Panmixia")

#> Hypothesis 2: (1) EG-ICE (2) WGR (3) NEA
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(3,5), "survey"] = "EG-ICE"
x_combined[c(1,4), "survey"] = "WGR"
x_combined[c(2), "survey"] = "NEA"

H2 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
H2 <- H2 + scale_fill_discrete(name = "Hypothesis 2")

#> Hypothesis 3: (1) EG (2) ICE-NEA (3) WGR
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(5), "survey"] = "EG"
x_combined[c(2,3), "survey"] = "ICE-NEA"
x_combined[c(1,4), "survey"] = "WGR"

H3 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
H3 <- H3 + scale_fill_discrete(name = "Hypothesis 3")

#> Hypothesis 4: (1) EG (2) ICE-WGR (3) NEA
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(5), "survey"] = "EG"
x_combined[c(1,4,3), "survey"] = "ICE-WGR"
x_combined[c(2), "survey"] = "NEA"

H4 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 5)),
        axis.text.y = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))
H4 <- H4 + scale_fill_discrete(name = "Hypothesis 4")

#> Hypothesis 5: (1) EG-ICE-NEA (2) WGR
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(2,3,5), "survey"] = "EG-ICE-NEA"
x_combined[c(1,4), "survey"] = "WGR"

H5 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
H5 <- H5 + scale_fill_discrete(name = "Hypothesis 5")

#> Hypothesis 6: (1) EG-NEA (2) ICE (3) WGR
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(2,5), "survey"] = "EG-NEA"
x_combined[c(3), "survey"] = "ICE"
x_combined[c(1,4), "survey"] = "WGR"

H6 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
H6 <- H6 + scale_fill_discrete(name = "Hypothesis 6")

#> Hypothesis 7: (1) EG-WGR (2) ICE-NEA
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(1,4,5), "survey"] = "EG-WGR"
x_combined[c(2,3), "survey"] = "ICE-NEA"

H7 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 5)),
        axis.text.y = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))
H7 <- H7 + scale_fill_discrete(name = "Hypothesis 7")

#> Hypothesis 8: (1) EG-NEA (2) ICE-WGR
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(2,5), "survey"] = "EG-NEA"
x_combined[c(1,4,3), "survey"] = "ICE-WGR"

H8 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
H8 <- H8 + scale_fill_discrete(name = "Hypothesis 8")

#> Hypothesis 8: (1) EG (2) ICE (3) WGR (4) NEA
x_combined <- rbind(x1, x2, x3, x4, x5)
x_combined[c(5), "survey"] = "EG"
x_combined[c(3), "survey"] = "ICE"
x_combined[c(1,4), "survey"] = "WGR"
x_combined[c(2), "survey"] = "NEA"

H9 <- ggOceanMaps::basemap(limits = sf::st_bbox(x_combined)[c("xmin", "xmax", "ymin", "ymax")], rotate = FALSE,
                           bathymetry = TRUE, bathy.style = "poly_greys", land.col = "#eeeac4", legends = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = x_combined, aes(fill = survey), alpha = 0.8, color = NA) +
  labs(fill = "Survey", color = "Survey") +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
H9 <- H9 + scale_fill_discrete(name = "Hypothesis 9") + annotation_scale(location = "br")  

All_Hipotheses <- ggarrange(H1, H2, H3, 
                            H4, H5, H6,
                            H7, H8, H9,
                    ncol = 3, nrow = 3)
ggsave("All_Hipotheses.png", dpi = 300, units = "cm", height = 20, width = 40)
getwd()

## 4. BEST-FIT MODEL STATE PROCESSES 
rm(list=(ls()))

#> BEST-FIT MODEL WITH BIOMASS DATA

bmod_bio <- readRDS("run_152.rds")
states1 <- bmod_bio$states
states.se1 <- bmod_bio$states.se
states.df <- melt(states1)
states.se.df <-melt(states.se1)

colnames(states.df) <- c("States","Year","log.bio")
colnames(states.se.df) <- c("states","year","se")

states.df$States <- as.character(states.df$States)
states.df$Year <- as.character(states.df$Year)
df1 <- cbind(states.df, states.se.df)
df2 <-select(df1, -states, -year)

df2 <- 
  df2 %>%
  mutate(States = revalue(States, c("1" = "EG", "2" = "ICE-WGR", "3" = "NEA")))

df2 <- 
  df2 %>%
  mutate(Year = revalue(Year, c("1"="1996", "2"="1997", "3"="1998", "4"="1999", "5"="2000", "6"="2001", "7"="2002", "8"="2003", "9"="2004", "10"="2005", "11"="2006", "12"="2007"
                                , "13"="2008", "14"="2009", "15"="2010", "16"="2011", "17"="2012", "18"="2013", "19"="2014", "20"="2015", "21"="2016","22"="2017", "23"="2018", "24"="2019")))


bmod_bio_plot <- ggplot(df2, 
                        aes(Year, log.bio, 
                            group = States, colour = States)) +
  geom_point() +
  geom_line() +
  
  geom_ribbon(aes(ymin=log.bio-se, ymax=log.bio+se), fill = "grey80", alpha = .5, linetype = 0, show.legend = F) +
  
  xlab("Years") + ylab("Log-Biomass") +
  theme(strip.text.y = element_blank (),
        legend.position = c(.99, .999),
        legend.justification = c("right", "top"),
        legend.title = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.box.background = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(t =10)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 90, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y.left = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.y.left = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))
bmod_bio_plot
bmod_abu_plot
setwd(fig_loc)
ggsave("Best-Fit-Model_BIOMASS.png", dpi = 300, units = "cm", height = 20, width = 40)

#> BEST-FIT MODEL WITH ABUNDANCE DATA

bmod_abu <- readRDS("run_152.rds")
states1 <- bmod_abu$states
states.se1 <- bmod_abu$states.se
states.df <- melt(states1)
states.se.df <-melt(states.se1)

colnames(states.df) <- c("States","Year","log.abu")
colnames(states.se.df) <- c("states","year","se")

states.df$States <- as.character(states.df$States)
states.df$Year <- as.character(states.df$Year)
df1 <- cbind(states.df, states.se.df)
df2 <-select(df1, -states, -year)

df2 <- 
  df2 %>%
  mutate(States = revalue(States, c("1" = "EG", "2" = "ICE-WGR", "3" = "NEA")))

df2 <- 
  df2 %>%
  mutate(Year = revalue(Year, c("1"="1996", "2"="1997", "3"="1998", "4"="1999", "5"="2000", "6"="2001", "7"="2002", "8"="2003", "9"="2004", "10"="2005", "11"="2006", "12"="2007"
                                , "13"="2008", "14"="2009", "15"="2010", "16"="2011", "17"="2012", "18"="2013", "19"="2014", "20"="2015", "21"="2016","22"="2017", "23"="2018", "24"="2019")))

# PLOT Abundance trajectories

bmod_abu_plot <- ggplot(df2, 
                        aes(Year, log.abu, 
                            group = States, colour = States)) +
  geom_point() +
  geom_line() +
  
  geom_ribbon(aes(ymin=log.abu-se, ymax=log.abu+se), fill = "grey80", alpha = .5, linetype = 0, show.legend = F) +
  
  xlab("Years") + ylab("Log-Abundance") +
  theme(strip.text.y = element_blank (),
        legend.position = c(.99, .999),
        legend.justification = c("right", "top"),
        legend.title = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.box.background = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(t =10)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 90, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y.left = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.y.left = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))
bmod1_plot

setwd(fig_loc)
ggsave("Best-Fit-Model_ABUNDANCE.png", dpi = 300, units = "cm", height = 20, width = 40)
#### OUTPUT ###

