library(dplyr)
library(leaflet)
library(leaflegend)
library(htmlwidgets)
library(sf)
library(sp)
library(readxl)
library(htmltools)
library(jsonlite)
library(data.table)
library(tidyr)
library(readr)

options(digits = 10)

## Year 16 NYCCAS sites
setwd("~/networkDrives/smb-share:server=nasprgshare220,share=share/EHS/BESP/EODEShare")

nyccas_sites <- read_xlsx(paste0(getwd(), "/NYCCAS/Logistics/Working/Scheduling/Year 16/",
                                 "Year16_FallSeason_Sites.xlsx"), sheet  = "Address")
nyccas_sites_y16 <- read_xlsx(paste0(getwd(), "/NYCCAS/Logistics/Working/Scheduling/Year 16/",
                                     "Year16_FallSeason_Sites.xlsx"), sheet  = "Fall")

nyccas_intsites <- nyccas_sites %>%
  filter(site_id %in% nyccas_sites_y16$SiteID |
           Core_EJ == "Reference") %>%
  mutate(cbdtp_extra = ifelse(site_id %in% c("12441", "8555", "7974", "4664-EJ", "12449", "11532", "7717"), "Yes", "No")) %>%
  ## crs code = EPSG code for NAD83 CRS
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

nyccas_intsites_aug <- nyccas_intsites %>%
  filter(cbdtp_extra == "Yes")

## Proposed new RT PM2.5 sites
newRTPM <- read_xlsx(paste0(getwd(), "/NYCCAS/Congestion_Pricing/DOT evaluation/new_rt_sites_proposed.xlsx"))
## Regular RT PM2.5 sites
regRTPM <- read_csv(paste0(getwd(), "/NYCCAS/Real Time Monitors/MasterSiteMonitorList.csv"))

RTPM <- regRTPM %>%
  select(latitude = Lat,
         longitude = Long,
         `Street Segment` = Location,
         Zone = Short_Loc) %>%
  bind_rows(newRTPM) %>%
  distinct(Zone, .keep_all = T) %>%
  ## Include only current and planned sites
  filter(Zone %in% c("Broadway/35th St",
                     "Cross Bronx Expy",
                     "Hunts Point",
                     "Manhattan Bridge",
                     "Midtown-DOT",
                     "Queens College",
                     "Queensboro Bridge",
                     "Williamsburg Bridge",
                     "Glendale",
                     "BQE",
                     "FDR",
                     "TME",
                     "RFK Bridge",
                     "SI Expressway",
                     "Van Wyck Control")) %>%
  mutate(
    Zone = case_when(
      Zone == "TME" ~ "Trans-Manhattan",
      Zone == "RFK Bridge" ~ "Deegan",
      TRUE~Zone),
    Type = ifelse(Zone %in% c("Cross Bronx Expy",
                              "FDR",
                              "BQE",
                              "Trans-Manhattan",
                              "Deegan",
                              "SI Expressway",
                              "Van Wyck Control"), "CBDTP RT", "NYCCAS Other RT"))

nyccas_RTsites <- RTPM %>%
  ## crs code = EPSG code for NAD83 CRS
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

## NYSDOT Short Count Locations

## Fall 2023 Short Count Locations
nysdot_F23 <- read_xlsx(paste0(getwd(), "/NYCCAS/Congestion_Pricing/DOT evaluation/",
                               "NYSDOT_countsites_CBDTP_dft_v2_05-03-24.xlsx"), 
                        sheet  = "fall2023_shortcount_locations")

## Other short count sites
nysdot_all <- read_xlsx(paste0(getwd(), "/NYCCAS/Congestion_Pricing/DOT evaluation/",
                               "NYSDOT_countsites_CBDTP_dft_v2_05-03-24.xlsx"), 
                        sheet  = "NYSDOT_counteraadt_and_truckpct") 

nysdot_all_nyc <- nysdot_all %>%
  filter(County %in% c("Bronx", "Kings", "New york", "Queens", "Richmond"))

nysdot_F23_sites <- nysdot_F23 %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269)

nysdot_alNYC_sites <- nysdot_all_nyc %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269)


map <- leaflet() %>%
  setView(-73.97937985, 40.71852252, zoom = 10) %>%
  addProviderTiles(provider = "CartoDB") %>%
  addCircleMarkers(data = nyccas_intsites, 
                   radius = 7,
                   group = "NYCCAS Integrated",
                   popup = ~paste("<b>Site  ID:</b></br>", htmlEscape(site_id)),
                   stroke = TRUE, # remove polygon borders
                   weight = 1, 
                   color = "black", 
                   fillColor = "gray", 
                   fillOpacity = 0.8) %>% 
  addCircleMarkers(data = nyccas_intsites_aug, 
                   radius = 5,
                   group = "CBDTP Expanded Integrated",
                   popup = ~paste("<b>Site  ID:</b></br>", htmlEscape(site_id)),
                   stroke = TRUE, # remove polygon borders
                   weight = 1, 
                   color = "black", 
                   fillColor = "blue", 
                   fillOpacity = 0.8) %>% 
  addCircleMarkers(data = nyccas_RTsites %>% filter(Type=="CBDTP RT"), 
                   radius = 5,
                   group = "Real-time PM2.5 - CBDTP",
                   popup = ~paste("<b>Site  ID:</b></br>", htmlEscape(Zone)),
                   stroke = TRUE, # remove polygon borders
                   weight = 1, 
                   color = "black", 
                   fillColor = "orange", 
                   fillOpacity = 0.8) %>% 
  addCircleMarkers(data = nyccas_RTsites %>% filter(Type=="NYCCAS Other RT"), 
                   radius = 5,
                   group = "Real-time PM2.5 - NYCCAS, Other",
                   popup = ~paste("<b>Site  ID:</b></br>", htmlEscape(Zone)),
                   stroke = TRUE, # remove polygon borders
                   weight = 1, 
                   color = "black", 
                   fillColor = "brown", 
                   fillOpacity = 0.8) %>% 
  addAwesomeMarkers(data = nysdot_F23_sites,
                    icon = awesomeIcons(icon = "car",
                                        library = "fa", 
                                        iconColor = "black",
                                        markerColor = "blue",
                                        squareMarker = TRUE), 
                    group = "Fall 2023 Short Counts for TBTA",
                    popup = ~paste("<b>Station ID:</b></br>", htmlEscape(`Station ID`))) %>% 
  addAwesomeMarkers(data = nysdot_alNYC_sites,
                    icon = awesomeIcons(icon = "car",
                                        library = "fa",
                                        iconColor = "black",
                                        markerColor = "gray",
                                        squareMarker = TRUE),
                    group = "All NYSDOT Short Counts",
                    popup = ~paste("<b>Station ID:</b></br>", htmlEscape(`Station ID`))) %>%
  hideGroup("All NYSDOT Short Counts") %>%
  addLayersControl(overlayGroups = c("NYCCAS Integrated",
                                     "CBDTP Expanded Integrated", 
                                     "Real-time PM2.5 - CBDTP",
                                     "Real-time PM2.5 - NYCCAS, Other",
                                     "Fall 2023 Short Counts for TBTA",
                                     "All NYSDOT Short Counts"), 
                   options = layersControlOptions(collapsed = FALSE))


saveWidget(
  widget = map,
  file = paste0(getwd(), "/NYCCAS/Congestion_Pricing/DOT evaluation/nyccas_map_with_nysdot.html"),
  selfcontained = TRUE
)


