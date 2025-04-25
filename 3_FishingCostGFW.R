####  Important Shark and Ray Areas can inform conservation planning in the Mediterranean and Black Seas
####  Chris Rohner et al.
####  Script 3
####  Fishing cost from GFW

library(rvest)
library(httr)
library(dplyr)
library(stringr)

#### SAR detections ####
### Load data for multiple files
  # Directory where the CSV files are stored
  directory <- "../data/GFW_new/"

  # List all CSV files in the directory
  file_list <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

    # Define bounding box
    bbox <- st_bbox(Bndry)

    # Define the CRS for EPSG:54009 (World Mollweide)
    crs <- st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

    # Convert bounding box to sf object and set CRS to EPSG:54009
    bbox_sf <- st_as_sfc(bbox) %>%
      st_set_crs(crs)

  # Function to load and filter data from each CSV file
  load_and_filter <- function(file) {
    data <- read.csv(file)

    sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs)

    # Filter data within bounding box
    sf_data <- st_intersection(sf_data, bbox_sf)
    return(sf_data)
  }

  # Load and filter data from each file
  sf_list <- lapply(file_list, load_and_filter)


    # Merge all sf objects into one
    g <- do.call(rbind, sf_list)
    head(g)


## Load data
  # Filter only those that are likely to be fishing
  gf = g %>% filter(fishing_score >= 0.9)

## Extract to PUs
  gfPU2 = gf  %>%
    dplyr::select(fishing_score, geometry) %>%
    mutate(ship = 1) %>%
    sf::st_interpolate_aw(to = PUs, extensive = TRUE, keep_NA = FALSE)

  sum(gfPU2$ship, na.rm = T)

## Extract sum of vessels per PU
  gfPU <- st_join(gf, PUs)

  # Count the number of vessels in each planning unit
  gfPU <- gfPU %>%
    group_by(cellID) %>%
    summarise(vessel_count = n())

  # Join the counts back to the planning units for mapping or further analysis
  fishing <- PUs %>%
    st_join(gfPU, by = "cellID")

  head(fishing)
    sum(fishing$vessel_count, na.rm=T)
    sum(!is.na(gfPU$cellID))
    sum(gfPU$vessel_count)

  summary(fishing$vessel_count)
  100/length(fishing$vessel_count)*6093 # 17.6%% of PUs without fishing

  ggplot() +
    geom_sf(data = fishing, aes(fill = vessel_count, col = vessel_count)) +
    geom_sf(data = fishing %>% filter(vessel_count == 0), fill = "snow2", color = "snow2") +
    geom_sf(data = fishing %>% filter(is.na(vessel_count)), fill = "snow2", color = "snow2") +
    scale_fill_viridis_c() + scale_colour_viridis_c() +
    splnr_gg_add(
      Bndry = Med03, overlay = landmass,
      colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)

  fishing = fishing %>% select(-cellID.y) %>% rename(cellID = cellID.x)

#### Combine SAR with AIS based data to examine the overlap/mismatch ####
    fishing_df <- st_drop_geometry(fishing)
    dat_fishing_df <- st_drop_geometry(dat_fishing)

  c <- fishing_df %>%
    left_join(dat_fishing_df, by = "cellID")

  c <- st_as_sf(c, geometry = st_geometry(fishing))


  # Check how many PUs in each
  c2 <- c %>%
    mutate(
      satellite = !is.na(vessel_count),
      AIS = ApparentFishingHrs != 0,
      both = satellite & AIS,
      either = satellite | AIS)

  summary(c2$AIS)
  100/length(c2$AIS)*(as.integer(summary(c2$AIS)[3])) # 72.6% of PUs with AIS info
  100/length(c2$satellite)*(as.integer(summary(c2$satellite)[3])) # 82.4% of PUs with satellite info
  100/length(c2$both)*(as.integer(summary(c2$both)[3])) # 63.4% of PUs overlapping info from AIS and satellite
  100/length(c2$either)*(as.integer(summary(c2$either)[3])) # 91.5% of PUs with at least some info from AIS or satellite


  # Create a new column to categorise conditions
  c3 <- c2 %>%
    mutate(
      condition = case_when(
        both ~ "Both",
        satellite & !both ~ "Satellite",
        AIS & !both ~ "AIS",
        TRUE ~ "Other"
      )
    )

  write_rds(c3, "Output/c3.rds")

  table(c3$condition)
  100/length(c3$condition)*as.numeric(table(c3$condition)[1]) # 9.1% only AIS
  100/length(c3$condition)*as.numeric(table(c3$condition)[2]) # 63.4% both
  100/length(c3$condition)*as.numeric(table(c3$condition)[3]) # 8.5%% only other
  100/length(c3$condition)*as.numeric(table(c3$condition)[4]) # 19% only satellite

  # Plot
  ggplot() +
    geom_sf(data = c3 %>% filter(condition == "Satellite"),
            aes(fill = condition, col = condition),
            alpha = 0.5) +
    geom_sf(data = c3 %>% filter(condition == "AIS"),
            aes(fill = condition, col = condition),
            alpha = 0.5) +
    geom_sf(data = c3 %>% filter(condition == "Both"),
            aes(fill = condition, col = condition),
            alpha = 0.5) +
    scale_fill_manual(values = c("Satellite" = "blue", "AIS" = "orange", "Both" = "lightgreen")) +
    scale_color_manual(values = c("Satellite" = "blue", "AIS" = "orange", "Both" = "lightgreen")) +
    labs(fill = "Condition", col = "Condition") +
    theme_minimal() +
    ggtitle("GFW - AIS vs Satellite") +
    splnr_gg_add(
      Bndry = Med03, overlay = landmass,
      colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)

#### Updated with only PUs used in the study ####
  c3 <- readRDS("../Output/c3.rds") # Done with the whole Med and Black Seas area (including PUs that don't have any sharks)
  noSharks <- readRDS("../Output/noSharks.rds") # PUs with no sharks

  c3 <- c3 %>% filter(!cellID %in% c(noSharks$cellID)) %>%
    mutate(condition = recode(condition, "Other" = "No data"))

  100/length(c3$condition)*as.numeric(table(c3$condition)[1]) # 9.0% only AIS
  100/length(c3$condition)*as.numeric(table(c3$condition)[2]) # 67.1% both
  100/length(c3$condition)*as.numeric(table(c3$condition)[3]) # 6.2%% only other
  100/length(c3$condition)*as.numeric(table(c3$condition)[4]) # 17.6% only satellite


  # Plot Supp. Fig. 1
  ggplot() +
    geom_sf(data = c3 %>% filter(condition == "Satellite"),
            aes(fill = condition, col = condition),
            alpha = 0.5) +
    geom_sf(data = c3 %>% filter(condition == "AIS"),
            aes(fill = condition, col = condition),
            alpha = 0.5) +
    geom_sf(data = c3 %>% filter(condition == "Both"),
            aes(fill = condition, col = condition),
            alpha = 0.5) +
    geom_sf(data = c3 %>% filter(condition == "No data"),
            aes(fill = condition, col = condition),
            alpha = 0.5) +
    scale_fill_manual(values = c("Satellite" = "blue", "AIS" = "orange", "Both" = "lightgreen", "No data" = "pink")) +
    scale_color_manual(values = c("Satellite" = "blue", "AIS" = "orange", "Both" = "lightgreen", "No data" = "pink")) +
    labs(fill = "Condition", col = "Condition") +
    theme_minimal() +
    ggtitle("GFW - AIS vs Satellite") +
    geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    splnr_gg_add(
      Bndry = Med03, overlay = landmass,
      colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)


  c4 = c3 %>% filter(both == TRUE)
  cor(c4$vessel_count, c4$ApparentFishingHrs)


  ## By interpolating Apparent fishing hours:
  table(c3$condition)

  # With a linear model
  model = lm(log10(c4$ApparentFishingHrs) ~ log10(c4$vessel_count))
  summary(model)

  ggplot(c4, aes(x = log10(vessel_count), y = log10(ApparentFishingHrs))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(x = "log(Vessel Count)", y = "log(Apparent Fishing Hours)") +
    annotate("text", x = Inf, y = Inf, label = paste("R² =", round(summary(model)$r.squared, 3)),
             hjust = 1.1, vjust = 2, size = 5, color = "red")

  c5 = c3 %>%
    mutate(SatDerived_ApparentFishingHrs = 10^(0.74049 + 1.03086 * log10(vessel_count))) %>%
    mutate(CombinedApparentFishingHrs = ifelse(!is.na(ApparentFishingHrs) & ApparentFishingHrs != 0,
                                               ApparentFishingHrs,
                                               SatDerived_ApparentFishingHrs))

  summary(c5$CombinedApparentFishingHrs) # 8.46% PUs with missing data > add half of the minimum to avoid empty cells
  table(c5$CombinedApparentFishingHrs)

  # Set all cells with <0.1 fishing hours (n = 214) = 0.1
  # and also the NA cells (n = 1974) = 6.2%
  c5 = c5 %>%
    mutate(CombinedApparentFishingHrs = ifelse(CombinedApparentFishingHrs <= 0.1, 0.1, CombinedApparentFishingHrs)) %>%
    mutate(CombinedApparentFishingHrs = ifelse(is.na(CombinedApparentFishingHrs), 0.1, CombinedApparentFishingHrs))

  summary(c5$CombinedApparentFishingHrs)

  write_rds(c5, "../Output/c5.rds")

  ggplot(c5) + geom_histogram(aes(x = CombinedApparentFishingHrs))

  ggplot() +
    geom_sf(data = Med03) +
    geom_sf(data = c5, aes(fill = log10(CombinedApparentFishingHrs), colour = log10(CombinedApparentFishingHrs))) +
    geom_sf(data = c5 %>% filter(is.na(CombinedApparentFishingHrs)), fill = "snow1", color = "snow1") +
    scale_fill_viridis_c(name = "log(Apparent Fishing Hrs)") + scale_colour_viridis_c(name = "log(Apparent Fishing Hrs)") +
    theme_bw() +
    labs(title = "Combined Apparent Fishing Hours AIS + Satellite Data") +
    splnr_gg_add(
      Bndry = Med03, overlay = landmass,
      colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)



  t = c5 %>% filter(condition == "Both") %>%
    mutate(diff = ApparentFishingHrs - SatDerived_ApparentFishingHrs)
  summary(t$diff)

  ggplot(t) + geom_point(aes(x = ApparentFishingHrs, y = SatDerived_ApparentFishingHrs))
  ggplot(t) + geom_histogram(aes(x = diff))
