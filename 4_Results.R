####  Important Shark and Ray Areas can inform conservation planning in the Mediterranean and Black Seas
####  Chris Rohner et al.
####  Script 4
####  Prioritisation and other results


# Load necessary data
source("../00_SetupPrioritizR.R")

### Result 1 Overlap of ISRAs and MPAs ####
### Planning region
  # Original area:
  st_area(Med03) # equivalent to 34553 PUs, each 86.51861 km2

  # Area without sharks
  sum(st_area(noSharks)) # equivalent to 2752 PUs

  # Area of planning region final
  PlanningRegionArea = (st_area(Med03)-sum(st_area(noSharks)) )/1000000

st_area(Med03)/34553*length(PUs$cellID)

### Area of no-take MPAs
summary(st_area(mpas)/1000000)
  sum(st_area(mpas)/1000000)
  100/(PlanningRegionArea)*(sum(st_area(mpas)/1000000)) # 0.14%

  ## Depth of MPAs
  mpa_bathy = oceandatr::get_bathymetry(spatial_grid = dat_mpas2 %>% filter(data == 1), classify_bathymetry = FALSE)
  summary(mpa_bathy$bathymetry)

# How many overlap with ISRAs
  mpas <- st_transform(mpas, st_crs(isra))

  # Find the intersection between the MPAs and ISRAs
  overlap <- st_intersection(mpas, isra)
    # 11 MPAs overlap

  # Join attributes from mpas to intersecting geometries
  intersecting_geometries <- st_join(overlap, mpas)
  intersecting_geometries$Name

  # Calculate the area of overlap
  overlap_area <- st_area(overlap)
  sum(overlap_area)/1000000
    100/(sum(st_area(mpas)/1000000))*(sum(overlap_area)/1000000) # 37.1% overlap

# Then calculate how much of the ISRAs is in an MPA
  # first get the total area of ISRA polygons
  total_isra_area <-  st_area(st_union(isra))

    100/(PlanningRegionArea)*(total_isra_area/1000000) # 16.2%


  # Calculate the total area of ISRA polygons that intersect with MPAs
  intersecting_isra_area <- sum(st_area(st_intersection(mpas, isra)))

  # Calculate the percentage
  (intersecting_isra_area / total_isra_area) * 100 #0.31%


  ## Plot Figure 1A
  dat_isra2_filtered <- dat_isra2 %>% filter(data == 1) %>% mutate(layer = "ISRAs")
  dat_mpas2_filtered <- dat_mpas2 %>% filter(data == 1) %>% mutate(layer = "No-take MPAs")

  # Combine the two data frames
  combined_data <- bind_rows(dat_isra2_filtered, dat_mpas2_filtered)

  # Plot using ggplot2
  (f1 <- ggplot() +
      geom_sf(data = combined_data, aes(fill = layer), color = NA, alpha = 0.8) +
      labs(title = bquote(bold("a") ~ "No-take MPAs and ISRAs")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      splnr_gg_add(Bndry = Bndry,  overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme) +
    scale_fill_manual(name = "Layers",
      values = c("ISRAs" = alpha("#F16364", 0.7), "No-take MPAs" = alpha("darkgreen", 0.8))) )


## Add the diversity figure
  head(dat_species)
  dsp = dat_species %>% mutate(diversity = rowSums(across(2:74)))

  # Check area per species: 4 CR and 3 EN species have huge distribution and need high % thereof protected
  column_sums <- dat_species %>% select(2:74) %>% st_drop_geometry() %>%
    pivot_longer(cols = everything(), names_to = "species", values_to = "value") %>%
    group_by(species) %>%
    summarise(area = sum(value, na.rm = TRUE)) %>%
    left_join(target2 %>% select(nameVariable, IUCN, target) %>% rename(species = nameVariable, target2a = target), by = "species") %>%
    left_join(target4 %>% select(nameVariable, target) %>% rename(species = nameVariable, target4a = target), by = "species") %>%
    mutate(target2 = target2a*area, t2 = 100/length(PUs$cellID)*target2,
           target4 = target4a*area, t4 = 100/length(PUs$cellID)*target4)

  # Add the "1" at the base of the legend
  breaks <- c(1,20,40,60)
  labels <- as.character(breaks)

  (f2 = ggplot(data = dsp, aes(fill = diversity, col = diversity)) +
    geom_sf() +
    scale_color_viridis(option = "viridis", breaks = breaks, labels = labels, limits = c(1, 60)) +
    scale_fill_viridis(option = "viridis", breaks = breaks, labels = labels, limits = c(1, 60)) +
      labs(color = "Species\nRichness") +
      labs(fill = "Species\nRichness") +
      labs(title = bquote(bold("b") ~ "Species richness")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    splnr_gg_add(
      Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme) )

  # Diversity in the Black Sea
  dsp %>% left_join(dat6 %>% st_drop_geometry() %>% dplyr::select(cellID, MEOW), by = "cellID") %>%
    filter(MEOW == "Black Sea") %>%
    summarise(mean(diversity), min(diversity), max(diversity), sd(diversity), median(diversity))

  # Species richness metrics
  r2 <- dat_species %>% sf::st_drop_geometry() %>% mutate(species.richness = rowSums(select(., Aetomylaeus_bovinus:Torpedo_torpedo), na.rm = TRUE))
  summary(r2$species.richness)

## Plot cost 1C
   (f3 =  ggplot() +
      geom_sf(data = Med03, fill = "transparent") +
      geom_sf(data = c5, aes(fill = log10(CombinedApparentFishingHrs), colour = log10(CombinedApparentFishingHrs))) +
      scale_fill_viridis_c(name = "log(Apparent\nFishing Hrs)") + scale_colour_viridis_c(name = "log(Apparent\nFishing Hrs)") +
      theme_bw() +
      labs(title = bquote(bold("c") ~ "Fishing cost")) +
       geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      splnr_gg_add(
        Bndry = Med03, overlay = landmass,
        colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme)   )


### Plot geographical names Fig. 1D
## Seas
seas <- data.frame(
  lon = c(-2.7, 5.2, 9.00, 12.5, 15, 18.5,  25, 30, 18, 35),  # Actual locations
  lat = c(36.2, 41,  44, 39.5,  43, 37.5, 39, 33, 34, 43.3),
    label = c("Alboran Sea", "Balearic\nSea", "Ligurian\nSea", "Tyrrhenian\nSea",
              "Adriatic\nSea", "Ionian\nSea", "Aegean\nSea", "Levantine Sea",
              "Libyan Sea", "Black Sea"))

    # Transform to cCRS
    seas = st_as_sf(seas, coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs = st_crs(Med03))

    seas$lon <- st_coordinates(seas)[,1]
    seas$lat <- st_coordinates(seas)[,2]

## Locations
locs <- data.frame(
        lon = c(13),
        lat = c(36),
        label = c("Strait of\nSicily"),
        label_lon = c(7),
        label_lat = c(35),
        segment_lon = c(9.5),
        segment_lat = c(35))

      # Transform to cCRS
        locs2 = st_as_sf(locs, coords = c("lon", "lat"), crs = 4326) %>%
          st_transform(crs = st_crs(Med03))
        locs3 = st_as_sf(locs, coords = c("label_lon", "label_lat"), crs = 4326) %>%
          st_transform(crs = st_crs(Med03))
        locs4 = st_as_sf(locs, coords = c("segment_lon", "segment_lat"), crs = 4326) %>%
          st_transform(crs = st_crs(Med03))

        locs$lon <- st_coordinates(locs2)[,1]
        locs$lat <- st_coordinates(locs2)[,2]

        locs$label_lon <- st_coordinates(locs3)[,1]
        locs$label_lat <- st_coordinates(locs3)[,2]

        locs$segment_lon <- st_coordinates(locs4)[,1]
        locs$segment_lat <- st_coordinates(locs4)[,2]


# Plot it with depth
depth_zone_order <- c("Inshore", "Midshelf", "Offshore")

(f1d <- ggplot() +
  geom_sf(data = PUs_combined %>%  mutate(DepthZone = recode(DepthZone,
                                                             "Epipelagic" = "Inshore",
                                                             "Mesopelagic" = "Midshelf",
                                                             "Bathypelagic" = "Offshore")),
          aes(fill = DepthZone, color = DepthZone), alpha = 1) +
    scale_fill_manual(values = c("Inshore" = alpha("#E0FFFF", 1),
                                 "Midshelf" = alpha("#ADD8E6", 1),
                                 "Offshore" = alpha("#4682B4", 1)),
                      name = "Depth Zones",
                      breaks = depth_zone_order,
                      labels = depth_zone_order) +
    scale_color_manual(values = c("Inshore" = alpha("#E0FFFF", 1),
                                  "Midshelf" = alpha("#ADD8E6", 1),
                                  "Offshore" = alpha("#4682B4", 1)),
                       name = "Depth Zones",
                       breaks = depth_zone_order,
                       labels = depth_zone_order) +

  geom_sf(data = Med03, fill = "transparent")    +

  geom_sf(data = landmass, fill = "grey70", color = "grey70") +
  splnr_gg_add(
    Bndry = Med03, overlay = landmass,
    colorOverlay = "grey70",
    cropOverlay = PUs, ggtheme = splnr_theme) +

  geom_text(data = seas,
            aes(x = lon, y = lat, label = label),
            size = 2, fontface = "bold", col = "black") +

  # Locations
  geom_text(data = locs,
            aes(x = lon, y = lat, label = label),
            size = 2, fontface = "italic")  +
  ggtitle(bquote(bold("d") ~ "Important locations and depth zones"))
)


# Figure 1
patchwork::wrap_plots(f1 + theme(axis.text.x = element_blank()),
                      f2 + theme(axis.text.x = element_blank()),
                      f3 + theme(axis.text.x = element_blank()),
                      f1d,
                      ncol = 1, nrow = 4)


### Which species occur in the no-take MPAs?
  datt <- dplyr::bind_cols(dat_species, dat_isra2 %>% sf::st_drop_geometry() %>% select(!cellID),
                           dat_mpas2 %>% sf::st_drop_geometry() %>% select(!cellID)) %>%
    rename(ISRAs = `data...76`, MPAs = `data...77`)

  head(datt)

  # In how many PUs is each species distributed
  sum_all_PUs <- datt %>%
    st_drop_geometry() %>%
    summarise(across(Aetomylaeus_bovinus:Torpedo_torpedo, ~ sum(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "Species", values_to = "all_PUs")

  # How many PUs of a species distribution are inside an MPA
  sum_MPAs <- datt %>%
    filter(MPAs == 1) %>%
    st_drop_geometry() %>%
    summarise(across(Aetomylaeus_bovinus:Torpedo_torpedo, ~ sum(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "Species", values_to = "PUs_in_MPAs")


  # How many PUs of a species distribution are inside an ISRA
  sum_ISRAs <- datt %>%
    filter(ISRAs == 1) %>%
    st_drop_geometry() %>%
    summarise(across(Aetomylaeus_bovinus:Torpedo_torpedo, ~ sum(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "Species", values_to = "PUs_in_ISRAs")

  # Merge the two summaries
  species_in_MPAs <- merge(sum_all_PUs, sum_MPAs, by = "Species", all.x = TRUE) %>% mutate(perc.in.MPAs = 100/all_PUs*PUs_in_MPAs)
  species_in_MPAs <- merge(species_in_MPAs, sum_ISRAs, by = "Species", all.x = TRUE) %>% mutate(perc.in.ISRAs = 100/all_PUs*PUs_in_ISRAs)
    head(species_in_MPAs)



### Correlation between species richness and cost
  dc = cbind(dsp$cellID, dsp$diversity, dat1d$cost, dat_isra2$data, Bathy$bathymetry) %>% as.data.frame() %>% rename(cellID = 1, richness = 2, cost = 3, isra = 4, depth = 5)

  cor(dc$richness, dc$cost)

  plot(dc$cost, dc$richness, main = "Scatter plot of Richness vs Cost",
             xlab = "Cost", ylab = "Richness")
  abline(lm(richness ~ cost, data = dc), col = "red")

  lm_model <- lm(richness ~ cost, data = dc)
  summary(lm_model)


### Correlation between species richness and ISRAs
  cor(dc$richness, dc$isra)

  dc %>% group_by(isra) %>%
    summarise(mean_diversity = mean(richness, na.rm = T))

  t.test(dc$richness[dc$isra == 0], dc$richness[dc$isra == 1])


### Correlation between cost and ISRAs
  cor(dc$cost, dc$isra)

  dc %>% group_by(isra) %>%
    summarise(mean_cost = mean(cost, na.rm = T))

  t.test(dc$cost[dc$isra == 0], dc$cost[dc$isra == 1])


### Correlation between depth and ISRAs
  cor(dc$depth, dc$isra)

  dc %>% group_by(isra) %>%
    summarise(mean_depth = mean(depth, na.rm = T),
              sd_depth = sd(depth, na.rm = T)) %>% as.data.frame()

  t.test(dc$depth[dc$isra == 0], dc$depth[dc$isra == 1])


### Fishing Cost calculations for methods
  head(c5)

  # How many observations with AIS
  table(c5$AIS)
  100/length(PUs$cellID)*(table(c5$AIS)[2]) #76.2%

  # How many with satellite
  100/length(PUs$cellID)*(table(c5$satellite)[2]) #84.8%

  # How many with both
  100/length(PUs$cellID)*(table(c5$both)[2]) #67.1%

  # How many with none
  100/length(PUs$cellID)*(table(c5$either)[1]) #6.2%



## KBAs and IMMAs

dat_imma <- readRDS("../Output/dat_imma.rds")
dat_imma %>%
  st_drop_geometry() %>%
  group_by(data) %>%
  summarise(mean_depth = mean(bathymetry), sd_depth = sd(bathymetry), median(bathymetry))

dat_imma %>%
  st_drop_geometry() %>%
  t.test(bathymetry ~ data, data = .)


dat_kba <- readRDS("../Output/dat_kba.rds")

dat_kba %>% st_drop_geometry() %>%
  group_by(data) %>%
  summarise(mean_depth = mean(bathymetry), sd_depth = sd(bathymetry), median(bathymetry)) %>% as.data.frame()


#
### Working out the Boundary Penalty ####
  # https://prioritizr.net/articles/calibrating_trade-offs_tutorial.html#preliminary-processing
  # 1. Find the range of boundary penalties to test later
  # Generate boundary length data for the planning units
  med_bd <- boundary_matrix(dat1d)
  med_bd <- rescale_matrix(med_bd)


  prelim_lower <- -10
  prelim_upper <- 0.1
  prelim_penalty <- round(10^seq(prelim_lower, prelim_upper, length.out = 9), 10)

  # Manually setting them
  prelim_penalty = c(0.0000000001, 0.0000006000, 0.0000100000, 0.005, 0.1, 0.7 ) # Only betwen the first two is useful
  prelim_penalty <- seq(from = 0.0000000001, to = 0.0000006, length.out = 6) # Again between the first two is the action, so let's go again

  prelim_penalty = c(1.0000e-10, 1.7240e-08, 3.4380e-08, 5.1520e-08, 6.8660e-08, 8.5800e-08, 1.0294e-07, 1.2008e-07, 0.00000024006) # Again between the first two...again
  prelim_penalty <- seq(from = 0.0000000001, to = 1.0e-08 , length.out = 9)

  # Then use the good options to run through full examples without the time-limit
  prelim_penalty = seq(from = 1.2896e-08, to = 1.4344e-08 , length.out = 3)

  # print penalty values
  print(prelim_penalty)

  # Test them, using simplified method to reduce runtime
  p0 <- prioritizr::problem(dat1d, features = features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target2$target) %>%
    add_locked_in_constraints(as.logical(dat_mpas2$data)) %>% # Lock in no-take MPAs
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)

  # generate preliminary prioritizations based on each penalty
  ## note that we specify a relaxed gap and time limit for the solver
  prelim_blended_results <- lapply(prelim_penalty, function(x) {
    s <-
      p0 %>%
      add_boundary_penalties(penalty = x, data = med_bd) %>%
      add_default_solver(gap = 0.4, time_limit = 3 * 60) %>%
      solve()
    s <- data.frame(s = s$solution_1)
    names(s) <- with_options(list(scipen = 30), paste0("penalty_", x))
    s
  })

  # format results as a single spatial object
  prelim_blended_results <- cbind(
    dat1c, do.call(bind_cols, prelim_blended_results)
  )

  # preview results
  print(prelim_blended_results)


  # plot maps of prioritizations
  long_data2 <- prelim_blended_results %>%
    pivot_longer(cols = starts_with("penalty_"),
                 names_to = "scenario",
                 values_to = "value") %>%
    mutate(value = as.factor(value))

  # Create the plot using ggplot2 and facet_wrap
  ggplot(long_data2) +
    geom_sf(aes(fill = value, geometry = geometry), color = NA) +
    scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkblue")) +
    facet_wrap(~ scenario) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(title = "Penalty Scenarios", fill = "Value") +
    splnr_gg_add(
      Bndry = Med03, overlay = landmass,
      colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)


 long_data2 %>% mutate(value = as.numeric(as.character(value))) %>%
    group_by(scenario) %>%
    summarise(selected = sum(value), n = length(value)) %>%
    mutate(perc.selected = 100/34553*selected)

 # Without a penalty
 table(s0$solution_1); 100/length(s0$solution_1)*(table(s0$solution_1)[2]) # 33.9% of PUs selected


  # Zoom to one
 ggplot(long_data2 %>% filter(scenario == "penalty_0.000000014344")) +
   geom_sf(aes(fill = value, geometry = geometry), color = NA) +
   scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkblue")) +
   theme_minimal() +
   theme(legend.position = "right") +
   labs(title = "Penalty Scenarios", fill = "Value") +
   splnr_gg_add(
     Bndry = Med03, overlay = landmass,
     colorOverlay = "grey70",
     cropOverlay = PUs, ggtheme = splnr_theme)

 ## Best boundary penalty for Scenarios 1-4:
 BPoverall = 0.005


 ## Reviewer comment: Visualise boundary penalty differences ####
 p0 <- prioritizr::problem(dat1d, features = features, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target_new$target) %>% # Medium target
   add_locked_in_constraints(as.logical(dat_mpas2$data)) %>% # Lock in no-take MPAs
   add_binary_decisions() %>%
   add_default_solver(verbose = FALSE) %>%
   # Different Boundary Penalties

    add_boundary_penalties(penalty = 5e-10, data = med_bd) # a
    #add_boundary_penalties(penalty = 5e-05, data = med_bd)  # b
    #add_boundary_penalties(penalty = 5e-03, data = med_bd)  # c
    #add_boundary_penalties(penalty = 5e-01, data = med_bd)  # d

 s0a <- solve(p0)
 s0b <- solve(p0)
 s0c <- solve(p0)
 s0d <- solve(p0)

 (gg0a <- splnr_plot_solution(s0a, plotTitle = "") +
     ggtitle(label = "", subtitle = bquote(bold("a") ~ "Boundary penalty = 5e-10")) +
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70", cropOverlay = PUs, ggtheme = splnr_theme) )

 (gg0b <- splnr_plot_solution(s0b, plotTitle = "") +
     ggtitle(label = "", subtitle = bquote(bold("b") ~ "Boundary penalty = 5e-5")) +
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70", cropOverlay = PUs, ggtheme = splnr_theme) )

 (gg0c <- splnr_plot_solution(s0c, plotTitle = "") +
     ggtitle(label = "", subtitle = bquote(bold("c") ~ "Boundary penalty = 5e-3")) +
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70", cropOverlay = PUs, ggtheme = splnr_theme) )

 (gg0d <- splnr_plot_solution(s0d, plotTitle = "") +
     ggtitle(label = "", subtitle = bquote(bold("c") ~ "Boundary penalty = 5e-1")) +
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70", cropOverlay = PUs, ggtheme = splnr_theme) )


#
### Result 2 Difference between spatial plans ignoring ISRAs (Scenario 1) and locking-in ISRAs (Scenario 2) ####
  ### Scenario 1: ignoring ISRAs ####
p1 <- prioritizr::problem(dat1d, features = features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_new$target) %>% # new target
    add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(as.logical(dat_mpas2$data)) %>%
    add_default_solver(gap = 0.001, verbose = FALSE)

  ## Solve it
  s1 <- solve(p1)

  ## Area and cost
  100/length(s1$solution_1)*table(s1$solution_1)[2]
  h1 <- left_join(as.data.frame(s1), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
  h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))

  ## Plot solution
  (gg1 <- splnr_plot_solution(s1, plotTitle = "") +
      ggtitle(label = "", subtitle = bquote(bold("a") ~ "Ignoring ISRAs")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      geom_sf(data = isra, fill = NA, color = "#F16364", size = 1.5) +

      splnr_gg_add(
    Bndry = Med03, overlay = landmass,
    colorOverlay = "grey70",
    cropOverlay = PUs, ggtheme = splnr_theme) +
      geom_label(aes(x = -653592.9, y = 5626014, label = "Selected\nArea: 26.9%\nCost: 2.6%"),
                 hjust = 0,  vjust = 1, size = 3.5) )


  ## Visualise targets:
  do <- c("CR", "EN", "VU", "NT", "LC", "DD")
  dfTarget <- splnr_get_featureRep(s1, p1,
                                   climsmart = FALSE, solnCol = "solution_1"
  ) %>%
    left_join(Dict, by = c("feature" = "nameVariable")) %>%
    rename(cat = category) %>%
    mutate(category = factor(IUCN, levels = do)) %>%
    arrange(category)


  # Plot
  dfTarget_normal = dfTarget %>% filter(target_cat == "normal")
  dfTarget_wide = dfTarget %>% filter(target_cat == "wide")
  dfTarget_new = rbind(dfTarget_normal, dfTarget_wide) %>% arrange(IUCN, desc(target))

  (ggTarget_new <- splnr_plot_featureRep(df = dfTarget_new, category = Dict, categoryFeatureCol = IUCN,
                                           nr = 1, showTarget = FALSE, plotTitle = "A) Ignoring ISRAs") +
    scale_x_discrete(limits = dfTarget_new[order(dfTarget_new$category), ]$feature, labels = dfTarget_new$species) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
          legend.position = "inside",
          legend.position.inside = c(0.02, 0.98),
          legend.justification = c(0, 1)) +
    scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) )


  ## importance scores
  # Ferrier Score
  ggFerrier1 <- splnr_plot_importanceScore(
    soln = s1,
    pDat = p1,
    method = "Ferrier",
    decimals = 4,
    legendTitle = "Ferrier Score"
  ) +
    geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme
    )


  # Rarity Wighted Richness.
  ggRWR1 <- splnr_plot_importanceScore(
    soln = s1,
    pDat = p1,
    method = "RWR",
    decimals = 2,
    legendTitle = "Rarity Weighted \nRichness"
  ) +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme
    )


  (ggScores <- patchwork::wrap_plots(ggFerrier1 + ggRWR1))


  ### Scenario 2: locking-in ISRAs ####
  # Set the problem
  p2 <- prioritizr::problem(dat1d, features = features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_new$target) %>%
    add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(as.logical(dat_isra2$data)) %>%
    add_locked_in_constraints(as.logical(dat_mpas2$data)) %>%
    add_default_solver(gap = 0.01, verbose = FALSE)

  # Solve it
  s2 <- solve(p2)

  100/length(s2$solution_1)*table(s2$solution_1)[2]
  h2 <- left_join(as.data.frame(s2), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
  h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost))) #

  # Plot solution
  ( gg2 <- splnr_plot_solution(s2, plotTitle = "")  +
      ggtitle(label = "", subtitle = bquote(bold("b") ~ "Including ISRAs")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      splnr_gg_add(
        Bndry = Med03, overlay = landmass,
        colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme) +
      geom_label(aes(x = -653592.9, y = 5626014, label = "Selected\nArea: 27.1%\nCost: 34.1%"),
                 hjust = 0,  vjust = 1, size = 3.5) )


  # Plot with MPAs and ISRAs also shown
  (gg2b <- splnr_plot_solution(s2, plotTitle = "B) Including ISRAs") +
      splnr_gg_add(
        PUs = NULL, Bndry = Bndry, lockedInAreas = dat_isra2,
        colInterest = dat_isra2$data, Type = "Full", # Type = "Contours"
        colorLI = "goldenrod", alphaLI = 0.8, overlay = landmass,  colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme, labelL = "ISRAs"
      ) +
      splnr_gg_add(
        PUs = NULL, Bndry = Bndry, lockedInAreas = dat_mpas2,
        colInterest = dat_mpas2$data, Type = "Full", # Type = "Contours"
        colorLI = "lightgreen", alphaLI = 0.8, overlay = landmass,  colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme, labelL = "MPAs") )


  ## Visualise targets:
  do <- c("CR", "EN", "VU", "NT", "LC", "DD")
  dfTarget2 <- splnr_get_featureRep(s2, p2,
                                   climsmart = FALSE, solnCol = "solution_1") %>%
    left_join(Dict, by = c("feature" = "nameVariable")) %>%
    rename(cat = category) %>%
    mutate(category = factor(IUCN, levels = do)) %>%
    arrange(category)


  # Plot
  dfTarget2_normal = dfTarget2 %>% filter(target_cat == "normal")
  dfTarget2_wide = dfTarget2 %>% filter(target_cat == "wide")
  dfTarget2_new = rbind(dfTarget2_normal, dfTarget2_wide) %>% arrange(IUCN, desc(target))

  (ggTarget_new2 <- splnr_plot_featureRep(df = dfTarget2_new, category = Dict, categoryFeatureCol = IUCN,
                                        nr = 1, showTarget = FALSE, plotTitle = "B) Including ISRAs") +
    scale_x_discrete(limits = dfTarget2_new[order(dfTarget2_new$category), ]$feature, labels = dfTarget2_new$species) + # Use species column for labels
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
          legend.position = "inside",
          legend.position.inside = c(0.02, 0.98),   # Adjust the values as needed
          legend.justification = c(0, 1)) +
    scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0))   )


  ## Check the importance scores
  # Ferrier Score
  ggFerrier2 <- splnr_plot_importanceScore(
    soln = s2,
    pDat = p2,
    method = "Ferrier",
    decimals = 4,
    legendTitle = "Ferrier Score"
  ) +
    geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme
    )


  # Rarity Wighted Richness.
  ggRWR2 <- splnr_plot_importanceScore(
    soln = s2,
    pDat = p2,
    method = "RWR",
    decimals = 2,
    legendTitle = "Rarity Weighted \nRichness"
  ) +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme
    )


  (ggScores <- patchwork::wrap_plots(ggFerrier1 + ggRWR1 + ggFerrier2 + ggRWR2))

  ## PUs and ISRAs
  # Number of PUs
  sum(dat$isra);  100/(length(dat$cellID))*sum(dat$isra) # ISRAs: 17% of PUs
  sum(dat$mpa);  100/(length(dat$cellID))*sum(dat$mpa) # MPAs: 0.22% of PUs
  length(dat$cellID) # PUs

  ### Compare scenarios ####

  ### Area covered
  # Number of PUs
  sum(dat$isra);  100/(length(dat$cellID))*sum(dat$isra)
  sum(dat$mpa);  100/(length(dat$cellID))*sum(dat$mpa)
  length(dat$cellID)

  table(s1$solution_1); 100/length(s1$solution_1)*(table(s1$solution_1)[2]) # 26.9%
  table(s2$solution_1); 100/length(s2$solution_1)*(table(s2$solution_1)[2]) # 27.1%

    # combine solution with isra and mpa information
    h1 <- left_join(as.data.frame(s1), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
    h2 <- left_join(as.data.frame(s2), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")

      # How many selected PUs are NOT in locked-in areas:
      sum(h1$solution_1) - (h1 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
      sum(h2$solution_1) - (h2 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())

      # How many selected PUs are in ISRAs:
      h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h1$isra)*(h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
      h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow())

      # How many selected PUs are in MPAs:
      h1 %>% filter(solution_1 == 1, mpa == 1) %>% nrow(); 100/sum(h1$mpa)*(h1 %>% filter(solution_1 == 1, mpa == 1) %>% nrow())
      h2 %>% filter(solution_1 == 1, mpa == 1) %>% nrow(); 100/sum(h2$mpa)*(h2 %>% filter(solution_1 == 1, mpa == 1) %>% nrow())

  ## Compare the costs
      tc = h1 %>% summarise(total_cost = sum(cost)) # overall cost
  h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost))) # 2.6%
  h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost))) # 34.1%

  ## Which ISRA is most expensive
  h4 = h2 %>% select(cellID, cost, solution_1, geometry, isra, mpa) %>%
    rename(sol_wISRA = solution_1) %>%
    mutate(ISRAn = dat_isra$ISRA_ISRA_names) %>%
    mutate(sol_noISRA = h1$solution_1)

  h4b <- h4 %>%
    separate_rows(ISRAn, sep = ",") %>%
    mutate(ISRAn = str_trim(ISRAn))

  # Group by the individual ISRA names and summarise
  # Cost of PUs for the ISRA solution
  t <- h4b %>%
    group_by(ISRAn) %>%
    filter(sol_wISRA == 1) %>%
    summarise(
      total_cost = sum(cost, na.rm = TRUE),
      PUs = n()) %>%
    mutate(cost_per_PU = total_cost/PUs)

  # Compared to the ignoring ISRA solution
  t2 <- h4b %>%
    group_by(ISRAn) %>%
    filter(sol_noISRA == 1) %>%
    summarise(
      total_cost = sum(cost, na.rm = TRUE),
      PUs = n()) %>%
    mutate(cost_per_PU = total_cost/PUs)

  t3 = t2 %>% left_join(t %>% select(ISRAn, PUs) %>% rename(totalPUs = PUs), by = "ISRAn") %>%
    mutate(perc_area = 100/totalPUs*PUs)


## Which ISRA has high importance score in the solution without ISRAs
  ferrier_s1 <- extract_importance_score_data(s1, p1, method = "Ferrier") %>%
    select(cellID, score, geometry)

  ferrier_s1 = ferrier_s1  %>%
    sf::st_drop_geometry()  %>%
    left_join(as.data.frame(dat1d %>% select(cellID, cost)), by = "cellID") %>%
    mutate(ISRAn = dat_isra$ISRA_ISRA_names)


  fe1 = ferrier_s1 %>%
    separate_rows(ISRAn, sep = ",") %>%
    mutate(ISRAn = str_trim(ISRAn))

  # Group by the individual ISRA names and summarise
  fe1 <- fe1 %>%
    group_by(ISRAn) %>%
    summarise(
      total_score = sum(score, na.rm = TRUE),
      PUs = n()) %>%
    mutate(score_per_PU = total_score/PUs)

  ggplot(ferrier_s1) +
    geom_boxplot(aes(x = ISRAn, y = score))


  ## And Ferrier score inside vs outside ISRAs in solution with ISRAs
  ferrier_s2 <- extract_importance_score_data(s2, p2, method = "Ferrier") %>%
    select(cellID, score, geometry) %>%
    left_join(dat %>% select(cellID, isra) %>% st_drop_geometry(), by = "cellID")

  ferrier_s2 %>% st_drop_geometry() %>%
    group_by(isra) %>% summarise(mean_score = mean(score))

  f_ig_ISRA = ferrier_s2 %>% filter(isra == 0)
  f_in_ISRA = ferrier_s2 %>% filter(isra == 1)
  t.test(f_ig_ISRA$score, f_in_ISRA$score)



## Plot cost, importance, and size
  pdata <- t %>% left_join(fe1 %>% dplyr::select(!PUs), by = "ISRAn") %>%
    filter(ISRAn != "")

  # Some have 0 and thus become Inf in the log10 call
  pdata$score_per_PU[pdata$score_per_PU == 0] <- 1e-6

    # Calculate medians for the quadrants
    x_median <- median(pdata$cost_per_PU, na.rm = TRUE)
      x_median <- log10(x_median)
    y_median <- median(pdata$score_per_PU, na.rm = TRUE)
      y_median <- log10(y_median)

    # Select which ones to label
    pdata2 = pdata %>% filter(ISRAn %in% c("Sirt Gulf", "Gaza", "Lagoon of Bizerte", "Amvrakikos Gulf", "Palmahim Brine Pools", "Corsica Canyons", "Tripolitania", "Jerba-Zarzis", "Kerkennah",
                                           "Strait of Sicily and Tunisian Plateau", "Balearic Islands", "Benidorm Island",
                                           "Ebro Delta", "Southern Adriatic Pit", "Latakia - Baniyas",  "Ligurian Sea", "Cilician Basin", "Tuscany Offshore Thumb", "Thracian Sea Shelf",
                                           "Strait of Messina", "Marmara Sea Shelf", "Danny Reef Palmahim", "Northwest Adriatic",  "Dalia Beach", "Gulf of Antalya", "Egadi Archipelago", "Santa Maria di Leuca",
                                           "Northeastern Sardinia", "Edremit Bay", "Costa Brava Canyons", "Eastern Corsica", "M'Diq & Cabo Negro", "Iskenderun and Mersin Bays"))

## Cost - importance ratio plot Fig. 5
  ggplot() +
    geom_rect(aes(xmin = x_median, xmax = Inf, ymin = y_median, ymax = Inf), fill = "lightgreen", alpha = 0.2) +  # Top-right
    geom_rect(aes(xmin = -Inf, xmax = x_median, ymin = y_median, ymax = Inf), fill = "darkgreen", alpha = 0.2) +  # Top-left
    geom_rect(aes(xmin = -Inf, xmax = x_median, ymin = -Inf, ymax = y_median), fill = "pink", alpha = 0.2) +  # Bottom-left
    geom_rect(aes(xmin = x_median, xmax = Inf, ymin = -Inf, ymax = y_median), fill = "red", alpha = 0.2) +  # Bottom-right

    geom_point(data = pdata, aes(x = log10(cost_per_PU), y = log10(score_per_PU), size = PUs), col = "darkgrey") +
    geom_point(data = pdata2, aes(x = log10(cost_per_PU), y = log10(score_per_PU), size = PUs)) +

    ggrepel::geom_text_repel(data = pdata2, aes(x = log10(cost_per_PU), y = log10(score_per_PU), label = ISRAn), vjust = -0.5, hjust = 0.4, size = 3) +
    geom_hline(yintercept = y_median, linetype = "dashed", color = "blue") +
    geom_vline(xintercept = x_median, linetype = "dashed", color = "blue") +
    scale_size_continuous(range = c(1, 10), breaks = scales::pretty_breaks(n = 5)) +

    ylab("Importance: log(Mean Ferrier Score)") + xlab("Cost: log(Mean Apparent Fishing Hours)") +
    labs(size = "Planning Units") +
    theme(
      legend.position = c(0.02, 0.05),
      legend.justification = c("left", "bottom"),
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.background = element_rect(fill = alpha("white", 0.75)),
      legend.key = element_rect(fill = NA),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(1, 1, 1, 1, "cm")
    ) +
    guides(size = guide_legend(override.aes = list(color = "black")))+
    annotate("text", x = 1, y = Inf, label = "High importance - low cost", hjust = 1.1, vjust = 1.5, size = 5, color = "black", fontface = "bold") +
    annotate("text", x = Inf, y = -5.9, label = "Low importance - high cost", hjust = -0.1, vjust = -0.5, size = 5, color = "black", fontface = "bold", angle = 90)


 ## Filter those in the best quadrant only
  subset = pdata %>% filter(log10(cost_per_PU) <= x_median, log10(score_per_PU) >= y_median)


### Plot both solutions and the comparison
  # Visualise the difference between the scenarios
  (ggComp <- splnr_plot_comparison(s1, s2) +
      scale_fill_manual(values = c("goldenrod", "#c6dbef", "darkblue")) +
      scale_colour_manual(values = c("goldenrod", "#c6dbef", "darkblue")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      splnr_gg_add(
        PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme
      ) +
      labs(fill = "Plan with ISRAs \ncompared to\nPlan ignoring ISRAs") +
      labs(title = bquote(bold("c") ~ "Comparison")) )


  # Fig. 2
  patchwork::wrap_plots(gg1 + theme(legend.position = "none", axis.text.x = element_blank()),
                        gg2 + theme(axis.text.x = element_blank()),
                        ggComp, ncol = 1, nrow = 3)



  # Stats on the difference
  CorrMat <- splnr_get_kappaCorrData(list(s1, s2),
                                     name_sol = c("soln1", "soln2"))
  (ggCorr <- splnr_plot_corrMat(CorrMat,
                                AxisLabels = c("Solution 1", "Solution 2")))

  df_comp = s1 %>% dplyr::select(cellID, solution_1) %>% st_drop_geometry() %>%
    left_join(s2 %>% dplyr::select(cellID, solution_1)  %>% rename(solution_2 = solution_1) %>% st_drop_geometry(), by = "cellID" )

  df_comp %>% filter(solution_1 == 1 & solution_2 == 1) %>% nrow()
    100/length(df_comp$cellID) * (df_comp %>% filter(solution_1 == 1 & solution_2 == 1) %>% nrow())

  ### Plot both importance score metrics
  patchwork::wrap_plots(ggFerrier1 + labs(title = "A) Ignoring ISRAs") + theme(legend.position = "none", axis.text.x = element_blank()),
                        ggFerrier2 + labs(title = "B) Including ISRAs"),
                        ncol = 1, nrow = 2)


## Visualise targets

  ## Scatter plot
  rdcols3 <- data.frame(
    IUCN = c("CR", "EN", "VU", "NT", "LC", "DD"),
    cols = c("#FF0000", "#FFA500", "#FFFF00", "#ADFF2F", "#008000", "#808080"),
    stringsAsFactors = FALSE)

  # Create a named vector for color mapping
  colour_map <- setNames(rdcols3$cols, rdcols3$IUCN)

  dfTarget_newC_labels = dfTarget_newC %>% filter(species %in% c("Raja montagui", "Taeniurops grabatus", "Centroscymnus coelolepis", "Sphyrna zygaena", "Prionaece glauca", "Lamna nasus",
                                                                 "Alopias vulpinus", "Odontaspis ferox", "Centrophorus uyato", "Mobular mobula",  "Galeus melastomus", "Hexanchus griseus",
                                                                 "Tetronarce nobiliana", "Raja polystigma", "Carcharhinus obscurus",  "Glaucostegus cemiculus", "Squatina squatina",
                                                                  "Mustelus asterias", "Myliobatis aquila"))
  # Fig. 4 A
  (p4A = ggplot(dfTarget_newC) +
    geom_point(aes(x = areakm2, y = RH_exceed1, col = IUCN), size = 4,  alpha = 0.5) +
    geom_point(aes(x = areakm2, y = RH_exceed2, col = IUCN), size = 4, , shape = 17) + # , shape = 17 Different shape for the second set of points
    geom_segment(aes(x = areakm2, xend = areakm2, y = RH_exceed1, yend = RH_exceed2, col = IUCN), alpha = 0.5) +
    ggrepel::geom_text_repel(data = dfTarget_newC_labels, aes(x = areakm2, y = RH_exceed2, label = species), vjust = -0.5, hjust = 0.4, size = 3, fontface = "italic") +
    scale_colour_manual(values = colour_map, breaks = iucn_order) +
    xlab(expression(paste("Range size (km"^2*")"))) +
    ylab("Excess of area selected above target (% of range)")  +
    labs(title = bquote(bold("a") ~ "Excess of area selected per species"), colour = "IUCN Category") +
      theme(legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.5))) )



  # By red list category
  dfTarget_newC %>% group_by(category) %>%
    summarise(species = length(category),
              meanarea1 = mean(RH_exceed1), sd(RH_exceed1),
              meanarea2 = mean(RH_exceed2), sd(RH_exceed2),
              p_value = t.test(RH_exceed1, RH_exceed2)$p.value) # t test to check significance


  df_long <- dfTarget_newC %>% select(-relative_held, -Rima_comments, -incidental, -IUCN_Global, -IUCN_Med) %>%
    filter(IUCN != "DD") %>%

    pivot_longer(cols = starts_with("RH_exceed"),
                 names_to = "Measurement",
                 values_to = "Value")
  # Calculate mean and median values
  means_df <- df_long %>%
    group_by(category, Measurement) %>%
    summarise(mean_value = mean(Value), .groups = 'drop')
  median_df <- df_long %>%
    group_by(category, Measurement) %>%
    summarise(median_value = median(Value), .groups = 'drop') %>%
    mutate(median_value_label = ifelse(median_value < 10 & median_value < 10,
                                       sprintf("%.2f", median_value),
                                       sprintf("%.1f", median_value)))

  # Get significance
  ttest_results <- df_long %>%
    group_by(category) %>%
    summarise(p_value = t.test(Value[Measurement == "RH_exceed1"], Value[Measurement == "RH_exceed2"])$p.value,
              .groups = 'drop') %>%
    mutate(significance = case_when(
      p_value < 0.05 ~ "*",
      TRUE ~ ""))

  # Join t-test results with the original data for annotation
  annotation_df <- df_long %>%
    distinct(category) %>%
    left_join(ttest_results, by = "category")


  # New Figure 4B
  (p4B =   ggplot(df_long, aes(x = category, y = Value, fill = category)) +
    geom_boxplot(aes(group = interaction(category, Measurement), alpha = ifelse(Measurement == "RH_exceed1", 0.6, 1))) +
    # Add mean values for relative_held1
    geom_text(data = median_df %>% filter(Measurement == "RH_exceed1"),
              aes(x = category, y = median_value, label = median_value_label),
              position = position_dodge(width = 0.75), hjust = 1.6, vjust = 1.3, size = 3, color = "black") +
    # Add mean values for relative_held2
    geom_text(data = median_df %>% filter(Measurement == "RH_exceed2"),
              aes(x = category, y = median_value, label = median_value_label),
              position = position_dodge(width = 0.75), hjust = -0.6, vjust = 1.3, size = 3, color = "black") +
    geom_text(data = annotation_df, aes(x = category, y = c(17, 12, 22, 0, 0), label = significance),
              size = 10, color = "red", vjust = 1) +
    scale_fill_manual(values = iucn_colours) +
    labs(title = bquote(bold("b") ~ "Excess of area selected when ignoring or including ISRAs"),
         x = "IUCN Category",
         y = "Excess of area selected above target (% of range)",
         fill = "IUCN Category") +
    theme(legend.position = "none")  +
    scale_alpha_identity() )

  patchwork::wrap_plots(p4A, p4B, ncol = 1, nrow = 2)

  png("high_res_plot.png", width = 787, height = 1068, unit = pixels,  res = 300)
  patchwork::wrap_plots(p4A, p4B, ncol = 1, nrow = 2)
  dev.off()


  ### Reviewer comment: make new figure for supps ####
  # Set taxonomic order
  taxonomic_order <- c("Shark", "Ray", "Chimaera")

  # Create custom facet labels
  facet_labels <- c("Shark" = "Sharks", "Ray" = "Rays", "Chimaera" = "C")

  # Prepare data for plotting
  plot_data <- dfTarget_newC %>%
    mutate(
      display_name = gsub("_", " ", feature),
      IUCN = factor(IUCN, levels = iucn_order),
      group = factor(cat, levels = taxonomic_order),
      target_pct = target * 100)

  # Create the plot with outlined bars and target markers
  stacked_plot <- ggplot() +
    # Layer 1: Scenario 2 values with IUCN colors but transparent
    geom_col(data = plot_data,
             aes(x = relative_held2,
                 y = reorder(display_name, desc(as.numeric(IUCN))),
                 fill = IUCN),
             alpha = 0.5, width = 0.8,
             color = "grey", linewidth = 0.3) +
    # Layer 2: Scenario 1 values with solid IUCN color on top
    geom_col(data = plot_data,
             aes(x = relative_held1,
                 y = reorder(display_name, desc(as.numeric(IUCN))),
                 fill = IUCN),
             width = 0.8,
             color = "grey", linewidth = 0.3) +
    # Layer 3: Target markers as thin bars
    geom_col(data = plot_data,
             aes(x = target_pct,
                 y = reorder(display_name, desc(as.numeric(IUCN)))),
             width = 0.8,
             fill = NA, color = "black", linewidth = 0.3) +
    # Simple facet by group with custom labels
    facet_grid(group ~ ., scales = "free_y", space = "free_y",
               labeller = labeller(group = facet_labels)) +
    scale_fill_manual(values = colour_map, breaks = iucn_order) +
    labs(x = "Area selected (% of range)",
         y = "",
         title = "Area selected by scenario",
         subtitle = "Solid bars: Scenario 1 (ignoring ISRAs), Extended transparent bars: Scenario 2 (including ISRAs)",
         fill = "IUCN Status") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "right",
      axis.text.y = element_text(size = 8, face = "italic"),
      strip.text = element_text(size = 11, face = "bold"),
      strip.background = element_rect(fill = "white")
    )

  print(stacked_plot)


### Cost reduction 1: Excluding the D2 and C4 criteria (Scenario 4) ####
  ### Scenario 1: ignoring ISRAs (no need to re-run) ####
  p1 <- prioritizr::problem(dat1d, features = features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_new$target) %>% #
    add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(as.logical(dat_mpas2$data)) %>%
    add_default_solver(gap = 0.01, verbose = FALSE)

  ## Solve it
  s1 <- solve(p1)

  ### Scenario 4: locking-in ISRAs ####
  p2B <- prioritizr::problem(datb, features = features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_new$target) %>%
    add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(as.logical(dat_isra2B$data)) %>%
    add_locked_in_constraints(as.logical(dat_mpas2$data)) %>%
    add_default_solver(gap = 0.01, verbose = FALSE)

  # Solve it
  s2B <- solve(p2B)

  # Plot solution
  ( gg2B <- splnr_plot_solution(s2B)  +
      splnr_gg_add(
        Bndry = Med03, overlay = landmass,
        colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme) )

  ## Visualise targets:
  do <- c("CR", "EN", "VU", "NT", "LC", "DD")
  dfTarget2B <- splnr_get_featureRep(s2B, p2B,
                                    climsmart = FALSE, solnCol = "solution_1") %>%
    left_join(Dict, by = c("feature" = "nameVariable")) %>%
    rename(cat = category) %>%
    mutate(category = factor(IUCN, levels = do)) %>%
    arrange(category)

  # Plot
  (ggTargetB <- splnr_plot_featureRep(df = dfTarget2B, category = Dict, categoryFeatureCol = IUCN,
                                     nr = 1, showTarget = TRUE) +
      scale_x_discrete(limits = dfTarget2[order(dfTarget2$category), ]$feature, labels = dfTarget2$species) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
            legend.position = "inside",
            legend.position.inside = c(0.02, 0.98),
            legend.justification = c(0, 1)) +
      scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )


  ## Check the importance scores
  # Ferrier Score
  ggFerrier2B <- splnr_plot_importanceScore(
    soln = s2B,
    pDat = p2B,
    method = "Ferrier",
    decimals = 4,
    legendTitle = "Ferrier Score") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)

  # Rarity Wighted Richness.
  ggRWR2B <- splnr_plot_importanceScore(
    soln = s2B,
    pDat = p2B,
    method = "RWR",
    decimals = 2,
    legendTitle = "Rarity Weighted \nRichness") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)

  (ggScores <- patchwork::wrap_plots(ggFerrier1B + ggRWR1B + ggFerrier2B + ggRWR2B))

  ## PUs and ISRAs
  # Number of PUs
  sum(datb$isra);  100/(length(datb$cellID))*sum(datb$isra) # ISRAs: 9.9% of PUs
  sum(dat$mpa);  100/(length(dat$cellID))*sum(dat$mpa) # MPAs: 0.22% of PUs
  length(datb$cellID) # PUs

  ### Compare scenarios ####

  ### Check area covered
  # Number of PUs
  sum(dat$isra);  100/(length(dat$cellID))*sum(dat$isra)
  sum(datb$isra);  100/(length(datb$cellID))*sum(datb$isra)
  sum(dat$mpa);  100/(length(dat$cellID))*sum(dat$mpa)
  length(datb$cellID)

  table(s1$solution_1); 100/length(s1$solution_1)*(table(s1$solution_1)[2]) # Scenario 1
  table(s2$solution_1); 100/length(s2$solution_1)*(table(s2$solution_1)[2]) # Scenario 2
  table(s2B$solution_1); 100/length(s2B$solution_1)*(table(s2B$solution_1)[2]) # Scenario 4

  # combine solution with isra and mpa information
  h1 <- left_join(as.data.frame(s1), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
  h2 <- left_join(as.data.frame(s2), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
  h2B <- as.data.frame(s2B)

  # How many selected PUs are NOT in locked-in areas:
  sum(h1$solution_1) - (h1 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
  sum(h2$solution_1) - (h2 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
  sum(h2B$solution_1) - (h2B %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())

  # How many selected PUs are in ISRAs:
  h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h1$isra)*(h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
  h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
  h2B %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow())


## Compare the costs
    tc = h1 %>% summarise(total_cost = sum(cost)) # overall cost
  h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
  h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
  h2B %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2B %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))

  ## Which ISRA is most expensive
  h4 = h2 %>% select(cellID, cost, solution_1, geometry, isra, mpa) %>%
    rename(sol_wISRA = solution_1) %>%
    mutate(ISRAn = dat_isra$ISRA_ISRA_names) %>%
    mutate(sol_noISRA = h1$solution_1)

  h4b <- h4 %>%
    separate_rows(ISRAn, sep = ",") %>%  # Split the ISRAn column by comma into multiple rows
    mutate(ISRAn = str_trim(ISRAn))      # Trim any leading/trailing white space from ISRA names

  # Group by the individual ISRA names and summarise
  # Cost of PUs for the ISRA solution
  t <- h4b %>%
    group_by(ISRAn) %>%
    filter(sol_wISRA == 1) %>%
    summarise(
      total_cost = sum(cost, na.rm = TRUE),
      PUs = n()) %>%
    mutate(cost_per_PU = total_cost/PUs)

  # Compared to the ignoring ISRA solution
  t <- h4b %>%
    group_by(ISRAn) %>%
    filter(sol_noISRA == 1) %>%
    summarise(
      total_cost = sum(cost, na.rm = TRUE),
      PUs = n()) %>%
    mutate(cost_per_PU = total_cost/PUs)

  ### Plot both solutions
  patchwork::wrap_plots(gg1, gg2, gg2b, ncol = 1, nrow = 2)

  # Visualise the difference between the scenarios
  (ggComp <- splnr_plot_comparison(s2, s2B) +
      scale_fill_manual(values = c("goldenrod", "grey90", "darkblue")) +
      scale_colour_manual(values = c("goldenrod", "grey90", "darkblue")) +
      splnr_gg_add(
        PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme
      ) +
      labs(fill = "Plan with ISRAs \ncompared to\nPlan ignoring ISRAs") )


  patchwork::wrap_plots(gg2 + labs(title = "Including ISRAs"),
                        gg2B + labs(title = "Subset of ISRAs"),
                        ggComp, ncol = 1, nrow = 3)



  # Stats on the difference: All vs Subset of ISRAs (Scenario 4)
  CorrMat <- splnr_get_kappaCorrData(list(s2, s2B),
                                     name_sol = c("soln1", "soln2"))
  (ggCorr <- splnr_plot_corrMat(CorrMat,
                                AxisLabels = c("Solution 1", "Solution 2")))

  # Subset of ISRAs (Scenario 4) vs no ISRAs
  CorrMat <- splnr_get_kappaCorrData(list(s1, s2B),
                                     name_sol = c("soln1", "soln2"))
  (ggCorr <- splnr_plot_corrMat(CorrMat,
                                AxisLabels = c("Solution 1", "Solution 2")))

  ### Plot both importance score metrics
  (ggScores <- patchwork::wrap_plots(ggFerrier2B + labs(title = "Subset of ISRAs") + theme(legend.position = "none", axis.text.x = element_blank()) +
                                       ggFerrier2 + labs(title = "Including ISRAs") + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
                                       ggRWR1 + theme(legend.position = "none") +
                                       ggRWR2B + theme(axis.text.y = element_blank())) )

  ## Visualise targets
  dfTarget1 <- splnr_get_featureRep(s1, p1, climsmart = FALSE, solnCol = "solution_1")
  dfTarget2 <- splnr_get_featureRep(s2, p2, climsmart = FALSE, solnCol = "solution_1")
  dfTarget2B <- splnr_get_featureRep(s2B, p2B, climsmart = FALSE, solnCol = "solution_1")

  dfTarget <- left_join(dfTarget1  %>% mutate(relative_held1 = relative_held),
                        dfTarget2 %>% dplyr::select(feature, relative_held) %>% rename(relative_held2 = relative_held))

  dfTarget <- dfTarget %>%
    left_join(Dict, by = c("feature" = "nameVariable")) %>%
    rename(cat = category) %>%
    mutate(category = factor(IUCN, levels = do)) %>%
    arrange(category, species)

  dfTarget %>% mutate(diff = relative_held2 - relative_held1) %>% filter(diff > 0) %>%
    summarise(positive_diff_count = n())


  # By red list category
  dfTarget %>% group_by(category) %>%
    summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
              meanarea2 = mean(relative_held2), sd(relative_held2),
              p_value = t.test(relative_held1, relative_held2)$p.value)

  dfTarget_threatened = dfTarget %>% mutate(threatened = ifelse(category %in% c("CR", "EN", "VU"), "Threatened", "Not threatened"))

  dfTarget_threatened %>%
    group_by(threatened) %>%
    summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
              meanarea2 = mean(relative_held2), sd(relative_held2),
              p_value = t.test(relative_held1, relative_held2)$p.value)

  dfTarget_threatenedT = dfTarget_threatened %>% filter(threatened == "Threatened")
  t.test(dfTarget_threatenedT$relative_held1, dfTarget_threatenedT$relative_held2)


### Cost reduction 2: By cost-importance analysis (Scenario 3) ####
  ### Scenario 1: ignoring ISRAs (no need to re-run) ####
  p1s <- prioritizr::problem(dat1d, features = features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_new$target) %>%
    add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(as.logical(dat_mpas2$data)) %>%
    add_default_solver(gap = 0.01, verbose = FALSE)

  ## Solve it
  s1s <- solve(p1s)

  ### Scenario 3: locking-in ISRAs ####
  # Set the problem
  p2C <- prioritizr::problem(datc, features = features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_new$target) %>% #
    add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(as.logical(dat_isra2C$data)) %>%
    add_locked_in_constraints(as.logical(dat_mpas2$data)) %>%
    add_default_solver(gap = 0.01, verbose = FALSE)

  # Solve it
  s2C <- solve(p2C)

  # Plot solution
  ( gg2C <- splnr_plot_solution(s2C, plotTitle = "")  +
      ggtitle(label = "", subtitle = bquote(bold("b") ~ "Including subset of ISRAs")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      splnr_gg_add(
        Bndry = Med03, overlay = landmass,
        colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme) +
      geom_label(aes(x = -653592.9, y = 5626014, label = "Selected\nArea: 27.3%\nCost: 15.6%"),
                 hjust = 0,  vjust = 1, size = 3.5) )

  ## Visualise targets:
  do <- c("CR", "EN", "VU", "NT", "LC", "DD")
  dfTarget2C <- splnr_get_featureRep(s2C, p2C,
                                     climsmart = FALSE, solnCol = "solution_1") %>%
    left_join(Dict, by = c("feature" = "nameVariable")) %>%
    rename(cat = category) %>%
    mutate(category = factor(IUCN, levels = do)) %>%
    arrange(category)

  # Plot
  (ggTargetC <- splnr_plot_featureRep(df = dfTarget2C, category = Dict, categoryFeatureCol = IUCN,
                                      nr = 1, showTarget = TRUE) +
      scale_x_discrete(limits = dfTarget2C[order(dfTarget2C$category), ]$feature, labels = dfTarget2C$species) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
            legend.position = "inside",
            legend.position.inside = c(0.02, 0.98),
            legend.justification = c(0, 1)) +
      scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )


  ## Importance scores
  # Ferrier Score
  ggFerrier2C <- splnr_plot_importanceScore(
    soln = s2C,
    pDat = p2C,
    method = "Ferrier",
    decimals = 4,
    legendTitle = "Ferrier Score") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)

  # Rarity Wighted Richness.
  ggRWR2B <- splnr_plot_importanceScore(
    soln = s2C,
    pDat = p2C,
    method = "RWR",
    decimals = 2,
    legendTitle = "Rarity Weighted \nRichness") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme)

  (ggScores <- patchwork::wrap_plots(ggFerrier1C + ggRWR1C + ggFerrier2C + ggRWR2C))

  ## PUs and ISRAs
  # Number of PUs
  sum(datc$isra);  100/(length(datc$cellID))*sum(datc$isra)
  sum(dat$mpa);  100/(length(dat$cellID))*sum(dat$mpa)
  length(datc$cellID) # PUs

  ### Compare scenarios ####

  ### Check area covered
  # Number of PUs
  sum(dat$isra);  100/(length(dat$cellID))*sum(dat$isra)
  sum(datc$isra);  100/(length(datc$cellID))*sum(datc$isra)
  sum(dat$mpa);  100/(length(dat$cellID))*sum(dat$mpa)
  length(datc$cellID)

  table(s1$solution_1); 100/length(s1$solution_1)*(table(s1$solution_1)[2]) # Scenario 1
  table(s2$solution_1); 100/length(s2$solution_1)*(table(s2$solution_1)[2]) # Scenario 2
  table(s2B$solution_1); 100/length(s2B$solution_1)*(table(s2B$solution_1)[2]) # Scenario 4
  table(s2C$solution_1); 100/length(s2C$solution_1)*(table(s2C$solution_1)[2]) # Scenario 3

  # combine solution with isra and mpa information
  h1 <- left_join(as.data.frame(s1), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
  h2 <- left_join(as.data.frame(s2), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
  h2B <- as.data.frame(s2B)
  h2C <- as.data.frame(s2C)

  # How many selected PUs are NOT in locked-in areas:
  sum(h1$solution_1) - (h1 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
  sum(h2$solution_1) - (h2 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
  sum(h2B$solution_1) - (h2B %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
  sum(h2C$solution_1) - (h2C %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())

  # How many selected PUs are in ISRAs:
  h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h1$isra)*(h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
  h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
  h2B %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2B$isra)*(h2B %>% filter(solution_1 == 1, isra == 1) %>% nrow())
  h2C %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2C$isra)*(h2C %>% filter(solution_1 == 1, isra == 1) %>% nrow())

  ## Compare the costs
  tc = h1 %>% summarise(total_cost = sum(cost)) # overall cost
  h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
  h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
  h2B %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2B %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
  h2C %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2C %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))

  ## Which ISRA is most expensive
  h4 = h2 %>% select(cellID, cost, solution_1, geometry, isra, mpa) %>%
    rename(sol_wISRA = solution_1) %>%
    mutate(ISRAn = dat_isra$ISRA_ISRA_names) %>%
    mutate(sol_noISRA = h1$solution_1)

  h4b <- h4 %>%
    separate_rows(ISRAn, sep = ",") %>%
    mutate(ISRAn = str_trim(ISRAn))

  h4c <- h4 %>%
    separate_rows(ISRAn, sep = ",") %>%
    mutate(ISRAn = str_trim(ISRAn))

  # Cost of PUs for the ISRA solution
  t <- h4c %>%
    group_by(ISRAn) %>%
    filter(sol_wISRA == 1) %>%
    summarise(
      total_cost = sum(cost, na.rm = TRUE),
      PUs = n()) %>%
    mutate(cost_per_PU = total_cost/PUs)

  # Compared to the ignoring ISRA solution
  t <- h4b %>%
    group_by(ISRAn) %>%
    filter(sol_noISRA == 1) %>%
    summarise(
      total_cost = sum(cost, na.rm = TRUE),
      PUs = n()) %>%
    mutate(cost_per_PU = total_cost/PUs)


  ### Plot both solutions
  patchwork::wrap_plots(gg1, gg2, gg2c, ncol = 1, nrow = 2)


  # Visualise the difference between the scenario 2 and 3
  (ggCompC <- splnr_plot_comparison(s2, s2C) +
      scale_fill_manual(values = c("goldenrod", "#c6dbef", "darkblue")) +
      scale_colour_manual(values = c("goldenrod", "#c6dbef", "darkblue")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      splnr_gg_add(
        PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme
      ) +
      labs(fill = "Plan with ISRAs \ncompared to\nPlan ignoring ISRAs") +
      labs(title = bquote(bold("c") ~ "Comparison")) )

  # re-plot the figure with all ISRAs included (but new title)
  ( gg2Ca <- splnr_plot_solution(s2, plotTitle = "")  +
      ggtitle(label = "", subtitle = bquote(bold("a") ~ "Including all ISRAs")) +
      geom_sf(data = landmass, fill = "grey70", color = "grey70") +
      splnr_gg_add(
        Bndry = Med03, overlay = landmass,
        colorOverlay = "grey70",
        cropOverlay = PUs, ggtheme = splnr_theme) +
      geom_label(aes(x = -653592.9, y = 5626014, label = "Selected\nArea: 27.1%\nCost: 34.1%"),
                 hjust = 0,  vjust = 1, size = 3.5) )

  # Supp Fig
  patchwork::wrap_plots(gg2Ca + theme(legend.position = "none", axis.text.x = element_blank()),
                        gg2C + theme(axis.text.x = element_blank()),
                        ggCompC, ncol = 1, nrow = 3)


  # Stats on the difference:
  CorrMat <- splnr_get_kappaCorrData(list(s2, s2C),
                                     name_sol = c("soln1", "soln2"))
  (ggCorr <- splnr_plot_corrMat(CorrMat,
                                AxisLabels = c("Solution 1", "Solution 2")))

  # Subset of ISRAs vs no ISRAs: 0.72
  CorrMat <- splnr_get_kappaCorrData(list(s1, s2C),
                                     name_sol = c("soln1", "soln2"))
  (ggCorr <- splnr_plot_corrMat(CorrMat,
                                AxisLabels = c("Solution 1", "Solution 2")))


  ### Plot both importance score metrics
  (ggScores <- patchwork::wrap_plots(ggFerrier2C + labs(title = "Subset of ISRAs") + theme(legend.position = "none", axis.text.x = element_blank()) +
                                       ggFerrier2 + labs(title = "Including ISRAs") + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
                                       ggRWR1 + theme(legend.position = "none") +
                                       ggRWR2C + theme(axis.text.y = element_blank())) )


  ## Visualise targets
  dfTarget1 <- splnr_get_featureRep(s1, p1, climsmart = FALSE, solnCol = "solution_1")
  dfTarget2 <- splnr_get_featureRep(s2, p2, climsmart = FALSE, solnCol = "solution_1")
  dfTarget2B <- splnr_get_featureRep(s2B, p2B, climsmart = FALSE, solnCol = "solution_1")

  dfTarget <- left_join(dfTarget1  %>% mutate(relative_held1 = relative_held),
                        dfTarget2 %>% dplyr::select(feature, relative_held) %>% rename(relative_held2 = relative_held))

  dfTarget <- dfTarget %>%
    left_join(Dict, by = c("feature" = "nameVariable")) %>%
    rename(cat = category) %>%
    mutate(category = factor(IUCN, levels = do)) %>%
    arrange(category, species)

  # By red list category
  dfTarget %>% group_by(category) %>%
    summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
              meanarea2 = mean(relative_held2), sd(relative_held2),
              p_value = t.test(relative_held1, relative_held2)$p.value)

  dfTarget_threatened = dfTarget %>% mutate(threatened = ifelse(category %in% c("CR", "EN", "VU"), "Threatened", "Not threatened"))

  dfTarget_threatened %>%
    group_by(threatened) %>%
    summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
              meanarea2 = mean(relative_held2), sd(relative_held2),
              p_value = t.test(relative_held1, relative_held2)$p.value)

  dfTarget_threatenedT = dfTarget_threatened %>% filter(threatened == "Threatened")
  t.test(dfTarget_threatenedT$relative_held1, dfTarget_threatenedT$relative_held2)


### Supplementary: By Depth ####
### 5A. Surface layer ####
  ### Ignoring ISRAs (Scenario 7) ####
## Solve the problem
  pSU1 <- prioritizr::problem(datSU, features = featuresSU, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target4SU$target) %>%
    add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(as.logical(dat_mpasSU2$data)) %>%
    add_default_solver(gap = 0.001, verbose = FALSE)

  ## Solve it
  sSU1 <- solve(pSU1)

  100/length(sSU1$solution_1)*table(sSU1$solution_1)[2]

## Plot solution
  (ggSU1 <- splnr_plot_solution(sSU1, plotTitle = "Epipelagic - Ignoring ISRAs") + # Plot title without "Solution"
      splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                   cropOverlay = PUs0m, ggtheme = splnr_theme) )

## Visualise targets:
  do <- c("CR", "EN", "VU", "NT", "LC", "DD")
  dfTarget <- splnr_get_featureRep(sSU1, pSU1,
                                   climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in%  c("isra", "mpa")) %>%
    left_join(Dict, by = c("feature" = "nameVariable")) %>%
    rename(cat = category) %>%
    mutate(category = factor(IUCN, levels = do)) %>%
    arrange(category)

  # Plot
  (ggTargetSU1 <- splnr_plot_featureRep(df = dfTarget, category = Dict, categoryFeatureCol = IUCN,
                                     nr = 1, showTarget = FALSE) +
      scale_x_discrete(limits = dfTarget[order(dfTarget$category), ]$feature, labels = dfTarget$species) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
            legend.position = "inside",
            legend.position.inside = c(0.02, 0.98),
            legend.justification = c(0, 1)) +
      scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )

## Check the importance scores
  # Ferrier Score
  ( ggFerrierSU1 <- splnr_plot_importanceScore(
    soln = sSU1, pDat = pSU1,
    method = "Ferrier", decimals = 4,
    legendTitle = "Ferrier Score") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs0m, ggtheme = splnr_theme) )

  ### Including ISRAs (Scenario 8) ####
 ## Solve the problem
 pSU2 <- prioritizr::problem(datSU, features = featuresSU, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target4SU$target) %>%
   add_boundary_penalties(penalty = BPoverall, data = med_bd) %>%
   add_binary_decisions() %>%
   add_locked_in_constraints(as.logical(dat_israSU2$data)) %>%
   add_locked_in_constraints(as.logical(dat_mpasSU2$data)) %>%
   add_default_solver(gap = 0.001, verbose = FALSE)

 ## Solve it
 sSU2 <- solve(pSU2)

 100/length(sSU2$solution_1)*table(sSU2$solution_1)[2]

 ## Plot solution
 (ggSU2 <- splnr_plot_solution(sSU2, plotTitle = "Epipelagic - Including ISRAs") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                  cropOverlay = PUs0m, ggtheme = splnr_theme) )

 ## Visualise targets:
 do <- c("CR", "EN", "VU", "NT", "LC", "DD")
 dfTarget <- splnr_get_featureRep(sSU2, pSU2,
                                  climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in%  c("isra", "mpa")) %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category)

 # Plot
 (ggTargetSU2 <- splnr_plot_featureRep(df = dfTarget, category = Dict, categoryFeatureCol = IUCN,
                                       nr = 1, showTarget = FALSE) +
     scale_x_discrete(limits = dfTarget[order(dfTarget$category), ]$feature, labels = dfTarget$species) +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
           legend.position = "inside",
           legend.position.inside = c(0.02, 0.98),
           legend.justification = c(0, 1)) +
     scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )

 ## Importance scores
 # Ferrier Score
 ( ggFerrierSU2 <- splnr_plot_importanceScore(
   soln = sSU2, pDat = pSU2,
   method = "Ferrier", decimals = 4,
   legendTitle = "Ferrier Score") +
   splnr_gg_add(
     PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
     cropOverlay = PUs0m, ggtheme = splnr_theme) )

  ### Comparing them ####

### Check area covered
# Number of PUs
 sum(datSU$isra);  100/(length(datSU$cellID))*sum(datSU$isra)
 sum(datSU$mpa);  100/(length(datSU$cellID))*sum(datSU$mpa)
  length(datSU$cellID)

 table(sSU1$solution_1); 100/length(sSU1$solution_1)*(table(sSU1$solution_1)[2])
 table(sSU2$solution_1); 100/length(sSU2$solution_1)*(table(sSU2$solution_1)[2])

 # combine solution with isra and mpa information
 h1 <- as.data.frame(sSU1)
 h2 <- as.data.frame(sSU2)

   # How many selected PUs are NOT in locked-in areas:
   sum(h1$solution_1) - (h1 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
   sum(h2$solution_1) - (h2 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())

   # How many selected PUs are in ISRAs:
   h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h1$isra)*(h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
   h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow())

 ## Compare the costs
    tc = h1 %>% summarise(total_cost = sum(cost)) # overall cost
   h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
   h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))

 # Cost of PUs for the ISRA solution
 t <- h4b %>%
   group_by(ISRAn) %>%
   filter(sol_wISRA == 1) %>%
   summarise(
     total_cost = sum(cost, na.rm = TRUE),
     PUs = n()) %>%
   mutate(cost_per_PU = total_cost/PUs)

 # Compared to the ignoring ISRA solution
 t <- h4b %>%
   group_by(ISRAn) %>%
   filter(sol_noISRA == 1) %>%
   summarise(
     total_cost = sum(cost, na.rm = TRUE),
     PUs = n()) %>%
   mutate(cost_per_PU = total_cost/PUs)

## Which ISRA has high importance score in the solution without ISRAs
 # Extract Ferrier scores and associated data
 ferrier_sSU1 <- extract_importance_score_data(sSU1, pSU1, method = "Ferrier") %>%
   select(cellID, score, geometry)

 ferrier_sSU1 = ferrier_sSU1  %>%
   sf::st_drop_geometry()  %>%
   left_join(as.data.frame(datSU %>% select(cellID, cost)), by = "cellID") %>%
   mutate(ISRAn = dat_israSU$ISRA_names)

 fSU1 = ferrier_sSU1 %>%
   separate_rows(ISRAn, sep = ",") %>%
   mutate(ISRAn = str_trim(ISRAn))

 # Group by the individual ISRA names and summarise
 fSU1 <- fSU1 %>%
   group_by(ISRAn) %>%
   summarise(
     total_score = sum(score, na.rm = TRUE),
     PUs = n()) %>%
   mutate(score_per_PU = total_score/PUs)


### Plot both solutions
 (ggCompSU <- splnr_plot_comparison(sSU1, sSU2) +
     scale_fill_manual(values = c("goldenrod", "grey90", "darkblue")) +
     scale_colour_manual(values = c("goldenrod", "grey90", "darkblue")) +
     splnr_gg_add(
       PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
       cropOverlay = PUs0m, ggtheme = splnr_theme
     ) +
     labs(fill = "Plan with ISRAs \ncompared to\nPlan ignoring ISRAs") )

 patchwork::wrap_plots(ggSU1 + labs(title = "Epipelagic - Without ISRAs"),
                       ggSU2 + labs(title = "Epipelagic - Including ISRAs"),
                       ggCompSU, ncol = 1, nrow = 3)


## Stats on the difference
 CorrMat <- splnr_get_kappaCorrData(list(sSU1, sSU2),
                                    name_sol = c("soln1", "soln2"))
 (ggCorr <- splnr_plot_corrMat(CorrMat,
                               AxisLabels = c("Solution 1", "Solution 2")))

## Visualise targets
 dfTargetSU1 <- splnr_get_featureRep(sSU1, pSU1, climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in% c("isra", "mpa"))
 dfTargetSU2 <- splnr_get_featureRep(sSU2, pSU2, climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in% c("isra", "mpa"))

 dfTargetSU <- left_join(dfTargetSU1  %>% mutate(relative_held1 = relative_held*100),
                       dfTargetSU2 %>% mutate(relative_held2 = relative_held*100) %>% dplyr::select(feature, relative_held2) )

 dfTargetSU <- dfTargetSU %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category, species)

 # 61 species with a higher relative held vs 10 species in the scenario ignoring ISRAs
 dfTargetCSU = dfTargetSU %>% mutate(diff = relative_held2 - relative_held1, diff1 = relative_held1-(target*100), diff2 = relative_held2-(target*100))
 dfTargetCSU %>% filter(diff > 0) %>%
   summarise(positive_diff_count = n())

 # By red list category
 dfTargetSU %>% group_by(category) %>%
   summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
             meanarea2 = mean(relative_held2), sd(relative_held2),
             p_value = t.test(relative_held1, relative_held2)$p.value)

 dfTargetSU_threatened = dfTargetSU %>% mutate(threatened = ifelse(category %in% c("CR", "EN", "VU"), "Threatened", "Not threatened"))

 dfTargetSU_threatened %>%
   group_by(threatened) %>%
   summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
             meanarea2 = mean(relative_held2), sd(relative_held2),
             p_value = t.test(relative_held1, relative_held2)$p.value)

 dfTargetSU_threatenedT = dfTargetSU_threatened %>% filter(threatened == "Threatened")
 t.test(dfTargetSU_threatenedT$relative_held1, dfTargetSU_threatenedT$relative_held2)


### 5B. Mesopelagic layer ####
  ### Boundary penalty ####
 med_bdMP <- boundary_matrix(datMP)
 med_bdMP <- rescale_matrix(med_bdMP)

 ## Preliminary penalties to try
 prelim_penalty <- seq(from = 0.00001, to = 0.005 , length.out = 6)

   # Then use the good options to run through full examples without the time-limit
   prelim_penalty = seq(from = 1.2896e-08, to = 1.4344e-08 , length.out = 3)

 ## Test them, using simplified method to reduce runtime
 p0MP <- prioritizr::problem(datMP, features = featuresMP, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target2MP$target) %>% # Medium target
   add_binary_decisions() %>%
   add_locked_in_constraints(as.logical(dat_mpasMP2$data)) %>%
   add_default_solver(verbose = FALSE)

 # generate preliminary prioritisations based on each penalty
 ## note that we specify a relaxed gap and time limit for the solver
 prelim_blended_results <- lapply(prelim_penalty, function(x) {
   s <-
     p0MP %>%
     add_boundary_penalties(penalty = x, data = med_bdMP) %>%
     add_default_solver(gap = 0.4, time_limit = 3 * 60) %>%
     solve()
   s <- data.frame(s = s$solution_1)
   names(s) <- with_options(list(scipen = 30), paste0("penalty_", x))
   s
 })

 prelim_blended_results <- cbind(
   datMP, do.call(bind_cols, prelim_blended_results))

 # preview results
 print(prelim_blended_results)

 ## Plot this
 long_data2MP <- prelim_blended_results %>%
   pivot_longer(cols = starts_with("penalty_"),
                names_to = "scenario",
                values_to = "value") %>%
   mutate(value = as.factor(value))

 # Create the plot using ggplot2 and facet_wrap
 ggplot(long_data2MP) +
   geom_sf(aes(fill = value, geometry = geometry), color = NA) +
   scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkblue")) +
   facet_wrap(~ scenario) +
   theme_minimal() +
   theme(legend.position = "right") +
   labs(title = "Penalty Scenarios", fill = "Value") +
   splnr_gg_add(
     Bndry = Med03, overlay = landmass,
     colorOverlay = "grey70",
     cropOverlay = PUs, ggtheme = splnr_theme)

 long_data2MP %>% mutate(value = as.numeric(as.character(value))) %>%
   group_by(scenario) %>%
   summarise(selected = sum(value), n = length(value)) %>%
   mutate(perc.selected = 100/(length(datMP$cost))*selected)

 ## Best boundary penalty for Mesopelagic
 BP_MP = 0.0035

  ### Ignoring ISRAs (Scenario 9) ####
 ## Solve the problem
  pMP1 <- prioritizr::problem(datMP, features = featuresMP, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target4MP$target) %>%
   add_boundary_penalties(penalty = BP_MP, data = med_bdMP) %>%
   add_binary_decisions() %>%
   add_locked_in_constraints(as.logical(dat_mpasMP2$data)) %>%
   add_default_solver(gap = 0.001, verbose = FALSE)

 ## Solve it
 sMP1 <- solve(pMP1)

 100/length(sMP1$solution_1)*table(sMP1$solution_1)[2]

 ## Plot solution
 (ggMP1 <- splnr_plot_solution(sMP1, plotTitle = "A) Mesopelagic - Ignoring ISRAs") + # Plot title without "Solution"
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                  cropOverlay = PUs0m, ggtheme = splnr_theme) )

 ggplot() +
   geom_sf(data = sMP1, aes(fill = log10(cost), col = log10(cost))) +
   splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                cropOverlay = PUs200m, ggtheme = splnr_theme)

 ## Visualise targets:
 do <- c("CR", "EN", "VU", "NT", "LC", "DD")
 dfTarget <- splnr_get_featureRep(sMP1, pMP1,
                                  climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in%  c("isra", "mpa")) %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category) %>%
   filter(!feature == "data")

 # Plot
 (ggTargetMP1 <- splnr_plot_featureRep(df = dfTarget, category = Dict, categoryFeatureCol = IUCN,
                                       nr = 1, showTarget = FALSE) +
     scale_x_discrete(limits = dfTarget[order(dfTarget$category), ]$feature, labels = dfTarget$species) +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
           legend.position = "inside",
           legend.position.inside = c(0.02, 0.98),
           legend.justification = c(0, 1)) +
     scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )

 ## Importance scores
 # Ferrier Score
 ( ggFerrierMP1 <- splnr_plot_importanceScore(
   soln = sMP1, pDat = pMP1,
   method = "Ferrier", decimals = 4,
   legendTitle = "Ferrier Score") +
   splnr_gg_add(
     PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
     cropOverlay = PUs0m, ggtheme = splnr_theme) )

  ### Including ISRAs (Scenario 10) ####
 ## Solve the problem
 pMP2 <- prioritizr::problem(datMP, features = featuresMP, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target4MP$target) %>%
   add_boundary_penalties(penalty = BP_MP, data = med_bdMP) %>%
   add_binary_decisions() %>%
   add_locked_in_constraints(as.logical(dat_israMP2$data)) %>%
   add_locked_in_constraints(as.logical(dat_mpasMP2$data)) %>%
   add_default_solver(gap = 0.001, verbose = FALSE)

 ## Solve it
 sMP2 <- solve(pMP2)

 100/length(sMP2$solution_1)*table(sMP2$solution_1)[2]

 ## Plot solution
 (ggMP2 <- splnr_plot_solution(sMP2, plotTitle = "B) Mesopelagic - Including ISRAs") +
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                  cropOverlay = PUs0m, ggtheme = splnr_theme) )

 ## Visualise targets:
 do <- c("CR", "EN", "VU", "NT", "LC", "DD")
 dfTarget <- splnr_get_featureRep(sMP2, pMP2,
                                  climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in%  c("isra", "mpa")) %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category) %>%
   filter(!feature == "data")

 # Plot
 (ggTargetMP2 <- splnr_plot_featureRep(df = dfTarget, category = Dict, categoryFeatureCol = IUCN,
                                       nr = 1, showTarget = FALSE) +
     scale_x_discrete(limits = dfTarget[order(dfTarget$category), ]$feature, labels = dfTarget$species) +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
           legend.position = "inside",
           legend.position.inside = c(0.02, 0.98),
           legend.justification = c(0, 1)) +
     scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )

 ## Importance scores
 # Ferrier Score
 ( ggFerrierMP2 <- splnr_plot_importanceScore(
   soln = sMP2, pDat = pMP2,
   method = "Ferrier", decimals = 4,
   legendTitle = "Ferrier Score") +
   splnr_gg_add(
     PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
     cropOverlay = PUs0m, ggtheme = splnr_theme) )

  ### Comparing them ####

 ### Check area covered
 # Number of PUs
 sum(datMP$isra);  100/(length(datMP$cellID))*sum(datMP$isra)
 sum(datMP$mpa);  100/(length(datMP$cellID))*sum(datMP$mpa)
  length(datMP$cellID)

 table(sMP1$solution_1); 100/length(sMP1$solution_1)*(table(sMP1$solution_1)[2])
 table(sMP2$solution_1); 100/length(sMP2$solution_1)*(table(sMP2$solution_1)[2])

 # combine solution with isra and mpa information
 h1 <- as.data.frame(sMP1)
 h2 <- as.data.frame(sMP2)

 # How many selected PUs are NOT in locked-in areas:
 sum(h1$solution_1) - (h1 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
 sum(h2$solution_1) - (h2 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())

 # How many selected PUs are in ISRAs:
 h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h1$isra)*(h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
 h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow())

 ## Compare the costs
  tc = h1 %>% summarise(total_cost = sum(cost)) # overall cost
 h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
 h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))

 ### Plot both solutions
 ggCompMP <- splnr_plot_comparison(sMP1, sMP2) +
     scale_fill_manual(values = c("goldenrod", "grey90", "darkblue")) +
     scale_colour_manual(values = c("goldenrod", "grey90", "darkblue")) +
   geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(
       PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
       cropOverlay = PUs0m, ggtheme = splnr_theme
     ) +
   labs(fill = "Plan with ISRAs \ncompared to\nPlan ignoring ISRAs") +
   labs(title = "C) Mesopelagic - Comparison")

 patchwork::wrap_plots(ggMP1 + theme(legend.position = "none", axis.text.x = element_blank()),
                       ggMP2 + theme(axis.text.x = element_blank()),
                       ggCompMP, ncol = 1, nrow = 3)


 ## Stats on the difference:
 (CorrMat <- splnr_get_kappaCorrData(list(sMP1, sMP2),
                                    name_sol = c("soln1", "soln2")) )

 ## Visualise targets
 dfTargetMP1 <- splnr_get_featureRep(sMP1, pMP1, climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in% c("isra", "mpa"))
 dfTargetMP2 <- splnr_get_featureRep(sMP2, pMP2, climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in% c("isra", "mpa"))

 dfTargetMP <- left_join(dfTargetMP1  %>% mutate(relative_held1 = relative_held*100),
                         dfTargetMP2 %>% mutate(relative_held2 = relative_held*100) %>% dplyr::select(feature, relative_held2) )

 dfTargetMP <- dfTargetMP %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category, species)

 # By red list category
 dfTargetMP %>% drop_na(category) %>%
   group_by(category) %>%
   summarise(
     meanarea1 = mean(relative_held1),
     sdarea1 = sd(relative_held1),
     meanarea2 = mean(relative_held2),
     sdarea2 = sd(relative_held2),
     p_value = ifelse(n() > 1, t.test(relative_held1, relative_held2)$p.value, NA) )

 dfTargetMP_threatened = dfTargetMP  %>% drop_na(category) %>% mutate(threatened = ifelse(category %in% c("CR", "EN", "VU"), "Threatened", "Not threatened"))

 dfTargetMP_threatened %>%
   group_by(threatened) %>%
   summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
             meanarea2 = mean(relative_held2), sd(relative_held2),
             p_value = t.test(relative_held1, relative_held2)$p.value)

 dfTargetMP_threatenedT = dfTargetMP_threatened %>% filter(threatened == "Threatened")
 t.test(dfTargetMP_threatenedT$relative_held1, dfTargetMP_threatenedT$relative_held2)

### 5C. Bathypelagic layer ####
 ### Boundary penalty ####
 med_bdBP <- boundary_matrix(datBP)
 med_bdBP <- rescale_matrix(med_bdBP)

 ## Preliminary penalties to try
 prelim_penalty <- seq(from = 1e-9, to = 1.00e-08 , length.out = 3)

 ## Test them, using simplified method to reduce runtime
 p0BP <- prioritizr::problem(datBP, features = featuresBP, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target2BP$target) %>%
   add_binary_decisions() %>%
   add_locked_in_constraints(as.logical(dat_mpasBP2$data)) %>%
   add_default_solver(verbose = FALSE)

 # generate preliminary prioritisations based on each penalty
 prelim_blended_results <- lapply(prelim_penalty, function(x) {
   s <-
     p0BP %>%
     add_boundary_penalties(penalty = x, data = med_bdBP) %>%
     add_default_solver(gap = 0.4, time_limit = 3 * 60) %>%
     solve()
   s <- data.frame(s = s$solution_1)
   names(s) <- with_options(list(scipen = 30), paste0("penalty_", x))
   s
 })

 prelim_blended_results <- cbind(
   datBP, do.call(bind_cols, prelim_blended_results))

 # preview results
 print(prelim_blended_results)

 ## Plot this
 long_data2BP <- prelim_blended_results %>%
   pivot_longer(cols = starts_with("penalty_"),
                names_to = "scenario",
                values_to = "value") %>%
   mutate(value = as.factor(value))

 ggplot(long_data2BP) +
   geom_sf(aes(fill = value, geometry = geometry), color = NA) +
   scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkblue")) +
   facet_wrap(~ scenario) +
   theme_minimal() +
   theme(legend.position = "right") +
   labs(title = "Penalty Scenarios", fill = "Value") +
   splnr_gg_add(
     Bndry = Med03, overlay = landmass,
     colorOverlay = "grey70",
     cropOverlay = PUs, ggtheme = splnr_theme)

 long_data2BP %>% mutate(value = as.numeric(as.character(value))) %>%
   group_by(scenario) %>%
   summarise(selected = sum(value), n = length(value)) %>%
   mutate(perc.selected = 100/(length(datMP$cost))*selected)

 ## Best boundary penalty for Bathypelagic
 BP_BP = 8e-9

 ### Ignoring ISRAs (Scenario 11) ####
 ## Solve the problem
 pBP1 <- prioritizr::problem(datBP, features = featuresBP, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target4BP$target) %>%
   add_boundary_penalties(penalty = BP_BP, data = med_bdBP) %>%
   add_binary_decisions() %>%
   add_locked_in_constraints(as.logical(dat_mpasBP2$data)) %>%
   add_default_solver(gap = 0.01, verbose = FALSE)

 ## Solve it
 sBP1 <- solve(pBP1)

 100/length(sBP1$solution_1)*table(sBP1$solution_1)[2]

 ## Plot solution
 (ggBP1 <- splnr_plot_solution(sBP1, plotTitle = "A) Bathypelagic - Ignoring ISRAs") + # Plot title without "Solution"
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                  cropOverlay = PUs0m, ggtheme = splnr_theme) )

 ## Visualise targets:
 do <- c("CR", "EN", "VU", "NT", "LC", "DD")
 dfTarget <- splnr_get_featureRep(sBP1, pBP1,
                                  climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in%  c("isra", "mpa")) %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category)

 # Plot
 (ggTargetBP1 <- splnr_plot_featureRep(df = dfTarget, category = Dict, categoryFeatureCol = IUCN,
                                       nr = 1, showTarget = FALSE) +
     scale_x_discrete(limits = dfTarget[order(dfTarget$category), ]$feature, labels = dfTarget$species) +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
           legend.position = "inside",
           legend.position.inside = c(0.02, 0.98),
           legend.justification = c(0, 1)) +
     scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )

 ## Importance scores
 # Ferrier Score
 (ggFerrierBP1 <- splnr_plot_importanceScore(
   soln = sBP1, pDat = pBP1,
   method = "Ferrier", decimals = 4,
   legendTitle = "Ferrier Score") +
   splnr_gg_add(
     PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
     cropOverlay = PUs0m, ggtheme = splnr_theme) )

 ### Including ISRAs (Scenario 12) ####
 ## Solve the problem
 pBP2 <- prioritizr::problem(datBP, features = featuresBP, cost_column = "cost") %>%
   add_min_set_objective() %>%
   add_relative_targets(target4BP$target) %>%
   add_boundary_penalties(penalty = BP_BP, data = med_bdBP) %>%
   add_binary_decisions() %>%
   add_locked_in_constraints(as.logical(dat_israBP2$data)) %>%
   add_locked_in_constraints(as.logical(dat_mpasBP2$data)) %>%
   add_default_solver(gap = 0.01, verbose = FALSE)

 ## Solve it
 sBP2 <- solve(pBP2)

 ## Plot solution
 (ggBP2 <- splnr_plot_solution(sBP2, plotTitle = "B) Bathypelagic - Including ISRAs") +
     geom_sf(data = landmass, fill = "grey70", color = "grey70") +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                  cropOverlay = PUs0m, ggtheme = splnr_theme) )

 splnr_plot_solution(sBP2, plotTitle = "Bathypelagic - Including ISRAs") +
   geom_sf(data = PUs1000m, fill = "red", colour = "red", alpha = 0.1) +
   splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                cropOverlay = PUs0m, ggtheme = splnr_theme)

 ## Visualise targets:
 do <- c("CR", "EN", "VU", "NT", "LC", "DD")
 dfTarget <- splnr_get_featureRep(sBP2, pBP2,
                                  climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in%  c("isra", "mpa")) %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category)

 # Plot
 (ggTargetBP2 <- splnr_plot_featureRep(df = dfTarget, category = Dict, categoryFeatureCol = IUCN,
                                       nr = 1, showTarget = FALSE) +
     scale_x_discrete(limits = dfTarget[order(dfTarget$category), ]$feature, labels = dfTarget$species) +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 10),
           legend.position = "inside",
           legend.position.inside = c(0.02, 0.98),
           legend.justification = c(0, 1)) +
     scale_fill_manual(values = c("CR" = rdcols[6], "EN" = rdcols[5], "VU" = rdcols[4], "NT" = rdcols[3], "LC" = rdcols[2], "DD" = rdcols[1])) )

 ## Importance scores
 # Ferrier Score
 ggFerrierBP2 <- splnr_plot_importanceScore(
   soln = sBP2, pDat = pBP2,
   method = "Ferrier", decimals = 4,
   legendTitle = "Ferrier Score") +
   splnr_gg_add(
     PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
     cropOverlay = PUs0m, ggtheme = splnr_theme)

 ### Comparing them ####

 ### Check area covered
 # Number of PUs
 sum(datBP$isra);  100/(length(datBP$cellID))*sum(datBP$isra)
 sum(datBP$mpa);  100/(length(datBP$cellID))*sum(datBP$mpa)
  length(datBP$cellID) # PUs

 table(sBP1$solution_1); 100/length(sBP1$solution_1)*(table(sBP1$solution_1)[2])
 table(sBP2$solution_1); 100/length(sBP2$solution_1)*(table(sBP2$solution_1)[2])

 # combine solution with isra and mpa information
 h1 <- as.data.frame(sBP1)
 h2 <- as.data.frame(sBP2)

 # How many selected PUs are NOT in locked-in areas:
 sum(h1$solution_1) - (h1 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
 sum(h2$solution_1) - (h2 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())

 # How many selected PUs are in ISRAs:
 h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h1$isra)*(h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow())
 h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(h2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow())

 ## Compare the costs
 tc = h1 %>% summarise(total_cost = sum(cost)) # overall cost
  h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
  h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))

 ### Plot both solutions
 ggCompBP <- splnr_plot_comparison(sBP1, sBP2) +
     scale_fill_manual(values = c("goldenrod", "grey90", "darkblue")) +
     scale_colour_manual(values = c("goldenrod", "grey90", "darkblue")) +
   geom_sf(data = landmass, fill = "grey70", color = "grey70") +
   splnr_gg_add(
       PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
       cropOverlay = PUs0m, ggtheme = splnr_theme
     ) +
   labs(fill = "Plan with ISRAs \ncompared to\nPlan ignoring ISRAs") +
   labs(title = "C) Bathypelagic - Comparison")

 patchwork::wrap_plots(ggBP1 + theme(legend.position = "none", axis.text.x = element_blank()),
                       ggBP2 + theme(axis.text.x = element_blank()) ,
                       ggCompBP, ncol = 1, nrow = 3)


 ## Stats on the difference
 (CorrMat <- splnr_get_kappaCorrData(list(sBP1, sBP2),
                                    name_sol = c("soln1", "soln2")) )

 ### Plot both importance score metrics
 patchwork::wrap_plots(ggFerrierBP1 + labs(title = "Bathypelagic - Without ISRAs") + theme(legend.position = "none", axis.text.x = element_blank()) +
                         ggFerrierBP2 + labs(title = "Bathypelagic - Including ISRAs") + theme(axis.text.x = element_blank(), axis.text.y = element_blank())
                         )

 ## Visualise targets
 dfTargetBP1 <- splnr_get_featureRep(sBP1, pBP1, climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in% c("isra", "mpa"))
 dfTargetBP2 <- splnr_get_featureRep(sBP2, pBP2, climsmart = FALSE, solnCol = "solution_1") %>% filter(!feature %in% c("isra", "mpa"))

 dfTargetBP <- left_join(dfTargetBP1  %>% mutate(relative_held1 = relative_held*100),
                         dfTargetBP2 %>% mutate(relative_held2 = relative_held*100) %>% dplyr::select(feature, relative_held2) )

 dfTargetBP <- dfTargetBP %>%
   left_join(Dict, by = c("feature" = "nameVariable")) %>%
   rename(cat = category) %>%
   mutate(category = factor(IUCN, levels = do)) %>%
   arrange(category, species)


 # By red list category
 dfTargetBP %>% drop_na(category) %>%
   group_by(category) %>%
   summarise(
     meanarea1 = mean(relative_held1),
     sdarea1 = sd(relative_held1),
     meanarea2 = mean(relative_held2),
     sdarea2 = sd(relative_held2),
     p_value = ifelse(n() > 1, t.test(relative_held1, relative_held2)$p.value, NA) )

 dfTargetBP_threatened = dfTargetBP  %>% drop_na(category) %>% mutate(threatened = ifelse(category %in% c("CR", "EN", "VU"), "Threatened", "Not threatened"))

 dfTargetBP_threatened %>%
   group_by(threatened) %>%
   summarise(meanarea1 = mean(relative_held1), sd(relative_held1),
             meanarea2 = mean(relative_held2), sd(relative_held2),
             p_value = t.test(relative_held1, relative_held2)$p.value)

 dfTargetBP_threatenedT = dfTargetBP_threatened %>% filter(threatened == "Threatened")
 t.test(dfTargetBP_threatenedT$relative_held1, dfTargetBP_threatenedT$relative_held2)


### All three solutions combined
# Ignoring ISRAs
 noI = sSU1 %>% select(cellID, solution_1, isra, mpa, cost) %>% rename(surfaceS = solution_1) %>%
   left_join(sMP1 %>% st_drop_geometry() %>% select(cellID, solution_1) %>% rename(mesoS = solution_1), by = "cellID") %>%
   left_join(sBP1 %>% st_drop_geometry() %>% select(cellID, solution_1) %>% rename(bathyS = solution_1), by = "cellID") %>%
   st_drop_geometry() %>%
   mutate(
     solution_1 = rowSums(select(., surfaceS, mesoS, bathyS), na.rm = TRUE)
   ) %>%
   left_join(sSU1 %>% select(cellID), by = "cellID") %>%
   st_as_sf()

 my_palette <- c("white", "snow2", "pink", "purple")  #
 my_palette <- c("snow2", "#E0FFFF", "#ADD8E6", "#4682B4")  #

 (ggnoI <- splnr_plot_solution(noI, plotTitle = "A) All Depth Layers Combined - Ignoring ISRAs") +
     scale_fill_manual(values = my_palette) +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                  cropOverlay = PUs0m, ggtheme = splnr_theme) )

 table(noI$solution_1)


# Including ISRAs
 withI = sSU2 %>% select(cellID, solution_1, isra, mpa, cost) %>% rename(surfaceS = solution_1) %>%
   left_join(sMP2 %>% st_drop_geometry() %>% select(cellID, solution_1) %>% rename(mesoS = solution_1), by = "cellID") %>%
   left_join(sBP2 %>% st_drop_geometry() %>% select(cellID, solution_1) %>% rename(bathyS = solution_1), by = "cellID") %>%
   st_drop_geometry() %>%
   mutate(
     solution_1 = rowSums(select(., surfaceS, mesoS, bathyS), na.rm = TRUE)
   ) %>%
   left_join(sSU2 %>% select(cellID), by = "cellID") %>%
   st_as_sf()

 (ggwithI <- splnr_plot_solution(withI, plotTitle = "B) All Depth Layers Combined - Including ISRAs") +
     scale_fill_manual(values = my_palette) +
     splnr_gg_add(Bndry = Med03, overlay = landmass, colorOverlay = "grey70",
                  cropOverlay = PUs0m, ggtheme = splnr_theme)  +
     labs(fill = "Depth Layers\nSelected")  )

 table(withI$solution_1)

 patchwork::wrap_plots(ggnoI + theme(legend.position = "none", axis.text.x = element_blank()),
                       ggwithI, ncol = 1, nrow = 2)

 # Difference
 (CorrMat <- splnr_get_kappaCorrData(list(noI, withI),
                                     name_sol = c("soln1", "soln2")) )
