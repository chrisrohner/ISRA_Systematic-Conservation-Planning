####  Important Shark and Ray Areas can inform conservation planning in the Mediterranean and Black Seas
####  Chris Rohner et al.
####  Script 1
####  Reliant on scrip 2 and 3 to get the data first

# Load libraries
library(spatialgridr)
library(spatialplanr)
library(tidyverse)
library(utils4mme)
library(sf)
library(prioritizr)
library(rcbc)
library(IUCNpalette)
library(viridis)
library(withr)
library(oceandatr)
theme_set(theme_bw())

# Key for Global Fishing Watch
key <- Sys.getenv("GFW_TOKEN")

### 1. Prepare area and mapping ####

  # Set the CRS
  cCRS <- "ESRI:54009"

  # Set the boundary: ISRA region 3
  Med03 <- st_read("../Data/ISRA Region 3 outline/ISRA_Region3_fixedgeometries.shp")  %>%
    sf::st_transform(cCRS)
  Bndry <- Med03

  # Land overlay
  landmass <- rnaturalearth::ne_countries(
    scale = "large",
    returnclass = "sf") %>%
    sf::st_transform(cCRS)

  # Planning units: Whole region, and then remove those that don't have any sharks
  PUs <- spatialgridr::get_grid(boundary = Med03, crs = cCRS, resolution = 10000, output = "sf_hex") %>% # 10 km resolution, hexagons
    dplyr::mutate(cellID = dplyr::row_number())

  noSharks <- readRDS("../Output/noSharks.rds")

  PUs <- PUs %>% filter(!cellID %in% c(noSharks$cellID))


## Mapping
  # Theme for plotting
  splnr_theme <- list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      text = ggplot2::element_text(size = 9, colour = "black"),
      axis.text = ggplot2::element_text(size = 9, colour = "black"),
      plot.title = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_blank()
    )
  )



### 2. Load data ####
# Get these from script #2

# Dictionary
  Dict <- read.csv("../Data/dict.csv") %>% arrange(nameVariable)
  Dict$species <- gsub("_", " ", Dict$nameVariable)
  species = Dict$species
  head(Dict)

# Feature layer
  dat_species <- readRDS("../Output/IUCN_SharkRayData.rds")
  feature_names <- splnr_featureNames(dat_species)

# ISRAs for Scenario 2
  dat_isra <- readRDS("../Output/dat_isra.rds")
  dat_isra2 <- readRDS("../Output/dat_isra2.rds")

    isra <- sf::st_read("../Data/isra_region03/isra_region03.geojson") %>%
      sf::st_wrap_dateline() %>%
      sf::st_zm() %>%
      sf::st_transform("ESRI:54009")

    # Remove the buffer zone for each ISRA
    isra = isra[!isra$newname == "recommended buffer",]
    isra$newname # 65 areas

  ## Subset of ISRAs without D2 and C4 for Scenario 3
    # D2: Strait of Sicily and Tunisian Plateau; Balearic Islands
    # C4: Strait of Gibraltar; Easter Gulf of Lion; Costa Brava Canyons; Strait of Messina

    dat_isra2B <- readRDS("../Output/dat_isra2B.rds")

  ## Best quadrant in cost-importance analysis ISRAs only for Scenario 4
    dat_isra2C <- readRDS("../Output/dat_isra2C.rds")


# MPAs
    mpas <- readRDS("../Output/mpas.rds")
    dat_mpas <- readRDS("../Output/dat_mpas.rds")
    dat_mpas2 <- readRDS("../Output/dat_mpas2.rds")

# Fishing:
    dat_fishing <- readRDS("../Output/fishingPUs.rds") %>% filter(!cellID %in% c(noSharks$cellID))
    c5 = readRDS("../Output/c5.rds")

# MEOW:
    datm <- readRDS("../Output/datm.rds")

## When ready, bind all the data together
  # Scenario  1 and 2
  dat <- dplyr::bind_cols(dat_species,
                          dat_isra2 %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(isra = data),
                          dat_mpas2 %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(mpa = data),
                          dat_fishing  %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(cost = ApparentFishingHrs)
                          )

  # Scenario 3
  datB <- dplyr::bind_cols(dat_species,
                          dat_isra2B %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(isra = data),
                          dat_mpas2 %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID)  %>% rename(mpa = data),
                          dat_fishing  %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(cost = ApparentFishingHrs))

  # Scenario 4
  datC <- dplyr::bind_cols(dat_species,
                           dat_isra2C %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(isra = data),
                           dat_mpas2 %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(mpa = data),
                           dat_fishing  %>%  sf::st_drop_geometry() %>% dplyr::select(-cellID) %>% rename(cost = ApparentFishingHrs))



### 3. Targets ####
  # Make specific targets based on IUCN status

  rdcols = iucn_palette(category="All", exclude=c("NE", "CO"))
  cols <- c("#808080", "#008000", "#ADFF2F", "#FFFF00", "#FFA500", "#FF0000")
  IUCN_statuses <- c("DD", "LC", "NT", "VU", "EN", "CR")

  # Create a data frame
  rdcols2 <- data.frame(IUCN = IUCN_statuses, cols = cols)

  # Initial targets
  catTarg2 <- c("CR" = 0.45, "EN" = 0.45, "VU" = 0.35, "NT" = 0.30, "LC" = 0.25 , "DD" = 0.3)

  target2 <- Dict %>%
    splnr_targets_byCategory(catTarg2, catName = "IUCN")

# Targets as per Rodrigues et al. 2004
  Dict2 = Dict  %>%
    mutate(spp_target_percentage_rodrigues = loglinear_interpolation(
    x = areakm2,
    coordinate_one_x = 1000000, # 1 mio km2 for species with wide distribution
    coordinate_one_y = 1,
    coordinate_two_x = 2751147, # Total planning area
    coordinate_two_y = 0.5) ) %>%

    mutate(spp_target_percentage_butchart = ifelse(
    Dict$areakm2 >= 10000000,
    (1000000 / Dict$areakm2) * 100,
    spp_target_percentage_rodrigues))


  ggplot(Dict2) +
      geom_line(aes(x = areakm2, y = spp_target_percentage_butchart)) +
    geom_point(aes(x = areakm2, y = spp_target_percentage_butchart))

  Dict2 = Dict2 %>%
    splnr_targets_byCategory(catTargNew_normal2, catName = "IUCN") %>%
    mutate(target_new = spp_target_percentage_butchart * target) %>%

    mutate(area_to_protect = areakm2*target_new)


  target_new <- Dict2 %>% rename(target_old = target, target = target_new) %>%
    mutate(target_cat = ifelse(spp_target_percentage_rodrigues < 1, "wide", "normal"))



### 4. Scenario 1 & 2: based on species distributions - one depth layer ####
# Scenario with just species distributions
  dat1 <- dat %>% dplyr::select(c(1:74, 77:78))
  features = feature_names

  # To check the boundary penalty
  # precompute the boundary data
  boundary_data <- boundary_matrix(dat1)

  # rescale boundary data
  boundary_data <- rescale_matrix(boundary_data)

  # With cost based on AIS and satellite data from GFW
  dat1d = dat1 %>% mutate(cost = c5$CombinedApparentFishingHrs )

  # Boundary penalty
  # Generate boundary length data for the planning units
  med_bd <- boundary_matrix(dat1d)
  med_bd <- rescale_matrix(med_bd) # manually re-scale the boundary length values

  # Medium targets, updated fishing cost:
  BPoverall = 0.005

### 5. Scenario 3: Without D2 and C4 ####
  datb = datB %>% mutate(cost = c5$CombinedApparentFishingHrs )

### 6. Scenario 4: ISRAs based on cost-importance analysis ####
  datc = datC %>% mutate(cost = c5$CombinedApparentFishingHrs )

    #
### 7. MEOW as feature, Scenarios 5 & 6 ####
# Also see separate script for Scenarios 5 & 6 with a boundary penalty

   head(datm)
  head(dat)
  dat6 <- left_join(datm %>% dplyr::select(cellID, MEOW), dat %>% dplyr::select(1:78) %>% st_drop_geometry(), by = join_by("cellID")) %>%
    mutate(cost = c5$CombinedApparentFishingHrs)

  head(target_new)
  MEOWs <- tibble(
    nameVariable = gsub(" ", "", unique(datm$MEOW)),
    category = "MEOW",
    IUCN = NA,
    target = 0.3,
    species = unique(datm$MEOW))


  target6 = MEOWs %>%
    dplyr::select(nameVariable, category, IUCN, target, species) %>%
    rbind(MEOWs)


### Importance score function ####
  extract_importance_score_data <- function(soln, pDat, method = "Ferrier") {
    # Ensure the inputs are valid
    assertthat::assert_that(inherits(soln, c("data.frame", "tbl_df", "tbl")),
                            inherits(pDat, c("R6", "ConservationProblem")),
                            is.character(method))
    assertthat::assert_that(method %in% c("Ferrier", "RWR", "RC"))

    soln <- soln %>% tibble::as_tibble()

    if (!"cellID" %in% names(soln)) {
      stop("The 'soln' data frame must contain a 'cellID' column.")
    }

    if (method == "Ferrier") {
      cat("Calculating Ferrier Score.\n")
      scored_soln <- prioritizr::eval_ferrier_importance(pDat, soln[, "solution_1"])
      scored_soln <- scored_soln %>%
        dplyr::mutate(cellID = soln$cellID,
                      geometry = soln$geometry) %>%
        dplyr::rename(score = "total") %>%
        sf::st_as_sf()
    } else if (method == "RWR") {
      cat("Calculating Rarity Weighted Richness.\n")
      scored_soln <- prioritizr::eval_rare_richness_importance(pDat, soln[, "solution_1"]) %>%
        dplyr::mutate(cellID = soln$cellID,
                      geometry = soln$geometry) %>%
        dplyr::rename(score = "rwr") %>%
        sf::st_as_sf()
    } else if (method == "RC") {
      cat("Calculating Replacement Cost.\n")
      scored_soln <- prioritizr::eval_replacement_importance(pDat, soln[, "solution_1"]) %>%
        dplyr::mutate(cellID = soln$cellID,
                      geometry = soln$geometry) %>%
        dplyr::rename(score = "rc") %>%
        sf::st_as_sf()
    } else {
      stop("Invalid importance score method supplied.")
    }

    # Return the scored solution data with cellID
    return(scored_soln)
  }






### 8. By Depth, Scenarios 7-12 ####
  Bathy <- oceandatr::get_bathymetry(spatial_grid = PUs %>% select(cellID, geometry), classify_bathymetry = FALSE) %>%
    rename(old_cellID = cellID) %>% mutate(cellID = seq(1:length(PUs$cellID)))

  # Inshore (surface)
  costSU = c5 %>%
    rename(old_cellID = cellID) %>%
    mutate(cellID = seq(1:length(c5$vessel_count))) %>%
    dplyr::select(cellID, CombinedApparentFishingHrs) %>% st_drop_geometry()

  # Midshelf (mesopelagic)
  costMP = Bathy %>% st_drop_geometry() %>%
    mutate(cost = c5$CombinedApparentFishingHrs) %>%
    filter(bathymetry <= -200 ) %>%
    select(cellID, cost)

## Data frame
  # Inshore
  datSU = readRDS("../Output/datSU.rds")
  datSU = datSU %>% mutate(cost = costSU$CombinedApparentFishingHrs)

    featuresSU = splnr_featureNames(datSU)[1:71]

    ggplot(datSU) + geom_sf(aes(fill = log10(cost))) + scale_fill_viridis()

  # Midshelf
  datMP = readRDS("../Output/datMP.rds")
  datMP = datMP %>%
    left_join(costMP, by= "cellID")
  featuresMP = splnr_featureNames(datMP)[3:59]

    ggplot(datMP) + geom_sf(aes(fill = log10(cost))) + scale_fill_viridis()

  # Offshore
  datBP = readRDS("../Output/datBP.rds")
  datBP = datBP %>% dplyr::select(cellID, Carcharodon_carcharias, Centroscymnus_coelolepis, Cetorhinus_maximus, Chimaera_monstrosa, Dalatias_licha, Dipturus_nidarosiensis,
  Dipturus_oxyrinchus, Echinorhinus_brucus, Etmopterus_spinax, Galeus_melastomus, Hexanchus_griseus, Hydrolagus_mirabilis, Isurus_paucus, Lamna_nasus, Mobula_mobular, Odontaspis_ferox,
  Raja_clavata, Somniosus_rostratus, Sphyrna_lewini, Squalus_acanthias, Squalus_blainville, isra, mpa, geometry) %>%
    left_join(c5 %>% dplyr::select(cellID, CombinedApparentFishingHrs) %>% rename(cost = CombinedApparentFishingHrs) %>% st_drop_geometry(), by = "cellID") %>%
    mutate(cost = 1) # Equal-area cost because there is no fishing this deep

    featuresBP = splnr_featureNames(datBP)[1:21]


## Targets
  catTarg4 <- c("CR" = 0.35, "EN" = 0.35, "VU" = 0.25, "NT" = 0.20, "LC" = 0.15 , "DD" = 0.2)

  target4SU <- target_new %>% filter(!nameVariable %in% c("Hydrolagus_mirabilis", "Galeus_atlanticus"))

  target4MP <- target_new %>% filter(!nameVariable %in% c("Himantura_leoparda", "Glaucostegus_cemiculus", "Dasyatis_marmorata", "Dasyatis_tortonesei", "Rhinoptera_marginata", "Carcharhinus_limbatus",
                                                                    "Carcharhinus_brachyurus", "Squatina_squatina", "Aetomylaeus_bovinus", "Gymnura_altavela", "Rhinobatos_rhinobatos", "Mustelus_asterias",
                                                                    "Carcharhinus_brevipinna", "Sphyrna_zygaena", "Raja_undulata", "Dasyatis_pastinaca"))

  target4BP <- target_new %>% filter(nameVariable %in% c("Carcharodon_carcharias",   "Centroscymnus_coelolepis", "Cetorhinus_maximus",      "Chimaera_monstrosa",       "Dalatias_licha"  ,         "Dipturus_nidarosiensis",
                                                   "Dipturus_oxyrinchus",      "Echinorhinus_brucus"  ,    "Etmopterus_spinax"   ,     "Galeus_melastomus" ,       "Hexanchus_griseus" ,       "Hydrolagus_mirabilis"  ,   "Isurus_paucus"   ,
                                                   "Lamna_nasus"    ,          "Mobula_mobular"   ,        "Odontaspis_ferox"   ,      "Raja_clavata"  ,           "Somniosus_rostratus"   ,   "Sphyrna_lewini"    ,       "Squalus_acanthias" ,
                                                   "Squalus_blainville" ))


## ISRA layers
  dat_israSU = readRDS("../Output/dat_israSU.rds")
  dat_israMP = readRDS("../Output/dat_israMP.rds")
  dat_israBP = readRDS("../Output/dat_israBP.rds")

  dat_israSU2 = readRDS("../Output/dat_israSU2.rds")
  dat_israMP2 = readRDS("../Output/dat_israMP2.rds")
  dat_israBP2 = readRDS("../Output/dat_israBP2.rds")

## PUs
  PUs0m <- PUs %>%
    rename(old_cellID = cellID) %>%
    mutate(cellID = seq(1:length(PUs$cellID)))

  PUs200m <- Bathy %>%
    dplyr::filter(bathymetry <= -200) %>%
    dplyr::select(-bathymetry)

  PUs1000m <- Bathy %>%
    dplyr::filter(bathymetry <= -1000) %>%
    dplyr::select(-bathymetry)

## MPA layers
  dat_mpasSU2 = dat_mpas2 %>%
    rename(old_cellID = cellID) %>%
    mutate(cellID = seq(1:length(dat_mpas2$cellID)))


  dat_mpasMP2 = dat_mpas2 %>%
    rename(old_cellID = cellID) %>%
    mutate(cellID = seq(1:length(dat_mpas2$cellID))) %>%
    dplyr::select(cellID, data)  %>%

    filter(cellID %in% c(PUs200m$cellID))

  dat_mpasBP2 = dat_mpas2  %>%
    rename(old_cellID = cellID) %>%
    mutate(cellID = seq(1:length(dat_mpas2$cellID))) %>%
    dplyr::select(cellID, data)  %>%
    filter(cellID %in% c(PUs1000m$cellID))




