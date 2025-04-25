####  Important Shark and Ray Areas can inform conservation planning in the Mediterranean and Black Seas
####  Chris Rohner et al.
####  Script 2
####  Organise data

library(spatialgridr)
library(spatialplanr) 
library(tidyverse)
library(utils4mme) 
library(sf)

key <- Sys.getenv("GFW_TOKEN")

# CBC solver
# Installation: check here https://github.com/coin-or/Cbc and https://dirkschumacher.github.io/rcbc/

#### Planning units ####
## Set the CRS
cCRS <- "ESRI:54009"

## Set the boundary: ISRA region 3
Med03 <- st_read("../Data/ISRA Region 3 outline/ISRA_Region3_fixedgeometries.shp")  %>%
  sf::st_transform(cCRS)

ggplot() + geom_sf(data = Med03) +
  theme_bw()

# Get area (km2)
st_area(Med03)/1000000

# For plotting later
landmass <- rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf") %>%
  sf::st_transform(cCRS)

## Get planning units (PUs) within the region
PUs <- spatialgridr::get_grid(boundary = Med03, crs = cCRS, resolution = 10000, output = "sf_hex") %>% # 10 km resolution, hexagons
  dplyr::mutate(cellID = dplyr::row_number())

  ## Some PUs have no sharks in the Black Sea - remove them from planning region
  noSharks = dsp %>% # From later in script #4
    filter(diversity == 0)
  length(noSharks$cellID) 

  write_rds(noSharks, "../Output/noSharks.rds")

  PUs <- PUs %>% filter(!cellID %in% c(noSharks$cellID))

# Plot to check
ggplot() +
  geom_sf(data = Med03) +
  geom_sf(data = PUs, linewidth = 0.1) +
  theme_bw()


#### Features (chondrichthyan species ranges) ####
## Load the species distributions:
  # 2 chimearas, 31 rays, 40 sharks
  # directly from here, or do it again below

dat_species <- readRDS("..Output/IUCN_SharkRayData.rds")

Species_shp_list <- list.files("../data/species ranges IUCN", pattern = ".shp$", recursive = TRUE, full.names = TRUE)

# The name of the species is BINOMIAL for most shapefiles, but in others it is SCI_NAME: enter all species names under BINOMIAL
  Shapefile_of_all_species = purrr::map_df(Species_shp_list, sf::st_read) %>%
    dplyr::filter(PRESENCE == 1) %>%
    dplyr::mutate(sci_name = coalesce(BINOMIAL, SCI_NAME)) %>%
  dplyr::select("sci_name", "BINOMIAL", "PRESENCE", "ORIGIN", "SEASONAL", "YEAR", "COMPILER", "CITATION", "geometry") %>%
  sf::st_wrap_dateline() %>%
  sf::st_transform(cCRS) %>%  # Transform the data to have the same crs as the PUs
  dplyr::mutate(sci_name = str_replace(sci_name," Iata", " lata"),
                sci_name = str_replace(sci_name," grabata", " grabatus")) %>%
  dplyr::filter(!(sci_name == "Carcharias taurus" & YEAR == 2020)) %>%
  dplyr::filter(!(sci_name == "Heptranchias perlo" & YEAR == 2020)) %>%
  dplyr::mutate(sci_name = str_replace_all(sci_name," ", "_")) %>%
  sf::st_crop(Med03) %>%
  sf::st_make_valid()
#

dat_species <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                              dat = Shapefile_of_all_species,
                                              feature_names = "sci_name",
                                              cutoff = 0.5)


  names(dat_species) <- str_replace_all(names(dat_species), " ", "_") 
  dat_species <- dat_species %>% mutate(cellID = row_number())  %>%
  dplyr::select(cellID, everything())

write_rds(dat_species, "../Output/IUCN_SharkRayData.rds")

dat = dat_species


#### Add MEOW (Marine Ecoregions of the world) ####
datm <- utils4mme::match_MEOW(dat) %>%
  dplyr::select(-c("Province", "Realm", "LatZone", "Area_MEOW")) %>%
  dplyr::rename(MEOW = EcoRegion)

ggplot() + geom_sf(data = datm, aes(fill = MEOW) )+
  geom_sf(data = PUs, fill = NA, colour = "black", linewidth = 0.01)

write_rds(datm, "../Output/datm.rds")


#### Add ISRAs ####
isra <- sf::st_read("../Data/isra_region03/isra_region03.geojson") %>%
  sf::st_wrap_dateline() %>%
  sf::st_zm() %>%
  sf::st_transform("ESRI:54009") 

# Remove the buffer zone for each ISRA
isra = isra[!isra$newname == "recommended buffer",]
isra$newname # 65 areas

# Some species names mistakes
species <- strsplit(isra$Species, ",") %>%
  unlist() %>%  gsub("<.*?>", "", .) %>%
  trimws()

head(species)
table(species)

isra$Species <- gsub("\\bRaja rádula\\b", "Raja radula", isra$Species)

# Convert the depth columns to numeric
isra$MaxDepth = as.numeric(gsub(",","",isra$MaxDepth))
isra$MinDepth = as.numeric(gsub(",","",isra$MinDepth))

table(isra$DepthLayer)

ggplot() +  geom_sf(data = Med03, fill = "white") +
  geom_sf(data = isra, fill = alpha("goldenrod", 0.2), col = "goldenrod")  +
  geom_sf(data = PUs, fill = NA, colour = "black", linewidth = 0.01) +
  theme_bw()


# Add the PUs
dat_isra <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                           dat = isra,
                                           feature_names = "layer",
                                           cutoff = 0.001) %>% 
  sf::st_drop_geometry()


  t = dat_isra %>% mutate(row_sum = rowSums(select(., 2:66), na.rm = TRUE)) %>%
    mutate(PUswISRAs = ifelse(row_sum > 1, 1, row_sum))
  sum(t$PUswISRAs)

  100/length(t$PUswISRAs)* sum(t$PUswISRAs) 
  100/st_area(Med03)*(sum(st_area(isra))) 
  table(dat_isra$ISRA_names)
  unique(dat_isra$ISRA_names)


  isra_columns <- names(dat_isra)[2:66]

  # Create a new column with the ISRA names concatenated where the value is 1
  dat_isra <- dat_isra %>%
    mutate(ISRA_names = apply(select(., all_of(isra_columns)), 1, function(row) {
      paste(names(row)[row == 1], collapse = ", ")
    }))

# Second version for locking in all ISRAs as one
dat_isra2 <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                            dat = isra,
                                            feature_names = NULL,
                                            cutoff = 0.001)
sum(dat_isra2$data)


# Clean up names
names(dat_isra) <- str_remove_all(names(dat_isra), "-")
names(dat_isra) <- str_remove_all(names(dat_isra), "–")
names(dat_isra) <- str_remove_all(names(dat_isra), "  ")
names(dat_isra) <- str_replace_all(names(dat_isra), " ", "_") 
names(dat_isra) <- stringr::str_c("ISRA_", names(dat_isra))
dat_isra = dat_isra %>% rename(cellID = 1)


write_rds(dat_isra, "../Output/dat_isra.rds")
write_rds(dat_isra2, "../Output/dat_isra2.rds")

dat <- dplyr::bind_cols(dat_species, dat_isra)
write_rds(dat, "../Output/IUCN_SharkRayData_wEEZ_wISRA.rds")


# Scenario 3 without C4 and D2
israB = isra %>% filter(!newname %in% c("Strait of Sicily and Tunisian Plateau", "Balearic Islands", "Strait of Gibraltar", "Eastern Gulf of Lion", "Costa Brava Canyons", "Strait of Messina"))
dat_isra2B <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                            dat = israB,
                                            feature_names = NULL,
                                            cutoff = 0.001)

write_rds(dat_isra2B, "../Output/dat_isra2B.rds")


# Scenario 4 with only those in the best cost-importance quadrants
israC = isra %>% filter(newname %in% c("Amvrakikos Gulf", "Balearic Islands", "Cala Vella Mallorca", "Corsica Conayons", "Eastern Corsica", "Eastern Gulf of Lion", "Formentera Island", "Gaza",
                                       "Jerba-Zarzis", "Kerkennah", "Lagoon of Bizerte", "M'Diq & Cabo Negro", "Petit to Grand Rhône Canyon Heads", "Ras Akrata – Cap Tenes",
                                       "Santa Maria di Leuca", "Sirt Gulf", "Southeastern Aegean Sea", "Strait of Sicily and Tunisian Plateau", "Sulcis", "Tripolitania", "Western Apulian Coast"))
dat_isra2C <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                             dat = israC,
                                             feature_names = NULL,

                                             cutoff = 0.001)

write_rds(dat_isra2C, "../Output/dat_isra2C.rds")


#### Add Protected Areas ####
# MAPAMED for the Med
MPA_shp1 = st_read("../Data/MPAs in Med/MAPAMED-FINAL.shp")

# Protected Planet (March 2024) for the Black Sea
MPA_shp2 = st_read("../Data/WDPA_BlackSea/WDPA0_subset.shp")
MPA_shp3 = st_read("../Data/WDPA_BlackSea/WDPA1_subset.shp")
MPA_shp4 = st_read("../Data/WDPA_BlackSea/WDPA2_subset.shp")

  MPA_shp2 <- st_transform(MPA_shp2, st_crs(MPA_shp1))
  MPA_shp3 <- st_transform(MPA_shp3, st_crs(MPA_shp1))
  MPA_shp4 <- st_transform(MPA_shp4, st_crs(MPA_shp1))

  # Merge the Black Sea shapefiles into one
  merged_MPA_shp <- rbind(MPA_shp2, MPA_shp3, MPA_shp4)
  table(merged_MPA_shp$IUCN_CAT)


# Subset the protected areas: Categories 1-3 (=no-take)
fully_protected1 = MPA_shp1 %>% filter(IUCN_CAT_E %in% c("Ia - Strict nature reserve", "Ib - Wilderness area", "II - National park", "III - Natural monument or feature"))
fully_protected2 = merged_MPA_shp %>% filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III"))
  # Med:
  100/sum(st_area(PUs))*sum(st_area(fully_protected1)) # 0.1%
  # Black Sea
  100/sum(st_area(PUs))*sum(st_area(fully_protected2)) # 0.03%

# Plot the result
ggplot() +
  geom_sf(data = PUs, linewidth = 0.1) +

  geom_sf(data = fully_protected1, fill = "darkgreen") +
  geom_sf(data = fully_protected2, fill = "darkgreen") +
  theme_minimal()

names(fully_protected1)
names(fully_protected2)

# Merge the two
fp1 = fully_protected1 %>% dplyr::select(NAME, IUCN_CAT_E, geometry) %>% rename(IUCN_CAT = IUCN_CAT_E)
fp2 = fully_protected2 %>% dplyr::select(NAME, IUCN_CAT, geometry)
mpas <- rbind(fp1, fp2) %>% sf::st_transform("ESRI:54009")
  100/st_area(Med03)*sum(st_area(mpas)) # 0.13%
  100/sum(st_area(PUs))*sum(st_area(mpas)) # 0.14%
  write_rds(mpas, "../Output/mpas.rds")

# Add the PUs
dat_mpas <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                           dat = mpas,
                                           feature_names = "NAME",
                                           cutoff = 0.1) %>%
  sf::st_drop_geometry()


# Second one for locked-in layer
dat_mpas2 <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                            dat = mpas,
                                            feature_names = NULL,
                                            cutoff = 0.1)


write_rds(dat_mpas, "../Output/dat_mpas.rds")
write_rds(dat_mpas2, "../Output/dat_mpas2.rds")

dat <- dplyr::bind_cols(dat_species, dat_isra, dat_mpas)

rm(MPA_shp1)
rm(MPA_shp2)
rm(MPA_shp3)
rm(MPA_shp4)
rm(merged_MPA_shp)
rm(fully_protected1)
rm(fully_protected2)
rm(fp1)
rm(fp2)

#### GFW data####
  start_date <- "2013-01-01"
  end_date <- "2023-01-01"
  temp_res <- "yearly"
  spat_res <- "high"
  region_source <- "eez"


  # 28 range states minus Spain, Georgia, Russia and Malta which give errors, see below
  # Also minus Bosnia and Herzegovina (no data) and Palestine (no data)
  region <- c("France", "Monaco", "Italy", "Slovenia", "Croatia", "Montenegro",
              "Albania", "Greece", "Turkey", "Bulgaria", "Romania", "Ukraine",
              "Syria", "Lebanon", "Israel", "Cyprus",
              "Egypt", "Libya",  "Tunisia", "Algeria", "Morocco", "United Kingdom")



  fishing <- purrr::map(region, function(x) spatialplanr::splnr_get_gfw(x,
                                                                        start_date = start_date,
                                                                        end_date = end_date,
                                                                        temp_res = temp_res,
                                                                        spat_res = spat_res,
                                                                        region_source = region_source,
                                                                        compress = TRUE))



  ## Then get the countries that are difficult
  # First find the ID codes of the regions you want for each of the countries: Spain, Georgia, Russia and Malta
  gfwr::get_region_id(region_name = "France", region_source = "eez", key = gfwr::gfw_auth())
    # Spain: 48966, 5693: works
    # Georgia: 5678: works
    # Malta: 8365, 5685: works
    # Russia: 5690: not working
  # c(48966, 5693, 5678, 8365, 5685, 5690)


  # Then extract GFW data for those
  fishing2 <- spatialplanr::splnr_get_gfw(region = c(48966, 5693, 5678, 8365, 5685),
                                          start_date = start_date,
                                          end_date = end_date,
                                          temp_res = temp_res,
                                          spat_res = spat_res,
                                          region_source = region_source,
                                          cCRS = cCRS,
                                          compress = TRUE)

  # Combine the two
  fishing_combined <- bind_rows(fishing) %>%  sf::st_transform(cCRS) %>%
    rbind(fishing2)

  ggplot() +
    geom_sf(data = Med03) +
    geom_sf(data = fishing_combined, aes(fill = `ApparentFishingHrs`), colour = NA, linewidth = 0.0001) +
    theme_bw()


  # Extract fishing for PUs:
  fishing_PUs <- fishing_combined %>%
    dplyr::select(-"GFWregionID") %>%
    sf::st_interpolate_aw(to = PUs, extensive = TRUE, keep_NA = FALSE) 

  ## Not the same length as PUs, so extrapolate and add 0 to missing cellIDs
  fishing_PUs <- fishing_PUs %>%
    mutate(cellID = as.character(row.names(.)))

  fishing_PUs <- PUs %>% left_join(fishing_PUs %>%  sf::st_drop_geometry(), by = "cellID") %>%
    mutate(ApparentFishingHrs = if_else(is.na(ApparentFishingHrs), 0, ApparentFishingHrs))

  ggplot() +
    geom_sf(data = Med03) +
    geom_sf(data = fishing_PUs, aes(fill = ApparentFishingHrs, colour = ApparentFishingHrs)) +
    geom_sf(data = fishing_PUs %>% filter(ApparentFishingHrs == 0), fill = "grey", color = "grey") +
    scale_fill_viridis_c() + scale_colour_viridis_c() +
    theme_bw()

  write_rds(fishing_PUs, "Output/fishingPUs.rds")
  write_rds(fishing_combined, "Output/fishing_combined.rds")

  
## When ready we can try and bind all the data together
#dat <- dplyr::bind_cols(dat_species, dat_eez, dat_isra, dat_mpas, dat_fishing)


#### By Depth ####

## Get PUs within the region
  Bathy <- oceandatr::get_bathymetry(spatial_grid = PUs %>% select(cellID, geometry), classify_bathymetry = FALSE)

  ggplot() +
    geom_sf(data = Bathy, aes(fill = bathymetry), colour = NA) +
    scale_fill_viridis_c()

  PUs0m <- PUs

  PUs200m <- Bathy %>%
    dplyr::filter(bathymetry <= -200) %>%
    dplyr::select(-bathymetry)

  PUs1000m <- Bathy %>%
    dplyr::filter(bathymetry <= -1000) %>%
    dplyr::select(-bathymetry)

  ggplot() +
    geom_sf(data = PUs0m, fill = "goldenrod", colour = "goldenrod") +
    geom_sf(data = PUs200m, fill = "lightblue", colour = "lightblue") +
    geom_sf(data = PUs1000m, fill = "darkblue", colour = "darkblue")

## ISRAs
  isra <- sf::st_read("../Data/isra_region03/isra_region03.geojson") %>%
    sf::st_wrap_dateline() %>%
    sf::st_zm() %>%
    sf::st_transform("ESRI:54009") 

  # Remove the buffer zone for each ISRA
  isra = isra[!isra$newname == "recommended buffer",]
  isra$newname # 65 areas

  isra$Species <- gsub("\\bRaja rádula\\b", "Raja radula", isra$Species)

  # Convert the depth columns to numeric
  isra$MaxDepth = as.numeric(gsub(",","",isra$MaxDepth))
  isra$MinDepth = as.numeric(gsub(",","",isra$MinDepth))

  table(isra$DepthLayer)

  ## Surface layer (0-200 m) n = 57
  dat_israSU <- spatialgridr::get_data_in_grid(spatial_grid = PUs0m,
                                             dat = isra %>% filter(str_detect(DepthRange, "Epipelagic")),
                                             feature_names = "layer",
                                             cutoff = 0.001) %>%
                                             sf::st_drop_geometry()

  # Create a new column with the ISRA names concatenated where the value is 1
  isra_columnsSU <- names(dat_israSU)[2:58]

  dat_israSU <- dat_israSU %>%
    mutate(ISRA_names = apply(select(., all_of(isra_columnsSU)), 1, function(row) {
      paste(names(row)[row == 1], collapse = ", ")
    }))

  dat_israSU2 <- spatialgridr::get_data_in_grid(spatial_grid = PUs0m,
                                              dat = isra %>% filter(str_detect(DepthRange, "Epipelagic")),
                                              feature_names = NULL,
                                              cutoff = 0.001)

    write_rds(dat_israSU, "../Output/dat_israSU.rds")
    write_rds(dat_israSU2, "../Output/dat_israSU2.rds")


  ## Mesopelagic layer (201-1000 m) n = 29
  dat_israMP <- spatialgridr::get_data_in_grid(spatial_grid = PUs200m,
                                               dat = isra %>% filter(str_detect(DepthRange, "Mesopelagic")),
                                               feature_names = "layer",
                                               cutoff = 0.001) %>%
                                               sf::st_drop_geometry()

  # Create a new column with the ISRA names concatenated where the value is 1
  isra_columnsMP <- names(dat_israMP)[2:30]

  dat_israMP <- dat_israMP %>%
    mutate(ISRA_names = apply(select(., all_of(isra_columnsMP)), 1, function(row) {
      paste(names(row)[row == 1], collapse = ", ")
    }))


  dat_israMP2 <- spatialgridr::get_data_in_grid(spatial_grid = PUs200m,
                                                dat = isra %>% filter(str_detect(DepthRange, "Mesopelagic")),
                                                feature_names = NULL,
                                                cutoff = 0.001)

    write_rds(dat_israMP, "../Output/dat_israMP.rds")
    write_rds(dat_israMP2, "../Output/dat_israMP2.rds")


  ## Bathypelagic layer (>1,000 m) n = 9
  dat_israBP <- spatialgridr::get_data_in_grid(spatial_grid = PUs1000m,
                                               dat = isra %>% filter(str_detect(DepthRange, "Bathypelagic")) %>% filter(newname != "Eastern Gulf of Lion"),
                                               feature_names = "layer",
                                               cutoff = 0.001) %>%
                                               sf::st_drop_geometry()

  # Create a new column with the ISRA names concatenated where the value is 1
  isra_columnsBP <- names(dat_israBP)[2:10]

  dat_israBP <- dat_israBP %>%
    mutate(ISRA_names = apply(select(., all_of(isra_columnsBP)), 1, function(row) {
      paste(names(row)[row == 1], collapse = ", ")
    }))

  dat_israBP2 <- spatialgridr::get_data_in_grid(spatial_grid = PUs1000m,
                                                dat = isra %>% filter(str_detect(DepthRange, "Bathypelagic")) %>% filter(newname != "Eastern Gulf of Lion"),
                                                feature_names = NULL,
                                                cutoff = 0.001)

    write_rds(dat_israBP, "../Output/dat_israBP.rds")
    write_rds(dat_israBP2, "../Output/dat_israBP2.rds")


## Species:
  # Coastal
  datSU <- dplyr::bind_cols(dat_species %>% dplyr::select(!c(Hydrolagus_mirabilis, Galeus_atlanticus)),
                            dat_israSU2 %>% dplyr::select(data) %>% rename(isra = data) %>% st_drop_geometry(),
                            dat_mpas2 %>% dplyr::select(data) %>% rename(mpa = data) %>% st_drop_geometry() )

  write_rds(datSU, "../Output/datSU.rds")

  # Midshelf
  datMP <- dat_israMP2 %>% left_join(dat_species  %>% st_drop_geometry() %>%
                                       dplyr::select(!c(Himantura_leoparda, Glaucostegus_cemiculus, Dasyatis_marmorata, Dasyatis_tortonesei, Rhinoptera_marginata, Carcharhinus_limbatus,
                                                             Carcharhinus_brachyurus, Squatina_squatina, Aetomylaeus_bovinus, Gymnura_altavela, Rhinobatos_rhinobatos, Mustelus_asterias,
                                                             Carcharhinus_brevipinna, Sphyrna_zygaena, Raja_undulata, Dasyatis_pastinaca)), by = "cellID") %>%
    left_join(dat_israMP2 %>% dplyr::select(cellID, data) %>% rename(isra = data) %>% st_drop_geometry(), by = "cellID") %>%
    left_join(dat_mpas2 %>% dplyr::select(cellID, data) %>% rename(mpa = data) %>% st_drop_geometry(), by = "cellID" )

  write_rds(datMP, "../Output/datMP.rds")

  # Offshore
  datBP <- dat_israBP2 %>% left_join(dat_species  %>% st_drop_geometry() %>%
                                       dplyr::select(c(cellID, Odontaspis_ferox, Raja_clavata, Sphyrna_lewini, Mobula_mobular,  Echinorhinus_brucus, Dipturus_oxyrinchus , Cetorhinus_maximus,
                                                       Carcharodon_carcharias, Dipturus_nidarosiensis, Centrophorus_uyato, Squalus_blainville, Chimaera_monstrosa, Isurus_paucus, Dalatias_licha,
                                                       Lamna_nasus, Squalus_acanthias, Galeus_melastomus, Etmopterus_spinax,  Hydrolagus_mirabilis, Hexanchus_griseus, Somniosus_rostratus,
                                                       Centroscymnus_coelolepis)), by = "cellID") %>%
    left_join(dat_israBP2 %>% dplyr::select(cellID, data) %>% rename(isra = data) %>% st_drop_geometry(), by = "cellID") %>%
    left_join(dat_mpas2 %>% dplyr::select(cellID, data) %>% rename(mpa = data)  %>% st_drop_geometry(), by = "cellID" ) #%>%
   
  write_rds(datBP, "../Output/datBP.rds")

  
#### Other data: IMMAs and KBAs ####
imma <- st_read("../data/imma/iucn-imma.shp")  %>%
  st_make_valid() %>%
  filter(Region.Cod %in% c("BSCSEA", "MEDSEA"))

  imma <- imma[st_is_valid(imma), ] 
  imma <-  imma %>% sf::st_transform("ESRI:54009")

  ggplot() +
    geom_sf(data = imma, fill = "pink") +
    geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    splnr_gg_add(Bndry = Bndry,  overlay = landmass, colorOverlay = "grey70",  
                 cropOverlay = PUs, ggtheme = splnr_theme)

  dat_imma <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                             dat = imma,
                                             feature_names = NULL,
                                             cutoff = 0.5)

  dat_imma <- dat_imma %>% mutate(cellID = seq(1:length(data))) %>%
    left_join(Bathy %>% st_drop_geometry(), by = "cellID")

  write_rds(dat_imma, "../Output/dat_imma.rds")


kba <- st_read("../data/KBAsGlobal_2024_June_02_project/KBAsGlobal_2024_June_02_POL.shp") %>%
  sf::st_transform("ESRI:54009")

ggplot() +
  geom_sf(data = kba, fill = "purple") +
  geom_sf(data = landmass, fill = "grey70", color = "grey70") +
  splnr_gg_add(Bndry = Bndry,  overlay = landmass, colorOverlay = "grey70",  
               cropOverlay = PUs, ggtheme = splnr_theme)

dat_kba <- spatialgridr::get_data_in_grid(spatial_grid = PUs,
                                           dat = kba,
                                           feature_names = NULL,
                                           cutoff = 0.5)

dat_kba <- dat_kba %>% mutate(cellID = seq(1:length(data))) %>%
  left_join(Bathy %>% st_drop_geometry(), by = "cellID")

write_rds(dat_kba, "../Output/dat_kba.rds")

