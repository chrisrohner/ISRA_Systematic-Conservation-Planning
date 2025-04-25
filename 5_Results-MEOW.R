####  Important Shark and Ray Areas can inform conservation planning in the Mediterranean and Black Seas
####  Chris Rohner et al.
####  Script 4
####  MEOW scenarios with a boundary penalty


library(prioritizr)  # 
library(sf)          # 
library(dplyr)       #
library(tidyr)       # 
library(ggplot2)     # 
library(spatialplanr)   


#### Scenario 5 MEOW and ignoring ISRAs: Set-up loop ####
BPMEOW = 0.000000001
timeL = 600

run_prioritisation_BP <- function(MEOW_data, targets, mpas_subset, boundary_data) {
  # Get the row indices for this MEOW
  row_indices <- which(dat6$cellID %in% MEOW_data$cellID)
  
  # Subset the boundary data to match these planning units
  bd_subset <- boundary_data[row_indices, row_indices]
  
  # Create a problem using prioritizr
  p <- prioritizr::problem(MEOW_data, features = features_loop, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_loop$target) %>% 
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BPMEOW, data = bd_subset) %>% # Add boundary penalty
    add_locked_in_constraints(as.logical(mpas_subset$data)) %>% # lock in MPAs
    add_default_solver(gap = 0.001, time_limit = timeL, verbose = FALSE)
  
  ## Solve it
  s <- solve(p)
}

# And if no locked-in layer
run_prioritisationNL_BP <- function(MEOW_data, targets, boundary_data) {
  # Get the row indices for this MEOW
  row_indices <- which(dat6$cellID %in% MEOW_data$cellID)
  
  # Subset the boundary data to match these planning units
  bd_subset <- boundary_data[row_indices, row_indices]
  
  # Create a problem using prioritizr
  p <- prioritizr::problem(MEOW_data, features = features_loop, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_loop$target) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BPMEOW, data = bd_subset) %>% 
    add_default_solver(gap = 0.001, time_limit = timeL, verbose = FALSE)
  
  ## Solve it
  s <- solve(p)
}

#### Scenario 5 MEOW and ignoring ISRAs: Results ####
MEOW_list <- unique(dat6$MEOW)
MEOW_list <- MEOW_list[MEOW_list != "Saharan Upwelling"] # Remove this very small area

# Initialise a list to store results
results_list_BP <- list()

# Prepare the full boundary data once
med_bd <- boundary_matrix(dat6)

# Loop over each MEOW
for (meow in MEOW_list) {
  print(paste("Processing MEOW with boundary penalty:", meow))
  # Filter data for the current MEOW
  MEOW_data <- dat6 %>%
    filter(MEOW == meow)
  print(paste("Number of rows in MEOW_data:", nrow(MEOW_data)))
  
  # Filter dat_mpas2 for the current MEOW
  dat_mpas2_loop <- dat_mpas2 %>%
    mutate(cellID = seq(1:31801)) %>%
    filter(cellID %in% MEOW_data$cellID)
  print(paste("Number of rows in dat_mpas2_loop:", nrow(dat_mpas2_loop)))
  
  # Filter features
  feature_columns <- names(MEOW_data)[4:76]
  # Filter feature columns to keep only those with at least one '1'
  features_loop <- feature_columns[sapply(feature_columns, function(col) any(MEOW_data[[col]] == 1))]
  if (length(features_loop) == 0) {
    warning(paste("No valid features found for MEOW:", meow, "- Skipping this MEOW"))
    next
  }
  
  # Filter targets
  target_loop <- target_new %>%
    filter(nameVariable %in% features_loop)
  
  # Check if there are any MPAs for the current MEOW
  if (nrow(dat_mpas2_loop) == 0 || !any(as.logical(dat_mpas2_loop$data))) {
    warning(paste("No MPAs found for MEOW:", meow, "- Running alternative prioritization"))
    # Run alternative prioritization for the current MEOW
    solution <- run_prioritisationNL_BP(MEOW_data, target_loop, med_bd)
  } else {
    # Run standard prioritization for the current MEOW
    solution <- run_prioritisation_BP(MEOW_data, target_loop, dat_mpas2_loop, med_bd)
  }
  
  # Store the solution with MEOW information
  solution$MEOW <- meow
  results_list_BP[[meow]] <- solution
  
  # Print progress
  cat(paste("Completed", which(MEOW_list == meow), "of", length(MEOW_list), "MEOWs\n"))
}

## Combine all solutions into a single data frame
crBP2 <- do.call(rbind, results_list_BP)

# Calculate selection percentage
100/length(crBP2$solution_1)*table(crBP2$solution_1)[2] 
h1 <- left_join(as.data.frame(crBP2), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost))) 


## Plot it
(gge1BP = splnr_plot_solution(crBP2, plotTitle = "") +
    ggtitle(label = "", subtitle = bquote(bold("a") ~ "MEOW - Ignoring ISRAs")) +
    geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    geom_sf(data = isra, fill = NA, color = "#F16364", size = 1.5) +
    
    splnr_gg_add(
      Bndry = Med03, overlay = landmass,
      colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme) +
    geom_label(aes(x = -653592.9, y = 5626014, label = "Selected\nArea: 31.9%\nCost: 8.8%"),
               hjust = 0,  vjust = 1, size = 3.5) )


#### Scenario 6 MEOW and including ISRAs: Set up loop ####
BPMEOW = 0.000000001
timeL = 600

run_prioritisation_BP_with_ISRAs <- function(MEOW_data, targets, mpas_subset, boundary_data) {
  # Get the row indices for this MEOW
  row_indices <- which(dat6$cellID %in% MEOW_data$cellID)
  
  # Subset the boundary data to match these planning units
  bd_subset <- boundary_data[row_indices, row_indices]
  
  # Create a combined locked-in layer with both MPAs and ISRAs
  locked_in <- as.logical(mpas_subset$data) | as.logical(MEOW_data$isra)
  
  # Create a problem using prioritizr
  p <- prioritizr::problem(MEOW_data, features = features_loop, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_loop$target) %>% # Medium target
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BPMEOW, data = bd_subset) %>% # Add boundary penalty
    add_locked_in_constraints(locked_in) %>% # lock in both MPAs and ISRAs
    add_default_solver(gap = 0.001, time_limit = timeL, verbose = FALSE)
  
  ## Solve it
  s <- solve(p)
}

# And if no locked-in MPAs layer, but still using ISRAs
run_prioritisationNL_BP_with_ISRAs <- function(MEOW_data, targets, boundary_data) {
  # Get the row indices for this MEOW
  row_indices <- which(dat6$cellID %in% MEOW_data$cellID)
  
  # Subset the boundary data to match these planning units
  bd_subset <- boundary_data[row_indices, row_indices]
  
  # Create a problem using prioritizr
  p <- prioritizr::problem(MEOW_data, features = features_loop, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(target_loop$target) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BPMEOW, data = bd_subset) %>% # Add boundary penalty
    add_locked_in_constraints(as.logical(MEOW_data$isra)) %>% # lock in ISRAs
    add_default_solver(gap = 0.001, time_limit = timeL, verbose = FALSE)
  
  ## Solve it
  s <- solve(p)
}

#### Scenario 6 MEWO and including ISRAs: Result ####
MEOW_list <- unique(dat6$MEOW)
MEOW_list <- MEOW_list[MEOW_list != "Saharan Upwelling"] # Remove this very small area

# Initialize a list to store results
results_list_BP_ISRAs <- list()

# Prepare the full boundary data once
med_bd <- boundary_matrix(dat6)

# Loop over each MEOW
for (meow in MEOW_list) {
  print(paste("Processing MEOW with boundary penalty and ISRAs:", meow))
  # Filter data for the current MEOW
  MEOW_data <- dat6 %>%
    filter(MEOW == meow)
  print(paste("Number of rows in MEOW_data:", nrow(MEOW_data)))
  
  # Filter dat_mpas2 for the current MEOW
  dat_mpas2_loop <- dat_mpas2 %>%
    mutate(cellID = seq(1:31801)) %>%
    filter(cellID %in% MEOW_data$cellID)
  print(paste("Number of rows in dat_mpas2_loop:", nrow(dat_mpas2_loop)))
  
  # Filter features
  feature_columns <- names(MEOW_data)[4:76]
  # Filter feature columns to keep only those with at least one '1'
  features_loop <- feature_columns[sapply(feature_columns, function(col) any(MEOW_data[[col]] == 1))]
  if (length(features_loop) == 0) {
    warning(paste("No valid features found for MEOW:", meow, "- Skipping this MEOW"))
    next
  }
  
  # Filter targets
  target_loop <- target_new %>%
    filter(nameVariable %in% features_loop)
  
  # Check if there are any MPAs for the current MEOW
  if (nrow(dat_mpas2_loop) == 0 || !any(as.logical(dat_mpas2_loop$data))) {
    warning(paste("No MPAs found for MEOW:", meow, "- Running alternative prioritization"))
    # Run alternative prioritization for the current MEOW
    solution <- run_prioritisationNL_BP_with_ISRAs(MEOW_data, target_loop, med_bd)
  } else {
    # Run standard prioritization for the current MEOW
    solution <- run_prioritisation_BP_with_ISRAs(MEOW_data, target_loop, dat_mpas2_loop, med_bd)
  }
  
  # Store the solution with MEOW information
  solution$MEOW <- meow
  results_list_BP_ISRAs[[meow]] <- solution
  
  # Print progress
  cat(paste("Completed", which(MEOW_list == meow), "of", length(MEOW_list), "MEOWs\n"))
}

## Combine all solutions into a single data frame
crBP2_ISRAs <- do.call(rbind, results_list_BP_ISRAs)

# Calculate selection percentage
100/length(crBP2_ISRAs$solution_1)*table(crBP2_ISRAs$solution_1)[2] 
h1 <- left_join(as.data.frame(crBP2_ISRAs), as.data.frame(dat) %>% dplyr::select(cellID, isra, mpa), by = "cellID")
h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(h1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost))) 


## Plot it
(gge1BP_ISRAs = splnr_plot_solution(crBP2_ISRAs, plotTitle = "") +
    ggtitle(label = "", subtitle = bquote(bold("b") ~ "MEOW - Including ISRAs")) +
    geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    geom_sf(data = isra, fill = NA, color = "#F16364", size = 1.5) +
    
    splnr_gg_add(
      Bndry = Med03, overlay = landmass,
      colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme) +
    geom_label(aes(x = -653592.9, y = 5626014, label = "Selected\nArea: 36.2%\nCost: 37.1%"),
               hjust = 0,  vjust = 1, size = 3.5) )


### Compare them ####
table(crBP2$solution_1)[2]
crBP2

table(crBP2_ISRAs$solution_1)[2]
crBP2_ISRAs

# Visualise the difference between the scenarios
(ggComp_MEOW <- splnr_plot_comparison(crBP2, crBP2_ISRAs) +
    scale_fill_manual(values = c("goldenrod", "#c6dbef", "darkblue")) + 
    scale_colour_manual(values = c("goldenrod", "#c6dbef", "darkblue")) + 
    geom_sf(data = landmass, fill = "grey70", color = "grey70") +
    splnr_gg_add(
      PUs = NULL, Bndry = Bndry, overlay = landmass, colorOverlay = "grey70",
      cropOverlay = PUs, ggtheme = splnr_theme
    ) +
    labs(fill = "MEOW - Plan with ISRAs \ncompared to\nplan ignoring ISRAs") +
    labs(title = bquote(bold("c") ~ "Comparison")) )


#Fig. S5
patchwork::wrap_plots(gge1BP + theme(legend.position = "none", axis.text.x = element_blank()),
                      gge1BP_ISRAs + theme(axis.text.x = element_blank()),
                      ggComp_MEOW, ncol = 1, nrow = 3)


# Stats on the differences

# Stats on the difference:
(CorrMat <- splnr_get_kappaCorrData(list(crBP2, crBP2_ISRAs),
                                    name_sol = c("soln1", "soln2")) )


# how different is the solution from the overall solution: Without ISRAs
splnr_get_kappaCorrData(list(s1M, crBP2),
                        name_sol = c("soln1", "soln2"))

# how different is the solution from the overall solution: With ISRAs
splnr_get_kappaCorrData(list(s2M, crBP2_ISRAs),
                        name_sol = c("soln1", "soln2"))


## Area selected
# Per MEOW
ce1 <- crBP2 %>%
  group_by(MEOW) %>%
  summarise(PUs = length(solution_1), PUs_selected = sum(solution_1)) %>%
  mutate(percent_protected1 = 100/PUs*PUs_selected) %>%
  sf::st_drop_geometry()

#
ce2 <- crBP2_ISRAs %>%
  group_by(MEOW) %>%
  summarise(PUs = length(solution_1), PUs_selected = sum(solution_1)) %>%
  mutate(percent_protected2 = 100/PUs*PUs_selected) %>%
  sf::st_drop_geometry()

(ce <- left_join(ce1, ce2 %>% dplyr::select(MEOW, percent_protected2), by = join_by(MEOW)) %>%
    mutate(difference = percent_protected2-percent_protected1) )


## Cost per MEOW
hm1 <- crBP2 %>% st_drop_geometry()
hm2 <- crBP2_ISRAs %>% st_drop_geometry()

# How many selected PUs are NOT in locked-in areas:
sum(hm1$solution_1) - (hm1 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())
sum(hm2$solution_1) - (hm2 %>% filter(solution_1 == 1, isra == 1 | mpa == 1) %>% nrow())

# How many selected PUs are in ISRAs:
hm1 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(hm1$isra)*(h1 %>% filter(solution_1 == 1, isra == 1) %>% nrow()) # 
hm2 %>% filter(solution_1 == 1, isra == 1) %>% nrow(); 100/sum(hm2$isra)*(h2 %>% filter(solution_1 == 1, isra == 1) %>% nrow()) # 

## Compare the costs
tc = hm1 %>% summarise(total_cost = sum(cost)) # overall cost
hm1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(hm1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))
hm2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)); 100/tc*(hm2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))

# Solution with ISRAs is more costly
hm2 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost))/(  hm1 %>% filter(solution_1 == 1) %>% summarise(total_cost = sum(cost)))


ce1b <- crBP2 %>% 
  filter(solution_1 == 1) %>%
  group_by(MEOW) %>%
  summarise(PUs_selected = sum(solution_1),
            totalcost = sum(cost)) %>%
  mutate(costPerPU = totalcost/PUs_selected) %>%
  sf::st_drop_geometry()


ce2b <- as.data.frame(crBP2_ISRAs) %>% 
  filter(solution_1 == 1) %>%
  group_by(MEOW) %>%
  summarise(PUs_selected2 = sum(solution_1),
            totalcost2 = sum(cost)) %>%
  mutate(costPerPU2 = totalcost2/PUs_selected2) %>%
  sf::st_drop_geometry()


(ceb <- left_join(ce1b, ce2b, by = join_by(MEOW)) %>%
    mutate(difference = costPerPU2-costPerPU,
           ratio = costPerPU2/costPerPU,
           totalcostratio = totalcost2/totalcost,
           perdiffPUs = 100/PUs_selected*PUs_selected2) )

pdatM = ceb %>% left_join(ce %>% select(MEOW, PUs), by = "MEOW")  %>% droplevels() %>%
  mutate(percArea1 = 100/PUs*PUs_selected,
         percArea2 = 100/PUs*PUs_selected2)

pdatM_long <- pdatM %>%
  pivot_longer(
    cols = matches("PUs_selected|costPerPU"),
    names_to = c(".value", "Scenario"),
    names_pattern = "^(.*?)(2?)$"
  ) %>%
  mutate(Scenario = ifelse(Scenario == "2", "Including ISRAs", "Ignoring ISRAs"),
         percArea = 100/PUs*PUs_selected)

# Plot Fig6
ggplot(pdatM_long) +
  geom_hline(yintercept = 30, linetype = "dashed", color = "darkgreen") + # 30 by 30 line
  
  geom_point(aes(x = log10(costPerPU), y = percArea, size = PUs, col = MEOW, shape = Scenario)) +
  geom_segment(data = pdatM, aes(
    x = log10(costPerPU), y = percArea1,
    xend = log10(costPerPU2), yend = percArea2,
    color = MEOW)) +
  
  geom_text(data = pdatM, aes(
    x = (log10(costPerPU) + log10(costPerPU2)) / 2,
    y = (percArea1 + percArea2) / 2,
    label = MEOW),
    vjust = -0.5, hjust = 0.5, size = 3) +  
  
  scale_shape_manual(values = c(16, 17)) + 
  scale_size_continuous(range = c(1, 10), breaks = scales::pretty_breaks(n = 5)) +
  
  ylab("Percentage of Planning Units Selected") +
  xlab("log(Cost (Apparent Fishing Hours per Planning Unit))") +
  labs(size = "Total Planning Units", color = "MEOW", shape = "Scenario") +
  
  guides(color = FALSE, shape = guide_legend(title = "Scenario")) +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA))
#



## Which ISRA is most expensive
dat_isra_meow = dat_isra %>% filter(!(cellID %in% c(18889, 19157, 19428))) # Remove Sahara Upwelling MEOW
h1_meow = h1 %>% filter(!(cellID %in% c(18889, 19157, 19428)))

hm4 = hm2 %>% select(cellID, cost, solution_1, isra, mpa) %>%
  rename(sol_wISRA = solution_1) %>%
  mutate(ISRAn = dat_isra_meow$ISRA_ISRA_names) %>%
  mutate(sol_noISRA = h1_meow$solution_1)

hm4b <- hm4 %>%
  separate_rows(ISRAn, sep = ",") %>%  # Split the ISRAn column by comma into multiple rows
  mutate(ISRAn = str_trim(ISRAn))      # Trim any leading/trailing white space from ISRA names

# Group by the individual ISRA names and summarise
# Cost of PUs for the ISRA solution
t <- hm4b %>%
  group_by(ISRAn) %>%
  filter(sol_wISRA == 1) %>%
  summarise(
    total_cost = sum(cost, na.rm = TRUE),
    PUs = n()) %>%
  mutate(cost_per_PU = total_cost/PUs)

# Compared to the ignoring ISRA solution
t2 <- hm4b %>%
  group_by(ISRAn) %>%
  filter(sol_noISRA == 1) %>%
  summarise(
    total_cost = sum(cost, na.rm = TRUE),
    PUs = n()) %>%
  mutate(cost_per_PU = total_cost/PUs)

tf =  t %>% left_join(t2, by = "ISRAn")


