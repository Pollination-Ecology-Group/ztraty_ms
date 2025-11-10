
# This script produce graphs for Marting for his presentation and potentialy paper


# Install and load HanPolNet package ----
#install.packages("devtools")
remove.packages("HanPolNet") # may be handy for updating the package

# It's a good idea to restart your R session now (Session > Restart R)

# Load the remotes package
library(remotes)

# Set the environment variable for your current R session
# Replace "ghp_..." with your actual token
Sys.setenv(GITHUB_PAT = "")


# Now, re-install the fresh, complete version from GitHub
devtools::install_github("Pollination-Ecology-Group/HanPolNet")

# It should now load correctly
library(HanPolNet)


## Plant abundance ----
# 1. Get the necessary data
plant_data_std <- get_plant_data(output = "standardized") # I select only data standardized by 
plant_data_std  <- plant_data_std[plant_data_std$Suc_pra > 0,]
hca_results <- calculate_hca(plant_data_std, focal_species = "Suc_pra")

# 2. Generate the plot with stats (default)
p_year <- plot_hca_neighborhood(
  hca_data = hca_results,
  abundance_data = plant_data_std,
  focal_species = "Suc_pra",
  years = 22:24,
  add_stats = FALSE,
  colors = c(Conspecific = "lightseagreen", `Heterospecific (HCA)` = "lightcoral")
)
print(p_year)

#Figure 1: Floral neighborhood of Succisa pratensis from 2022 to 2024. Violin plots show the distribution of standardized plant abundance for individual plots where S. pratensis was present. Conspecific (light sea green) represents the standardized abundance of the focal species, S. pratensis. Heterospecific (HCA) (light coral) represents the Heterospecific Co-flowering Abundance index, which is the cumulative standardized abundance of all other co-flowering plant species in the plot. Each point represents an individual plot. Standardized abundance is a unitless value scaled from 0 to 1, where 1 represents the maximum observed abundance for a given species across the entire dataset.

## Pollinator visitation -----
plot_linear <- plot_pollinator_diet(
  focal_plant = "Suc_pra",
  years = 22:24,
  is_pollinator = TRUE,
  summary_level = "plot",
  log_scale = FALSE,
  colors = c(Conspecific = "lightseagreen", Heterospecific = "lightcoral"),
  ylim = NULL,
  add_stats = FALSE,
  
)
print(plot_linear)

#Figure 2: Standardized interaction rates for pollinators visiting Succisa pratensis from 2022 to 2024. Violin plots compare the distribution of interaction rates for the specific group of pollinators known to visit S. pratensis. Conspecific (light sea green) represents the interaction rates of these pollinators on S. pratensis itself. Heterospecific (light coral) represents the interaction rates of the same pollinators on all other plant species. The standardized interaction rate is calculated as the total number of interactions per sampling visit. Each point represents the total standardized rate for a single plot.

## Testing the pollen deposition plotting function ####
# Plot the data grouped by year (the default)
plot_by_year <- plot_pollen_deposition(colors = c(Conspecific = "lightseagreen", Heterospecific = "lightcoral"))
print(plot_by_year)




# Plot the data grouped by day, without stats
plot_by_day <- plot_pollen_deposition(group_by = "day", years = 2024, add_stats = FALSE,colors = c(Conspecific = "lightseagreen", Heterospecific = "lightcoral"))
print(plot_by_day)




#Figure 3: Pollen deposition on stigmas of Succisa pratensis. Violin plots show the distribution of pollen grains counted on individual stigmas, grouped by year (or day of collection). Conspecific (light sea green) represents the number of pollen grains from S. pratensis. Heterospecific (light coral) represents the sum of all pollen grains from other plant species. Each point represents an individual stigma. The visualization allows for a comparison of the quantity and type of pollen received by the focal plant over time.

