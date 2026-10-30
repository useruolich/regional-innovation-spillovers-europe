library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(ggplot2)
library(sf)
library(geosphere)
library(tmap)
library(leaflet)
library(writexl)
library(readr)
library(base64enc)
library(car)
library(lmtest)
library(tidyverse)
library(GGally)

# radian/degree function
deg2rad <- function(deg) {
  deg * pi / 180
}

rad2deg <- function(rad) {
  rad * 180 / pi
}

nuts2 <- st_read("data/NUTS_RG_20M_2021_4326.shp")

# 240 NUTS-2/3 regions
selected_regions <- c(
  "BE1", "BE2", "BE3", "BG31", "BG32", "BG33", "BG34", "BG41", "BG42", "CZ01", "CZ02", "CZ03", "CZ04",
  "CZ05", "CZ06", "CZ07", "CZ08", "DK01", "DK02", "DK03", "DK04", "DK05", "DE11", "DE12", "DE13", "DE14",
  "DE21", "DE22", "DE23", "DE24", "DE25", "DE26", "DE27", "DE3", "DE4", "DE5", "DE6", "DE71", "DE72",
  "DE73", "DE8", "DE91", "DE92", "DE93", "DE94", "DEA1", "DEA2", "DEA3", "DEA4", "DEA5", "DEB1", "DEB2",
  "DEB3", "DEC", "DED4", "DED2", "DED5", "DEE", "DEF", "DEG", "IE04", "IE05", "IE06", "EL3", "EL41",
  "EL42", "EL43", "EL51", "EL52", "EL53", "EL54", "EL61", "EL62", "EL63", "EL64", "EL65", "ES11", "ES12",
  "ES13", "ES21", "ES22", "ES23", "ES24", "ES3", "ES41", "ES42", "ES43", "ES51", "ES52", "ES53", "ES61",
  "ES62", "ES63", "ES64", "ES7", "FR1", "FRB", "FRC", "FRD", "FRE", "FRF", "FRG", "FRH", "FRI", "FRJ",
  "FRK", "FRL", "FRM", "FRY", "HR02", "HR03", "HR05", "HR06", "ITC1", "ITC2", "ITC3", "ITC4", "ITH1",
  "ITH2", "ITH3", "ITH4", "ITH5", "ITI1", "ITI2", "ITI3", "ITI4", "ITF1", "ITF2", "ITF3", "ITF4", "ITF5",
  "ITF6", "ITG1", "ITG2", "LT01", "LT02", "HU11", "HU12", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33",
  "NL11", "NL12", "NL13", "NL21", "NL22", "NL23", "NL31", "NL32", "NL33", "NL34", "NL41", "NL42", "AT1",
  "AT2", "AT3", "PL21", "PL22", "PL41", "PL42", "PL43", "PL51", "PL52", "PL61", "PL62", "PL63", "PL71",
  "PL72", "PL81", "PL82", "PL84", "PL91", "PL92", "PT11", "PT15", "PT16", "PT17", "PT18", "PT2", "PT3",
  "RO11", "RO12", "RO21", "RO22", "RO31", "RO32", "RO41", "RO42", "SI03", "SI04", "SK01", "SK02", "SK03",
  "SK04", "FI19", "FI1B", "FI1C", "FI1D", "FI2", "SE11", "SE12", "SE21", "SE22", "SE23", "SE31", "SE32",
  "SE33", "NO02", "NO06", "NO07", "NO08", "NO09", "NO0A", "NO0B", "RS11", "RS12", "RS21", "RS22", "CH01",
  "CH02", "CH03", "CH04", "CH05", "CH06", "CH07", "UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ",
  "UKK", "UKL", "UKM", "UKN"
)

nuts2_filtered <- nuts2 %>%
  filter(NUTS_ID %in% selected_regions)

stopifnot(length(unique(nuts2_filtered$NUTS_ID)) == 240)

# Calculate the boundary-to-boundary distance matrix
distance_matrix_st <- st_distance(nuts2_filtered, nuts2_filtered)
distance_matrix <- as.matrix(distance_matrix_st) / 1000

rownames(distance_matrix) <- nuts2_filtered$NUTS_ID
colnames(distance_matrix) <- nuts2_filtered$NUTS_ID

# Map visualizations
#map1 (static)
ggplot(data = nuts2_filtered) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(aes(geometry = st_centroid(geometry)), color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Centroids of NUTS2 regions",
       subtitle = "Marked centers of each region",
       x = "longitude", y = "latitude")

#map2 (interactive)
tmap_mode("view")
tm_shape(nuts2_filtered) +
  tm_borders() +
  tm_dots(col = "red", size = 0.1, popup.vars = c("Region" = "NUTS_ID"))

#map3 (interactive+color)
leaflet(data = nuts2_filtered) %>%
  addTiles() %>%
  addPolygons(color = "black", weight = 1, fillOpacity = 0.2) %>%
  addCircleMarkers(lng = st_coordinates(st_centroid(nuts2_filtered$geometry))[,1],
                   lat = st_coordinates(st_centroid(nuts2_filtered$geometry))[,2],
                   radius = 3,
                   color = "red",
                   fillColor = "red",
                   popup = ~NUTS_ID)

# OECD data processing
oecd_data_raw <- read.csv("data/OECD_Complete_Dataset_csv.csv")
oecd_data_filtered <- oecd_data_raw %>%
  filter(Year %in% c(2017, 2018, 2019, 2020, 2021))
oecd_data_filtered <- oecd_data_filtered %>%
  mutate(Region = as.character(Region)) %>%
  mutate(region_id = str_extract(Region, "^[A-Z0-9]+"))

oecd_data_filtered <- oecd_data_filtered %>%
  filter(region_id %in% selected_regions)
oecd_data_processed <- oecd_data_filtered %>%
  mutate(
    `PCT.patent.applications` = as.numeric(`PCT.patent.applications`),
    `R.D.expenditure.in.the.business.sector` = as.numeric(`R.D.expenditure.in.the.business.sector`)
  ) %>%
  select(region_id,
         `PCT.patent.applications`,
         `R.D.expenditure.in.the.business.sector`) %>%
  group_by(region_id) %>%
  summarise(
    PatentApplications = mean(`PCT.patent.applications`, na.rm = TRUE),
    RD_Expenditure_in_private_sector = mean(`R.D.expenditure.in.the.business.sector`, na.rm = TRUE),
    .groups = 'drop'
  )

initial_regions_count_data <- nrow(oecd_data_processed)
oecd_data_processed <- oecd_data_processed %>%
  filter(PatentApplications > 0, RD_Expenditure_in_private_sector > 0)
if (nrow(oecd_data_processed) < initial_regions_count_data) {
  warning(paste("Dropped", initial_regions_count_data - nrow(oecd_data_processed),
                "regions from OECD data due to zero or missing PatentApplications or RD_Expenditure_in_private_sector for log transformation."))
}
oecd_data_processed <- oecd_data_processed %>%
  mutate(ln_Patent = log(PatentApplications),
         ln_RD = log(RD_Expenditure_in_private_sector))

# Align dataframes and matrices
distance_matrix_raw <- as.matrix(distance_matrix)
rownames(distance_matrix_raw) <- as.character(nuts2_filtered$NUTS_ID)
colnames(distance_matrix_raw) <- as.character(nuts2_filtered$NUTS_ID)
regions_in_oecd_data <- oecd_data_processed$region_id
regions_in_distance_matrix <- rownames(distance_matrix_raw)
common_regions <- sort(intersect(regions_in_oecd_data, regions_in_distance_matrix))

if (length(common_regions) == 0) {
  stop("No common regions found between your OECD data and the distance matrix after alignment. Please double-check data sources and NUTS_ID consistency.")
} else if (length(common_regions) < length(selected_regions)) {
  message(paste("Note: Working with", length(common_regions), "common regions out of", length(selected_regions), "expected. Regions not found in both data sources will be excluded."))
}

oecd_data_final <- oecd_data_processed %>%
  filter(region_id %in% common_regions) %>%
  arrange(region_id)
distance_matrix_sub <- distance_matrix_raw[common_regions, common_regions, drop = FALSE]

if (!all(rownames(distance_matrix_sub) == oecd_data_final$region_id)) {
  stop("CRITICAL ERROR: Region IDs in final processed data and subsetted distance matrix are not aligned after final filtering. Debugging needed.")
}
if (!all(colnames(distance_matrix_sub) == oecd_data_final$region_id)) {
  stop("CRITICAL ERROR: Region IDs in final processed data and subsetted distance matrix are not aligned after final filtering. Debugging needed.")
}

# Construct distance bands for spillovers, as per the Bottazzi and Peri paper
distance_bands <- list(
  "band_0_300km" = c(0, 300),
  "band_300_600km" = c(300, 600),
  "band_600_900km" = c(600, 900),
  "band_900_1300km" = c(900, 1300),
  "band_1300_2000km" = c(1300, 2000)
)

# Initialize a data frame to hold the spillover variables
spillover_data <- oecd_data_final %>%
  select(region_id)

# Loop through each region and calculate the spillover R&D for each distance band
for (band_name in names(distance_bands)) {
  spillover_data[[band_name]] <- NA
  lower_bound <- distance_bands[[band_name]][1]
  upper_bound <- distance_bands[[band_name]][2]
  
  for (i in 1:nrow(spillover_data)) {
    current_region_id <- spillover_data$region_id[i]
    
    # Get the row of distances for the current region
    distances_from_current_region <- distance_matrix_sub[current_region_id, ]
    
    # Identify neighbor regions that fall within the current distance band
    neighbor_regions_in_band <- names(distances_from_current_region[
      as.numeric(distances_from_current_region) > lower_bound &
        as.numeric(distances_from_current_region) <= upper_bound
    ])
    
    # Exclude the region itself from its neighbor list
    neighbor_regions_in_band <- setdiff(neighbor_regions_in_band, current_region_id)
    
    # Calculate the sum of R&D of the neighbors
    if (length(neighbor_regions_in_band) > 0) {
      spillover_sum <- oecd_data_final %>%
        filter(region_id %in% neighbor_regions_in_band) %>%
        summarise(spillover_sum = sum(RD_Expenditure_in_private_sector, na.rm = TRUE)) %>%
        pull(spillover_sum)
      spillover_data[[band_name]][i] <- spillover_sum
    } else {
      # Handle cases with no neighbors in a band
      spillover_data[[band_name]][i] <- 0
    }
  }
}

# Merge the new spillover variables back into your main dataset
oecd_data_final_with_spillovers <- left_join(oecd_data_final, spillover_data, by = "region_id")

# Create a list of the spillover variables for the model formula
# CORRECTED LINE: Removed the extra "band_" prefix
spillover_vars <- paste0("log(", names(distance_bands), " + 1)")
spillover_formula <- paste(spillover_vars, collapse = " + ")

# Build the final regression formula
model_formula_str <- paste("ln_Patent ~ ln_RD", spillover_formula, sep = " + ")
model_formula <- as.formula(model_formula_str)

# Run the regression model
model_spillovers <- lm(model_formula, data = oecd_data_final_with_spillovers)
summary(model_spillovers)





# Check for Linearity (Ramsey RESET Test)
# This test checks for functional form misspecification. A significant p-value (< 0.05)
# suggests that the model is non-linear and may be improperly specified.
library(lmtest)
resettest(model_spillovers, power = 2:3, type = "fitted")

# Check for Multicollinearity (VIF)
# VIF values measure how much the variance of an estimated regression coefficient
# is increased due to multicollinearity. Values above 5 or 10 are a cause for concern.
library(car)
vif_values <- vif(model_spillovers)
print(vif_values)
# Check for Homoscedasticity (Breusch-Pagan Test)
# This test checks if the residuals' variance is constant. A significant p-value (< 0.05)
# suggests heteroscedasticity (non-constant variance).
library(lmtest)
bptest(model_spillovers)

# Check for Normality of Residuals (Q-Q Plot)
# The Q-Q plot shows if the residuals follow a normal distribution.
# The points should fall closely along the diagonal straight line.
plot(model_spillovers, which = 2)

# Check for Linearity and Homoscedasticity (Residuals vs. Fitted Plot)
# This plot shows the distribution of residuals as a function of the fitted values.
# Look for a random cloud of points with no discernible pattern. A pattern,
# like a cone or a curve, indicates potential issues with linearity or constant variance.
plot(model_spillovers, which = 1)



