# Dissertation-code-
Code created on RStudio for Dissertation

# code to create correlation matrix 

# Install required packages (only run once)
install.packages("dplyr")
install.packages("caret")
install.packages("MASS")
install.packages("car")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("knitr")

# Load libraries
library(dplyr)
library(caret)
library(MASS)
library(car)
library(ggplot2)
library(reshape2)
library(knitr)

# Load data
data <- read.csv("data_with_dummies.csv")
View(data)

# Clean column names
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Convert imd_rank to numeric
data$imd_rank <- suppressWarnings(as.numeric(gsub(",", "", data$imd_rank)))

# Keep only numeric variables
numeric_vars <- data[, sapply(data, is.numeric)]

# Define desired column order
desired_order <- c(
  "diff_median_gpg",
  "firm_size.0...249", "firm_size250....499", "firm_size500....999",
  "firm_size1000....4999", "firm_size5000....19999", "firm_size20000.", "firm_sizeNot.Provided",
  "industryAccommodation.and.food.service.activities",
  "industryActivities.of.extraterritorial.organisations.and.bodies",
  "industryAdministrative.and.support.service.activities",
  "industryAgriculture.Forestry.and.Fishing",
  "industryArts.entertainment.and.recreation", "industryConstruction",
  "industryEducation", "industryElectricity.gas..steam.and.air.conditioning.supply",
  "industryFinancial.and.insurance.activities",
  "industryHuman.health.and.social.work.activities",
  "industryInformation.and.communication", "industryManufacturing",
  "industryMining.and.Quarrying", "industryMixed", "industryOther.service.activities",
  "industryProfessional..scientific.and.technical.activities",
  "industryPublic.administration.and.defence..compulsory.social.security",
  "industryReal.estate.activities", "industryTransportation.and.storage",
  "industryWater.supply.sewerage.waste.management.and.remediation.activities",
  "regionEast.Midlands", "regionEast.of.England", "regionLondon",
  "regionNorth.East", "regionNorth.West", "regionSouth.East", "regionSouth.West",
  "regionWest.Midlands", "regionYorkshire.and.The.Humber",
  "rural_urban_classificationLarger.rural..Nearer.to.a.major.town.or.city",
  "rural_urban_classificationSmaller.rural..Further.from.a.major.town.or.city",
  "rural_urban_classificationSmaller.rural..Nearer.to.a.major.town.or.city",
  "rural_urban_classificationUrban..Further.from.a.major.town.or.city",
  "rural_urban_classificationUrban..Nearer.to.a.major.town.or.city",
  "distance_from_london_km", "imd_rank", "median_age", "l4qual"
)

# Select only existing desired variables
existing_vars <- desired_order[desired_order %in% colnames(numeric_vars)]
reordered_numeric <- dplyr::select(numeric_vars, any_of(existing_vars))

# Compute correlation matrix
cor_matrix <- cor(reordered_numeric, use = "complete.obs")
cor_matrix_rounded <- round(cor_matrix, 3)

# Save to CSV
write.csv(cor_matrix_rounded, "correlation_matrix_reordered_version_3sf.csv", row.names = TRUE)

# Print kable for R Markdown
kable(as.data.frame(cor_matrix_rounded), caption = "Reordered Correlation Matrix (Rounded to 3 SF)")

# Rename map for cleaner labels
rename_map <- c(
  diff_median_gpg = "Median Hourly GPG (%)",
  distance_from_london_km = "Distance from London (km)",
  l4qual = "Level 4 Qualification",
  median_age = "Median Age (years)",
)

# Rename for labels safely
rename_safe <- function(vec) {
  new <- dplyr::recode(vec, !!!rename_map)
  ifelse(is.na(new), vec, new)
}
colnames(cor_matrix_rounded) <- rename_safe(colnames(cor_matrix_rounded))
rownames(cor_matrix_rounded) <- rename_safe(rownames(cor_matrix_rounded))

# Simplify for plotting
simplify_names <- function(vec) {
  vec <- gsub("^industry", "", vec)
  vec <- gsub("^region", "", vec)
  vec <- gsub("^firm_size", "", vec)
  vec <- gsub("^rural_urban_classification", "", vec)
  vec <- gsub("^diff_median_gpg$", "Median GPG", vec)
  vec <- gsub("^imd_rank$", "IMD Rank", vec)
  vec <- gsub("^l4qual$", "Level 4 Qual", vec)
  vec <- gsub("^median_age$", "Median Age", vec)
  vec <- gsub("^distance_from_london_km$", "Distance from London", vec)
  gsub("\\.+", " ", vec)
}
colnames(cor_matrix_rounded) <- simplify_names(colnames(cor_matrix_rounded))
rownames(cor_matrix_rounded) <- simplify_names(rownames(cor_matrix_rounded))

# Melt matrix for ggplot
melted_cor <- melt(cor_matrix_rounded)

# Heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_blank()
  ) +
  coord_fixed()


# Save heatmap
ggsave("correlation_heatmap.png", width = 12, height = 10, dpi = 300)





# code to create premliminary insights - bar charts. 

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Load and view the data
data <- read.csv("gpg_englandFINAL.csv")
View(data)
names(data)

# Clean the dataset
df_clean <- data %>%
  mutate(across(everything(), ~na_if(., "#N/A"))) %>%  
  mutate(across(where(is.character), str_trim))       

# --------- INDUSTRY ANALYSIS ---------
df_industry <- df_clean %>%
  dplyr::select(diff_median_gpg, industry) %>%
  filter(!is.na(industry)) %>%
  group_by(industry) %>%
  summarise(avg_gpg = mean(diff_median_gpg, na.rm = TRUE))

View(df_industry)

ggplot(df_industry, aes(x = reorder(industry, avg_gpg), y = avg_gpg)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Gender Pay Gap by Industry Type for Median Hourly Earnings, England, 2023–2024", 
    x = "Industry Type",
    y = "Gender Pay Gap (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10))

# --------- REGION ANALYSIS ---------
df_region <- df_clean %>%
  dplyr::select(diff_median_gpg, region) %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(avg_gpg = mean(diff_median_gpg, na.rm = TRUE))

View(df_region)

ggplot(df_region, aes(x = reorder(region, avg_gpg), y = avg_gpg)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(
    title = "Gender Pay Gap by Region for Median Hourly Earnings, England, 2023–2024",
    x = "Region",
    y = "Average Gender Pay Gap (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --------- FIRM SIZE ANALYSIS ---------
df_firm_size <- df_clean %>%
  dplyr::select(diff_median_gpg, firm_size) %>%
  filter(!is.na(firm_size)) %>%
  group_by(firm_size) %>%
  summarise(avg_gpg = mean(diff_median_gpg, na.rm = TRUE)) %>%
  mutate(firm_size = factor(firm_size, 
                            levels = c("0-249", "250-499", "500-999", "1000-4999", "5000-19999", "20000+")))

View(df_firm_size)

ggplot(df_firm_size, aes(x = firm_size, y = avg_gpg)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Gender Pay Gap by Firm Size for Median Hourly Earnings, England, 2023–2024",
    x = "Firm Size",
    y = "Gender Pay Gap (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, face = "bold"))

# --------- RURAL/URBAN ANALYSIS ---------
df_rural_urban <- df_clean %>%
  dplyr::select(diff_median_gpg, rural_urban_classification) %>%
  filter(!is.na(rural_urban_classification)) %>%
  group_by(rural_urban_classification) %>%
  summarise(avg_gpg = mean(diff_median_gpg, na.rm = TRUE))

View(df_rural_urban)

ggplot(df_rural_urban, aes(x = reorder(rural_urban_classification, avg_gpg), y = avg_gpg)) +
  geom_bar(stat = "identity", fill = "mediumpurple") +
  coord_flip() +
  labs(
    title = "Gender Pay Gap by Rural Urban Classification for Median Hourly Earnings, England, 2023–2024",
    x = "Rural Urban Classification",
    y = "Gender Pay Gap (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, face = "bold"))

# --------- DISTANCE FROM LONDON ANALYSIS ---------
df_clean <- df_clean %>%
  mutate(distance_group = cut(
    as.numeric(distance_from_london_km),
    breaks = c(0, 50, 100, 150, 200, 300, Inf),
    labels = c("0–50km", "50–100km", "100–150km", "150–200km", "200–300km", "300km+"),
    right = TRUE
  ))

distance_gpg <- df_clean %>%
  filter(!is.na(distance_group)) %>%
  group_by(distance_group) %>%
  summarise(avg_gpg = mean(diff_median_gpg, na.rm = TRUE))

View(distance_gpg)

ggplot(distance_gpg, aes(x = distance_group, y = avg_gpg)) +
  geom_bar(stat = "identity", fill = "steelblue") +



  labs(
    title = "Gender Pay Gap by Distance from London for Median Hourly Earnings, England, 2023–2024",
    x = "Distance from London (km)",
    y = "Gender Pay Gap (%)"
  ) +

# Stepwise regression code

data <- read.csv("data_with_dummies.csv")
names(data)

# Create Model 1 (industry and firm size)
model_1 <- lm(
  diff_median_gpg ~ 
    # Industry variables
    industryAccommodation.and.food.service.activities + 
    industryActivities.of.extraterritorial.organisations.and.bodies + 
    industryAdministrative.and.support.service.activities + 
    industryAgriculture..Forestry.and.Fishing + 
    industryArts..entertainment.and.recreation + 
    industryConstruction + 
    industryEducation + 
    industryElectricity..gas..steam.and.air.conditioning.supply + 
    industryFinancial.and.insurance.activities + 
    industryHuman.health.and.social.work.activities + 
    industryInformation.and.communication + 
    industryManufacturing + 
    industryMining.and.Quarrying + 
    industryMixed + 
    industryOther.service.activities + 
    industryProfessional..scientific.and.technical.activities + 
    industryPublic.administration.and.defence..compulsory.social.security + 
    industryReal.estate.activities + 
    industryTransportation.and.storage + 
    industryWater.supply..sewerage..waste.management.and.remediation.activities + 
    industryWholesale.and.retail.trade..repair.of.motor.vehicles.and.motorcycles + 
    # Firm size variables
    firm_size.0...249 + 
    firm_size1000....4999 + 
    firm_size20000. + 
    firm_size250....499 + 
    firm_size500....999 + 
    firm_size5000....19999 + 
    firm_sizeNot.Provided,
  data = data
)

# View summary and AIC for Model 1
summary(model_1)
AIC(model_1)

# Tidy results for Model 1 using broom
tidy_model_1 <- tidy(model_1)

# View the tidy table for Model 1
print(tidy_model_1)

# Export Model 1 results to CSV
write.csv(tidy_model_1, "model_1_resultsfinal.csv", row.names = FALSE)


# Create Model 2 (industry, firm size, rural and urban, region, distance from London)
model_2 <- lm(
  diff_median_gpg ~ 
    # Industry variables
    industryAccommodation.and.food.service.activities + 
    industryActivities.of.extraterritorial.organisations.and.bodies + 
    industryAdministrative.and.support.service.activities + 
    industryAgriculture..Forestry.and.Fishing + 
    industryArts..entertainment.and.recreation + 
    industryConstruction + 
    industryEducation + 
    industryElectricity..gas..steam.and.air.conditioning.supply + 
    industryFinancial.and.insurance.activities + 
    industryHuman.health.and.social.work.activities + 
    industryInformation.and.communication + 
    industryManufacturing + 
    industryMining.and.Quarrying + 
    industryMixed + 
    industryOther.service.activities + 
    industryProfessional..scientific.and.technical.activities + 
    industryPublic.administration.and.defence..compulsory.social.security + 
    industryReal.estate.activities + 
    industryTransportation.and.storage + 
    industryWater.supply..sewerage..waste.management.and.remediation.activities + 
    industryWholesale.and.retail.trade..repair.of.motor.vehicles.and.motorcycles + 
    # Firm size variables
    firm_size.0...249 + 
    firm_size1000....4999 + 
    firm_size20000. + 
    firm_size250....499 + 
    firm_size500....999 + 
    firm_size5000....19999 + 
    # Rural and Urban classification
    rural_urban_classificationLarger.rural..Further.from.a.major.town.or.city + 
    rural_urban_classificationLarger.rural..Nearer.to.a.major.town.or.city + 
    rural_urban_classificationSmaller.rural..Further.from.a.major.town.or.city + 
    rural_urban_classificationSmaller.rural..Nearer.to.a.major.town.or.city + 
    rural_urban_classificationUrban..Further.from.a.major.town.or.city + 
    rural_urban_classificationUrban..Nearer.to.a.major.town.or.city + 
    # Region variables
    regionEast.Midlands + 
    regionEast.of.England + 
    regionLondon + 
    regionNorth.East + 
    regionNorth.West + 
    regionSouth.East + 
    regionSouth.West + 
    regionWest.Midlands + 
    regionYorkshire.and.The.Humber + 
    # Distance from London
    distance_from_london_km,
  data = data
)

# View summary and AIC for Model 2
summary(model_2)
AIC(model_2)

# Tidy results for Model 2 using broom
tidy_model_2 <- tidy(model_2)

# View the tidy table for Model 2
print(tidy_model_2)

# Export Model 2 results to CSV
write.csv(tidy_model_2, "model_2_results.csv", row.names = FALSE)


# Create Model 3 (industry, firm size, rural and urban, region, distance from London, imd, l4qual, and median age)
model_3 <- lm(
  diff_median_gpg ~ 
    # Industry variables
    industryAccommodation.and.food.service.activities + 
    industryActivities.of.extraterritorial.organisations.and.bodies + 
    industryAdministrative.and.support.service.activities + 
    industryAgriculture..Forestry.and.Fishing + 
    industryArts..entertainment.and.recreation + 
    industryConstruction + 
    industryEducation + 
    industryElectricity..gas..steam.and.air.conditioning.supply + 
    industryFinancial.and.insurance.activities + 
    industryHuman.health.and.social.work.activities + 
    industryInformation.and.communication + 
    industryManufacturing + 
    industryMining.and.Quarrying + 
    industryMixed + 
    industryOther.service.activities + 
    industryProfessional..scientific.and.technical.activities + 
    industryPublic.administration.and.defence..compulsory.social.security + 
    industryReal.estate.activities + 
    industryTransportation.and.storage + 
    industryWater.supply..sewerage..waste.management.and.remediation.activities + 
    industryWholesale.and.retail.trade..repair.of.motor.vehicles.and.motorcycles + 
    # Firm size variables
    firm_size.0...249 + 
    firm_size1000....4999 + 
    firm_size20000. + 
    firm_size250....499 + 
    firm_size500....999 + 
    firm_size5000....19999 + 
    firm_sizeNot.Provided + 
    # Rural and Urban classification
    rural_urban_classificationLarger.rural..Further.from.a.major.town.or.city + 
    rural_urban_classificationLarger.rural..Nearer.to.a.major.town.or.city + 
    rural_urban_classificationSmaller.rural..Further.from.a.major.town.or.city + 
    rural_urban_classificationSmaller.rural..Nearer.to.a.major.town.or.city + 
    rural_urban_classificationUrban..Further.from.a.major.town.or.city + 
    rural_urban_classificationUrban..Nearer.to.a.major.town.or.city + 
    # Region variables
    regionEast.Midlands + 
    regionEast.of.England + 
    regionLondon + 
    regionNorth.East + 
    regionNorth.West + 
    regionSouth.East + 
    regionSouth.West + 
    regionWest.Midlands + 
    regionYorkshire.and.The.Humber + 
    # Distance from London
    distance_from_london_km + 
    # Additional variables: imd, l4qual, and median age
    imd_rank +  l4qual_. + 
    median_age,
  data = data
)

# View summary and AIC for Model 3
summary(model_3)
AIC(model_3)

# Tidy results for Model 3 using broom
tidy_model_3 <- tidy(model_3)

# View the tidy table for Model 3
print(tidy_model_3)

# Export Model 3 results to CSV
write.csv(tidy_model_3, "model_3_results.csv", row.names = FALSE)
