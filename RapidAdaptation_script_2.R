# Load libraries & import data --------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(tidyverse)
library(colourpicker)
library(lme4)
library(glmmTMB)
library(emmeans)
library(plotly)
library(corrplot)
library(ggcorrplot)
library(GGally)



# Import & clean data -----------------------------------------------------

setwd("C:/Users/luidnn/OneDrive - UKCEH/Documents/R projects/RapidAdaptation")

data <- read_excel("P:/07793_newLEAF/Workfiles/WP4/RapidAdaptationTrial_MasterSheet.xlsx", sheet = "Pinus contorta")

head(data)
str(data)
ncol(data)
# 28 columns
colnames(data)
unique(data$`Date of budset`)

#' "Block": 1-10                
#' "Position": 1-78             
#' "Column": 1-8               
#' "Row": 10                   
#' "cohort": Origin/Plantation/Regeneration                 
#' "provenance": North Coast/Skeena River/Alaska          
#' "provenance region": unknown/20104/32151/46134/47475/48744/52140
#' "Collection site (UK)": NA/Benmore/Rowens
#' "Family": e.g., LPNC-UKA5, LPSR - Ca6, etc. (55 families)               
#' "Status" dead/alive 
#'      -> CHANGE TO STATUS_1
#' "Date of budset": NA/September-December 2023, January-May 2024 
#'      -> CHANGE COLUMN NAME TO date_budset
#'      -> CHANGE TO JULIAN DAYS? does this work with dates across the new year?    
#' "Date of budburst": NA/February-March 2024 
#'      -> change column name to date_budburst
#'      -> change to Julian days    
#' "DBB (mm)...13" 
#'    -> change to DBB_1
#'    -> change to numerical       
#' "Height (cm)...14"   
#'    -> change to Height_1
#'    -> change to numerical
#'    -> remove  
#' "Adjusted Height (mm)"
#'   -> use this
#'   -> change to height
#' "Needle length 1 (mm)" 
#' "Needle length 1 (cm)" 
#' "Needle length 2 (mm)"
#' "Needle length 2 (cm)" 
#'    -> check which one to use
#' "Date" 
#'    -> change to "date_1"  
#'    -> or is this date for dbb, height and needle measurements?              
#' "Ruler": Oxford/Yellow/Red Ruler              
#' "Offset": 0.4/0.6              
#' "Status 22/09/2025"
#'    -> change to status_2    
#' "Size"
#'     -> remove               
#' "DBB (mm)...25"
#'    -> change to DBB_2
#'    -> change to numeric        
#' "Date measured DBB"
#'    -> change to date_DBB_2   
#'    -> change to Julian days        
#' "Height (cm)...27"
#'    -> change to height
#'    -> change to numeric    
#' "Date measured"
#'    -> change to date_height
#'    -> change to Julian days       


LP_all <- data %>% 
  rename(
    "status_1" = "Status",
    "date_budset" = "Date of budset",
    "date_budburst" = "Date of budburst",
    "DBB_1" = "DBB (mm)...13",
    "height_1" = "Adjusted Height (mm)",
    "needle_1" = "Needle length 1 (mm)",
    "needle_2" = "Needle length 2 (mm)",
    "date_1" = "Date",
    "status_2" = "Status 22/09/2025",
    "DBB_2" = "DBB (mm)...25",
    "date_DBB_2" = "Date measured DBB",
    "height" = "Height (cm)...27",
    "date_height" = "Date measured",
    "cohort" = Type,
    "provenance" = Provenance,
    "Bioassay_0" = "Bioassay leaf weight_2",
    "dry_mass" = "Dry mass (g)",
    "root_mass" = "Root dry mass (g)",
    "shootroot" = "Shoot-to-root ratio (S:R)",
    "total_dry_mass" = "Total dry mass (g)",
    "RMF" = "Root mass fraction (RMF)"
  )  %>% 
  select(-c(`Height (cm)...14`, `Needle length 1 (cm)`, `Needle length 2 (cm)`, 
            Ruler, Offset, Size,`Bioassay leaf weight`, `Dry mass > root mass?`)) %>% 
  mutate(across(c(DBB_1, height_1, needle_1, needle_2, height, DBB_2),as.numeric)) %>% 
  mutate(needle_mean = rowMeans(cbind(needle_1, needle_2), 
                                na.rm = TRUE)) %>% 
  mutate(Family = ifelse(Family == "sub for LPNC - UKA4 - actually LPNC - UKA1",
                         "LPNC - UKA1",
                         Family))


# remove all dead trees
LP <- subset(LP_all, status_2 == "alive")



# colour sets -------------------------------------------------------------

cohort_colors <- c(
  "Origin" = "#F08080",
  "Plantation" = "#528B8B",
  "Regeneration" = "#EEC900"
)

provenance_colors <- c(
  "Alaska" = "#EE6363",
  "North Coast" = "#66CD00",
  "Skeena River" = "#6495ED"
)


# Provenance & cohort distribution ----------------------------------------

ggplot(LP, aes(x=provenance, fill = cohort)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=cohort_colors) +
  theme_minimal()

ggplot(LP, aes(x=cohort, fill = provenance)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=provenance_colors) +
  theme_minimal()

# Trait distributions ------------------------------------------------------
colnames(LP)

LP_noPlant <- droplevels(subset(LP, cohort != "Plantation"))
ggplot(LP_noPlant, aes(x = RMF, fill = provenance, color=provenance)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.3,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(values=provenance_colors) +
  scale_color_manual(values=provenance_colors)


LP_noAlaska <- droplevels(subset(LP, provenance != "Alaska"))
ggplot(LP_noAlaska, aes(x = RMF, 
                       color = cohort, fill = cohort)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.3,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(values=cohort_colors) +
  scale_color_manual(values=cohort_colors)


# Relationship between variables ------------------------------------------

traits_noAlaska <- LP_noAlaska %>%
  select(height,
         DBB_2,
         needle_mean,
         total_dry_mass,
         RMF)

traits_noPlant <- LP_noPlant %>%
  select(height,
         DBB_2,
         needle_mean,
         total_dry_mass,
         RMF)

ggpairs(traits_noPlant,
        lower = list(
          continuous = wrap(
            "smooth",
            method = "loess",
            se = FALSE,
            alpha = 0.4
          )
        ))

ggplot(LP, aes(x = DBB_2, y = total_dry_mass)) +
  geom_point()

# 1. Correlation matrix ---------------------------------------------------

traits <- LP_noAlaska %>% 
  select(needle_mean, DBB_2, height, total_dry_mass, RMF)
cor_mat <- cor(traits, method ="pearson",
               use="pairwise.complete.obs")
ggcorrplot(cor_mat,
           lab = TRUE,
           lab_size = 5,
           type = "lower")
