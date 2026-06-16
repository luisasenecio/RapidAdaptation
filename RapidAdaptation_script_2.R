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
#'    -> change to diameter / diameter
#'    -> change to numeric        
#' "Date measured DBB"
#'    -> change to date_diameter   
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
    "diameter" = "DBB (mm)...25",
    "date_diameter" = "Date measured DBB",
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
  mutate(across(c(DBB_1, height_1, needle_1, needle_2, height, diameter),as.numeric)) %>% 
  mutate(needle_mean = rowMeans(cbind(needle_1, needle_2), 
                                na.rm = TRUE)) %>% 
  mutate(ID = paste(Block, Position, sep="-")) %>% 
  relocate(ID, .before=1) %>% 
  mutate(Family = ifelse(Family == "sub for LPNC - UKA4 - actually LPNC - UKA1",
                         "LPNC - UKA1",
                         Family))


#' remove all dead trees
#' remove odd trees (taproot, shootroot extreme): 1-58, 7-70
LP <- LP_all %>% 
  subset(status_2 == "alive") %>% 
  filter(!(ID %in% c("1-58", "7-70")))


# no Plantation
LP_noPlant <- droplevels(subset(LP, cohort != "Plantation"))

# no Alaska
LP_noAlaska <- droplevels(subset(LP, provenance != "Alaska"))

# colour sets -------------------------------------------------------------

cohort_colours <- c(
  "Origin" = "#F08080",
  "Plantation" = "#528B8B",
  "Regeneration" = "#EEC900"
)

provenance_colours <- c(
  "Alaska" = "#EE6363",
  "North Coast" = "#66CD00",
  "Skeena River" = "#6495ED"
)

# Provenance & cohort distribution ----------------------------------------

ggplot(LP, aes(x=provenance, fill = cohort)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=cohort_colours) +
  theme_minimal()

ggplot(LP, aes(x=cohort, fill = provenance)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=provenance_colours) +
  theme_minimal()

# Trait distributions ------------------------------------------------------
colnames(LP)

LP_noPlant <- droplevels(subset(LP, cohort != "Plantation"))
ggplot(LP_noPlant, aes(x = shootroot, fill = provenance, color=provenance)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(values=provenance_colours) +
  scale_color_manual(values=provenance_colours)

ggplot(LP_noAlaska, aes(x = shootroot, color = cohort, fill = cohort)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(values=cohort_colours) +
  scale_color_manual(values=cohort_colours)

# 1. Correlations between traits ------------------------------------------

traits_noPlant <- LP_noPlant %>%
  select(
         height,
         diameter,
         needle_mean,
         total_dry_mass,
         RMF,
         shootroot,
         growthform)

traits_noAlaska <- LP_noAlaska %>%
  select(
         height,
         diameter,
         needle_mean,
         total_dry_mass,
         RMF,
         shootroot,
         growthform)

# to make line red
my_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(colour = "black", alpha = 0.5) +
    geom_smooth(
      method = "loess",
      se = FALSE,
      colour = "red"
    )
}

ggpairs(traits_noPlant,
        lower = list(
          continuous = my_smooth
          )
        )

# simpler plot
cor_mat <- cor(traits_noPlant, method ="pearson",
               use="pairwise.complete.obs")
ggcorrplot(cor_mat,
           lab = TRUE,
           lab_size = 5,
           type = "lower")

# individual comparisons
ggplot(LP_noPlant, aes(x=height, y=shootroot)) +
  geom_point() +
  geom_smooth(method="loess", col="red") +
  theme_minimal()

ggplot(LP_noAlaska, aes(x=height, y=shootroot)) +
  geom_point() +
  geom_smooth(method="loess", col="red") +
  theme_minimal()

# 1b Trait consolidation --------------------------------------------------

# combining height and diameter for overall growth form (tall and slender or short and stocky)

LP$growthform <- LP$height/LP$diameter

# growthform value distribution
ggplot(LP, aes(x = growthform, fill = provenance, color=provenance)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")) +
  ylim(0, max(LP$growthform))

# height & diameter boxplots
ggplot(LP, aes(y = growthform, x = cohort, fill = provenance, color=provenance)) +
  geom_boxplot(alpha=0.5) +
  theme_minimal() +
  theme(
    legend.justification = c("right", "top")) +
  ylim(0,max(LP$growthform))

ggplot(LP_noAlaska, aes(x = diameter, y = height, color=cohort)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal() +
  theme(
    legend.justification = c("right", "top")) +
  scale_color_manual(values=cohort_colours)

# 1c Trait relationships by provenance and cohort ------------------------

# plot by plot:
ggplot(LP_noPlant, aes(x =height , y = total_dry_mass , color = provenance)) +
  geom_point() +
  scale_colour_manual(values=provenance_colours) +
  theme_minimal() +
  geom_smooth(method="lm", se=F)

# series of plots by provenance
ggpairs(
  data = LP_noPlant, columns = c("height",
                                 "diameter",
                                 "needle_mean",
                                 "total_dry_mass",
                                 "RMF",
                                 "shootroot"),
  mapping = aes(color=provenance),
  lower = list(continuous=wrap(
    "smooth", method="lm", se=F, alpha=0.3
  )),
  diag=list(continuous=wrap("densityDiag", alpha=0.6))
) + 
  scale_color_manual(values = unique(provenance_colours))+
  scale_fill_manual(values = unique(provenance_colours)) +
  theme_bw()

# order of colours is really important
unique(LP_noPlant$provenance)


cohort_colours_2<- c(
    "Origin" = "#F08080",
    "Plantation" = "#528B8B",
    "Regeneration" = "#EEAD0E")

ggpairs(LP_noAlaska, 
        columns=c("height",
                  "diameter",
                   "needle_mean",
                   "total_dry_mass",
                   "RMF",
                  "shootroot", 
                  "growthform"), 
        aes(colour=cohort),
        lower=list(continuous=wrap("smooth", se=F, alpha=0.3)), 
        diag=list(continuous=wrap("densityDiag", alpha=0.6)))+
  scale_color_manual(values = unique(cohort_colours_2))+
  scale_fill_manual(values = unique(cohort_colours_2)) +
  theme_bw()
    # careful with changing order of cohorts




# PCA ---------------------------------------------------------------------


