# Load libraries & import data --------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(tidyverse)
library(colourpicker)
library(car)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(plotly)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(ggpattern)
library(ggrepel)

# Import & clean data -----------------------------------------------------

setwd("C:/Users/luidnn/OneDrive - UKCEH/Documents/R projects/RapidAdaptation")

data <- read_excel("P:/07793_newLEAF/Workfiles/WP4/RapidAdaptationTrial_MasterSheet.xlsx", sheet = "Pinus contorta")
minitab_noPlantation_data <- read_excel("C:/Users/luidnn/OneDrive - UKCEH/Documents/R projects/RapidAdaptation/RapidAdaptation_minitab_LP.xlsx", sheet="noPlantation_clean")
minitab_noAlaska_data <- read_excel("C:/Users/luidnn/OneDrive - UKCEH/Documents/R projects/RapidAdaptation/RapidAdaptation_minitab_LP.xlsx", sheet="noAlaska_clean")

head(minitab_noPlantation_data)
head(minitab_noAlaska_data)

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
  mutate(
    height = as.numeric(height),
    diameter = as.numeric(diameter),
    slenderness = height / diameter
  ) %>% 
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

ggplot(LP_noAlaska, aes(x=provenance, fill = cohort)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=cohort_colours) +
  theme_minimal()

ggplot(LP_noAlaska, aes(x=cohort, fill = provenance)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=provenance_colours) +
  theme_minimal()

# originally planted
ggplot(LP_all, aes(x=cohort, fill = provenance)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=provenance_colours) +
  theme_minimal()

# proportion alive
prop_alive <- LP_all %>% 
  group_by(cohort) %>% 
  summarise(
    total = n(),
    alive = sum(status_2 == "alive"),
    perc_alive = alive / total *100,
    .groups ="drop"
  )

ggplot(prop_alive, aes(y=perc_alive, x = cohort, fill=cohort)) +
  geom_col(position="dodge") +
  theme_minimal() +
  scale_fill_manual(values=cohort_colours) +
  labs(
    y = "Percentage alive",
    x=NULL) +
  scale_y_continuous(limits=c(0,100)) +
  theme(legend.position="none",
        axis.title.y = element_text(size = 18),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14)
  )

ggsave("Perc_alive.png", dpi=300)

# 1a Trait distributions ------------------------------------------------------

LP_noPlant <- droplevels(subset(LP, cohort != "Plantation"))
ggplot(LP_noPlant, aes(x = slenderness, fill = provenance, color=provenance)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(values=provenance_colours) +
  scale_color_manual(values=provenance_colours)

LP_noAlaska <- droplevels(subset(LP, provenance != "Alaska"))
ggplot(LP_noAlaska, aes(x = slenderness, color = cohort, fill = cohort)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(values=cohort_colours) +
  scale_color_manual(values=cohort_colours)

# 1b Trait boxplots by cohort and provenance ---------------------------------

LP_long <- LP_noAlaska %>%
  select(ID, provenance, cohort, height, RMF, slenderness, needle_mean) %>%
  pivot_longer(
    cols = c(height, RMF, slenderness, needle_mean),
    names_to = "trait",
    values_to = "value"
  )

LP_long <- LP_long %>%
  mutate(
    cohort = as.factor(cohort),
    provenance = as.factor(provenance),
    trait = as.factor(trait)
  )

ggplot(LP_long, aes(x = cohort, y = value, fill=cohort)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ trait, scales = "free_y",
             labeller=labeller(trait=c(
               height="Height (cm)",
               needle_mean="Mean needle length (mm)",
               RMF="Root mass fraction",
               slenderness="Slenderness"
             ))) +
  scale_fill_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(
        strip.text = element_text(face = "bold", size = 16),
        legend.position="none",
        axis.text.x = element_text(, size =18, hjust = 1),
        axis.text.y  = element_text(size = 18)) +
  scale_fill_manual(values=cohort_colours) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y=NULL,
       x=NULL) +
  scale_x_discrete(labels=c(
    "Origin"="O",
    "Plantation"="P",
    "Regeneration"="R"
  ))

ggsave("trait_boxplots.png", 
       width = 22.01,
       height = 19.84,
       units = "cm",
       dpi=300)

# anova 
# no Plantation
mod_height_noPlant <- aov(height~cohort*provenance, data = LP_noPlant)

plot(mod_height_noPlant)

summary(mod_height_noPlant)
emmeans(mod_height_noPlant, pairwise ~ cohort | provenance, adjust="tukey")

# no Alaska
mod_height_noAlaska <- aov(height~cohort, data = LP_noAlaska)
mod_needle_noAlaska <- aov(needle_mean~cohort, data = LP_noAlaska)
mod_RMF_noAlaska <- aov(RMF~cohort, data = LP_noAlaska)
mod_slenderness_noAlaska <- aov(slenderness~cohort, data = LP_noAlaska)

plot(mod_slenderness_noAlaska)

summary(mod_slenderness_noAlaska)
emmeans(mod_slenderness_noAlaska, pairwise ~ cohort, adjust="tukey")

# 2. Correlations between traits ------------------------------------------

traits_noPlant <- LP_noPlant %>%
  select(
         height,
         diameter,
         needle_mean,
         total_dry_mass,
         RMF,
         shootroot,
         slenderness)

traits_noAlaska <- LP_noAlaska %>%
  select(
         height,
         diameter,
         needle_mean,
         total_dry_mass,
         RMF,
         shootroot,
         slenderness)

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
cor_mat <- cor(traits_noAlaska, method ="pearson",
               use="pairwise.complete.obs")
ggcorrplot(cor_mat,
           lab = TRUE,
           lab_size = 5,
           type = "lower") +
  scale_fill_gradient2(
    name="Correlation",
    low="#1874CD",
    mid="white",
    high = "#CD3700",
    midpoint = 0,
    limits = c(-1,1)
  ) +
  scale_x_discrete(labels=c(
    shootroot="shoot:root",
    total_dry_mass="Total dry mass",
    needle_mean="Mean needle length",
    diameter="Diameter",
    height="Height",
    slenderness="Slenderness"
  )) +
  scale_y_discrete(labels=c(
    shootroot="shoot:root",
    total_dry_mass="Total dry mass",
    needle_mean="Mean needle length",
    diameter="Diameter",
    height="Height"
      )) +
  theme(axis.text.x = element_text(, size =14),
        axis.text.y  = element_text(size = 14),
        legend.title=element_text(margin=margin(b=10)))
  

ggsave("trait_corr.png", dpi=300)

# individual comparisons
ggplot(LP_noPlant, aes(x=height, y=shootroot)) +
  geom_point() +
  geom_smooth(method="loess", col="red") +
  theme_minimal()

ggplot(LP_noAlaska, aes(x=height, y=shootroot)) +
  geom_point() +
  geom_smooth(method="loess", col="red") +
  theme_minimal()

# 2b Trait consolidation --------------------------------------------------

# combining height and diameter for overall growth form (tall and slender or short and stocky)

LP$slenderness <- LP$height/LP$diameter

# slenderness value distribution
ggplot(LP, aes(x = slenderness, fill = provenance, color=provenance)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")) +
  ylim(0, max(LP$slenderness))

# height & diameter boxplots
ggplot(LP, aes(y = slenderness, x = cohort, fill = provenance, color=provenance)) +
  geom_boxplot(alpha=0.5) +
  theme_minimal() +
  theme(
    legend.justification = c("right", "top")) +
  ylim(0,max(LP$slenderness))

ggplot(LP_noAlaska, aes(x = diameter, y = height, color=cohort)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal() +
  theme(
    legend.justification = c("right", "top")) +
  scale_color_manual(values=cohort_colours)

# 2c Trait relationships by provenance and cohort ------------------------

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
                  "slenderness"), 
        aes(colour=cohort),
        lower=list(continuous=wrap("smooth", se=F, alpha=0.3)), 
        diag=list(continuous=wrap("densityDiag", alpha=0.6)))+
  scale_color_manual(values = unique(cohort_colours_2))+
  scale_fill_manual(values = unique(cohort_colours_2)) +
  theme_bw()
    # careful with changing order of cohorts

# 3. Variance partitioning ---------------------------------------------------------------

# Annika's minitab data

# no Alaska
# reorder source and trait levels for plot aesthetics
head(minitab_noAlaska_data)
unique(minitab_noAlaska_data$trait)

variance_noAlaska <- minitab_noAlaska_data %>% 
  mutate(
    prop_var = as.numeric(prop_var),
    prop_Std = as.numeric(prop_Std),
    variance = as.numeric(variance),
    perc_variance = prop_var * 100,
    perc_Std = prop_Std * 100
  ) %>% 
  filter(model != "nested" & source != "Total") %>% 
  mutate(
    trait = factor(trait,
                   levels=c("height", "needle", "RMF", 
                            "slenderness")),
    source=factor(source,
                  c("provenance", "cohort", "cohort*provenance", 
                    "Block", "Error"))
  )

           
cols <- scales::hue_pal()(length(levels(variance_noAlaska$source)))
names(cols) <- levels(variance_noAlaska$source)

# override just "Error"
cols["Error"] <- "grey"

# define labels
source_labels <- c(
  "provenance" = "Provenance",
  "cohort" = "Cohort",
  "cohort*provenance" = "Cohort × Provenance",
  "Block" = "Block",
  "Error" = "Error"
)

# define sources
pattern_vals <- c(
  "provenance" = "stripe",
  "cohort" = "circle",
  "cohort*provenance" = "crosshatch",
  "Block" = "circle",
  "Error" = "none"
)

# patterned stacked barplot
ggplot(variance_noAlaska,
       aes(x = trait, y = perc_variance,
           fill = source, pattern = source)) +
  geom_col_pattern(
    colour = "black",
    linewidth = 0.5,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02
  ) +
  scale_fill_manual(values = cols, name = "Source", labels = source_labels) +
  scale_pattern_manual(values = pattern_vals, name = "Source", labels = source_labels) +
  guides(pattern = "none") +
  scale_x_discrete(labels = c(
    "height" = "Height",
    "needle_mean" = "Needle length",
    "RMF" = "RMF",
    "slenderness" = "Slenderness"
  )) +
  labs(y = "Percent variance", x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

ggsave("variance_partitioning.png", 
       width = 22.01,
       height = 19.84,
       units = "cm",
       dpi=300)






# no Plantation
variance_noPlant <- minitab_noPlantation_data %>% 
  mutate(
    prop_var = as.numeric(prop_var),
    prop_Std = as.numeric(prop_Std),
    variance = as.numeric(variance),
    perc_variance = prop_var * 100,
    perc_Std = prop_Std * 100
  ) %>% 
  filter(model != "nested" & source != "Total")

head(minitab_noPlantation_data)
head(variance_noPlant)
unique(variance_noPlant$source)

ggplot(variance_noPlant, aes(x = trait, y = perc_variance, pattern = source)) +
  geom_col_pattern(
    aes(fill=source),
    colour="black",
    linewidth=0.5,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02
  ) +
  scale_fill_grey(start = 0.2, end = 0.9) +
  theme_minimal()

      
# 4. PCA ------------------------------------------------------------------

# only keep trees that have values for all traits
traits <- c("height", "RMF", "needle_mean", "slenderness")
LP_pca_clean <- LP %>% 
  drop_na(all_of(traits))

pca_traits <- LP_pca_clean %>%
  select(
    height,
    needle_mean,
    RMF,
    slenderness) 

# construct PCA
pca <- prcomp(pca_traits, scale.=TRUE)
summary(pca)

pca_df <- as.data.frame(pca$x) %>% 
  mutate(cohort=LP_pca_clean$cohort,
         provenance=LP_pca_clean$provenance)

# Cohort
ggplot(pca_df, aes(PC1, PC2, colour = cohort)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group=cohort, fill=cohort), 
               linewidth=0.1,
               geom="polygon",
               alpha=0.3,
               colour=NA) +
  geom_point(data=centroids_cohort,
             aes(PC1, PC2, colour = cohort),
             shape = 4, size = 5, stroke = 3) +
  scale_fill_manual(values = cohort_colours) +     
  scale_colour_manual(values = cohort_colours) + 
  guides(fill="none") +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0("PC1 (", round(summary(pca)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2,2] * 100, 1), "%)"),
    colour = "Cohort"
    )

# cohhort centroids
centroids_cohort <- pca_df %>% 
  group_by(cohort) %>% 
  summarise(
    PC1=mean(PC1),
    PC2=mean(PC2),
    .groups="drop"
  )
centroids_cohort

# Provenance
ggplot(pca_df, aes(PC1, PC2, colour = provenance)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group=provenance, fill=provenance), 
               linewidth=0.1,
               geom="polygon",
               alpha=0.3,
               colour=NA) +
  geom_point(data=centroids_provenance,
             aes(PC1, PC2, colour = provenance),
             shape = 4, size = 5, stroke = 3) +
  scale_fill_manual(values = provenance_colours) +     
  scale_colour_manual(values = provenance_colours) + 
  guides(fill="none") +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0("PC1 (", round(summary(pca)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2,2] * 100, 1), "%)"),
    colour = "Provenance"
  )

# provenance centroids
centroids_provenance <- pca_df %>% 
  group_by(provenance) %>% 
  summarise(
    PC1=mean(PC1),
    PC2=mean(PC2),
    .groups="drop"
  )
centroids_provenance
  

# Statistical tests on centroids
library(vegan)
library(pairwiseAdonis)

# cohort
adonis_result_cohort <- adonis2(
  pca_traits ~cohort,
  data=pca_df,
  method="euclidian"
)
adonis_result_cohort
# sig diff because of centroids or dispersion?
dispersion_cohort <- betadisper(dist(pca_traits), pca_df$cohort)
anova(dispersion_cohort)
  #' not sig -> cohorts don't differ because of dispersion

# which cohorts differ
pairwise.adonis2(
  pca_traits~cohort,
  data=pca_df
)
install.packages("pairwiseAdonis")
# not available

manova_res_cohort <- manova(as.matrix(pca$x) ~ pca_df$cohort)
summary(manova_res_cohort, test="Pillai")

# provenance
adonis_result_provenance <- adonis2(
  pca_traits ~provenance,
  data=pca_df,
  method="euclidian"
)
adonis_result_provenance
# sig diff because of centroids or dispersion?
dispersion_provenance <- betadisper(dist(pca_traits), pca_df$provenance)
anova(dispersion_provenance)
  # dispersion not sig

  
# which traits contribute to PC1 and which ones to PC2?
pca$rotation

# Kit's PCA ---------------------------------------------------------------------

# calculating eigenvectors and eigenvalues for genotype data
pca <- snpgdsPCA(
  geno_gds,                # a SNP GDS file, contains genotype data for each tree
  autosome.only = FALSE
)

# creating a vector: percentage for variance proportion
pc.percent <- pca$varprop * 100

# importing trait data
Keyfile_sequenced_Psylvestris <- read_excel(
  "P:/07793_newLEAF/Workfiles/WP4/Rapid adaptation(SP)/Keyfile_sequenced_Psylvestris.xlsx",
  col_types = c(
    "text",    # Trees ID
    "text",    # Family final
    "text",    # Family group final
    "text",    # Soil Final
    "numeric", # Height 2024
    "numeric", # Height 2025
    "numeric", # Shoot-to-root ratio
    "numeric"  # Root mass fraction
  )
)

# matching tree IDs from genotype data to genotyping PCA coordinates
# using first two PCs, adding coordinates for each tree
pca_df <- data.frame(
  Trees_ID_clean = read.gdsn(index.gdsn(geno_gds, "sample.id")),
  PC1 = pca$eigenvect[, 1],
  PC2 = pca$eigenvect[, 2]
)

# adding trait data to PCA coordinates for each tree
pca_df <- left_join(
  pca_df,
  Keyfile_sequenced_Psylvestris,
  by = "Trees_ID_clean"
)

# making soil traits into factors
pca_df <- pca_df %>%
  mutate(
    `Soil Final` = factor(`Soil Final`, levels = c("L", "M", "H")),
    `Family group final` = factor(`Family group final`, levels = c("O", "L", "M", "H"))
  )

# plot PCA, using soil traits as ellipses
pca_plot <- ggplot(pca_df, aes(
  PC1, PC2,
  color = `Family group final`,
  shape = `Soil Final`
)) +
  geom_point(size = 3, alpha = 0.85) +
  theme_classic(base_size = 14) +
  xlab(paste0("PC1 (", round(pc.percent[1], 1), "%)")) +
  ylab(paste0("PC2 (", round(pc.percent[2], 1), "%)")) +
  labs(
    colour = "Family group",
    shape = "Soil treatment"
  ) +
  theme(
    legend.position = "right"
  )

ggsave(
  filename = file.path(fig_dir, "PCA_family_soil.svg"),
  plot = pca_plot,
  width = 7, height = 6
)
