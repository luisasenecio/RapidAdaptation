#' newLeaf - Rapid Adaptation
#' This project analyses rapid adaptation in P. sylvestris, P. contorta, and T. heterophylla
#' This script focuses on P. contorta (Lodgepole pine)
#' Experimental setup:
#' 1. Origin population in North America (Skeena river)
#' 2. Seed taken from origin populations -> germinated in UK -> planted at Rowens & Benmore -> mature plantations
#' 3. Natural regeneration from mature plantations (naturalised)
#' Seeds taken from all three populations
#' 
#' Data collection:
#' first measurements taken on 19th & 20th February 2024
#'  everything left of date column = first measurements
#' second DBB measurements taken on 10th and 16th October 2025
#' second height measurements taken on  9th October 2025


#' Biological questions:
#' Do provenance/family and budburst/budset/height/root:shoot ratio correlate?
#' first need to know distribution of all variables and traits

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
#' "Type": Origin/Plantation/Regeneration                 
#' "Provenance": North Coast/Skeena River/Alaska          
#' "Provenance region": unknown/20104/32151/46134/47475/48744/52140
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
#'    -> change to height_2
#'    -> change to numeric    
#' "Date measured"
#'    -> change to date_height_2
#'    -> change to Julian days       


LP <- data %>% 
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
    "height_2" = "Height (cm)...27",
    "date_height_2" = "Date measured"
  )  %>% 
  select(-c(`Height (cm)...14`, `Needle length 1 (cm)`, `Needle length 2 (cm)`, Ruler, Offset, Size)) %>% 
  mutate(across(c(DBB_1, height_1, needle_1, needle_2, height_2, DBB_2),as.numeric)) %>% 
  mutate(height_2 = height_2*10) %>% 
  mutate(Family = ifelse(Family == "sub for LPNC - UKA4 - actually LPNC - UKA1",
                         "LPNC - UKA1",
                         Family))
    # NAs introduced by coercion

# "sub for LPNC - UKA4 - actually LPNC - UKA1"
length(unique(LP$Family))



# Convert into Julian days ------------------------------------------------

# Budset
range(LP$date_budset, na.rm = T)
#' year/month/day
#' 2023/09/11 - 2024/05/17 
#' 
LP$date_budset <- as.Date(LP$date_budset)
LP$Julian_budset <- julian(LP$date_budset, origin = as.Date("2023-09-10"))
  # set origin date as one day before first budset so that Julian day value is 1 instead of 0

# Budburst

range(LP$date_budburst, na.rm = T)
#' 2024/02/02 - 2024/05/22

LP$date_budburst <- as.Date(LP$date_budburst)
LP$Julian_budburst <- julian(LP$date_budburst, origin = as.Date("2024-02-01"))


# Data exploration 
colnames(LP)


# Number of trees per group ------------------------------------------------

# FAMILY
colnames(LP)
length(unique(LP$Family))
# 54 families

LP %>%  
  count(Family, sort=TRUE) %>% 
  ggplot(aes(x=reorder(Family, -n), y=n, fill = n))  +
  geom_col() +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  labs(y="Number of trees", x="Family")
# wildly different distribution
#' 5 families with 50 trees
#' most families 10 trees
#' 1 family ~8 trees

ggsave("TreesPerFamily.png")

# Status across whole experiment ------------------------------------------------------------------

LP_long_status <- LP %>% 
  pivot_longer(
    cols = c(status_1, status_2),
    names_to = "status_group",
    values_to = "status"
  ) %>% 
  mutate(year = case_when(
      status_group == "status_1" ~ 2024,
      status_group == "status_2" ~ 2025
  ))

counts_status <- LP_long_status %>% 
  filter(!is.na(status)) %>% 
  group_by(status, status_group) %>% 
  summarise(n=n(), .groups="drop") %>% 
  mutate(x_pos = case_when(
    status_group == "status_1" & status == "alive" ~ 1 - 0.2,
    status_group == "status_1" & status == "dead"  ~ 1 + 0.2,
    status_group == "status_2" & status == "alive" ~ 2 - 0.2,
    status_group == "status_2" & status == "dead"  ~ 2 + 0.2
  ),
  y_pos = n + 5)

ggplot(LP_long_status, aes(x = factor(year), fill = status)) +
  geom_bar(alpha=0.9,position = "dodge") +
  geom_text(
    data = counts_status, 
    aes(x=x_pos, y=y_pos,label = n),
    inherit.aes=F,
    vjust=-1
  )+
  theme_minimal() +
  labs(title = "Tree status in 2024 and 2025", x = "Year", y = "Count") +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(
    values = c("dead" = "steelblue", "alive" = "tomato")
   ) +
  labs(fill = "Status") +
  scale_y_continuous(limits=c(0,650))

ggsave("status.png")


# Status by Type ----------------------------------------------------------

StatusType <- LP %>% 
  group_by(Type) %>% 
  summarise(
    total = n(),
    alive =sum(status_2 == "alive"),
    dead = sum(status_2 == "dead"),
    proportion_alive=alive/total
  )

ggplot(StatusType,
       aes(x = Type, y = proportion_alive*100, fill = Type)) +
  geom_col() +
  labs(
    x = "Type",
    y = "Percentage alive"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("Origin" = "#F08080", "Plantation" = "#528B8B", "Regeneration" = "#EEC900")
  )

ggsave("StatusProvenanceProportion.png")


# Status by Provenance ----------------------------------------------------

StatusProvenance <- LP %>% 
  group_by(Provenance) %>% 
  summarise(
    total = n(),
    alive =sum(status_2 == "alive"),
    dead = sum(status_2 == "dead"),
    proportion_alive=alive/total
  )

ggplot(StatusProvenance,
       aes(x = Provenance, y = proportion_alive*100, fill = Provenance)) +
  geom_col() +
  labs(
    x = "Provenance",
    y = "Percentage alive"
  ) +
  theme_minimal()

ggsave("StatusProvenanceProportion.png")

# Status by Block ---------------------------------------------------------

StatusBlockCounts <- LP %>% 
  count(Block, status_2)

ggplot(StatusBlockCounts, aes(x=factor(Block), y=n, fill=status_2)) +
  geom_col(position="dodge") +
  theme_minimal() +
  labs(
    x="Block", y="Count",
    fill="Status"
  ) +
  scale_fill_manual(values = c("dead" = "steelblue", "alive" = "tomato")
  ) 

ggsave("StatusByBlock.png")



# Interaction graph -------------------------------------------------------------

# how do provenance AND type influence mortality?

# summary table for how many originally in each cohort & provenance AND how many survived
interaction <- LP %>% 
  count(Provenance, Type, status_2) %>% 
  pivot_wider(
    names_from = status_2,
    values_from = n
  ) %>% 
  left_join(TypeProvenanceCounts %>%  select(Provenance, Type, n),
            by = c("Provenance", "Type")) %>% 
  mutate(prop_alive = alive / n)


ggplot(interaction, aes(x = Provenance, y = prop_alive*100, fill = Type)) +
  geom_col(position="dodge") +
  facet_wrap(~ Type)  +
  labs(
    y = "Percentage alive"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("InteractionPlot.png")

# Are differences in mortality within Provenances and between Types (cohorts) significant?

# alive = 1, dead = 0
LP$status_bin <- ifelse(LP$status_2 == "alive", 1, 0)


# Interaction model -------------------------------------------------------

 # start with one explanatory variable (Provenance)
# Reference level = Alaska
# Dummy 1 = ProvNC
# Dummy 2 = ProvSR


LP$fStatus <- factor(LP$status_2)
LP$Provenance <- factor(LP$Provenance)
LP$Type <- factor(LP$Type)

model <- glm(fStatus ~ Provenance * Type, 
             data = LP, family = binomial)

summary(model)
  # Regeneration is significant predictor of status?


# binomial mixed model (GLMM)
  #' fixed effects: type & provenance
  #' random effect: family (many families, not main treatment)

LP$status_bin <- ifelse(LP$status_2 == "alive", 1,0)
model <- glmer(status_bin ~ Type + Provenance + Type:Provenance + (1| Family),
                    data=LP,
                    family=binomial)
# generalised linear mixed effects regression?


# test effects
anova(model, test = "Chisq")
  #' do survival rates differ among origin, plantation, and regeneration?
  #' do survival rates differ among alaska, north coast, and skeena river?
  #' does the effect of provenance depend on type?
  #' 
  #' npar = numberof parameters used to describe that effect
  #' sum sq = how much variation in survival is explained by that effect (in percent?)
  #' mean sq = average variation per parameter (e.g., 5.8 out of 11.6 variation explained is quite a lot)
  #' F value = signal-to-noise ratio for that effect
  #' Higher Sum Sq and F value = stronger effect
  #' 
  #' Type explains a meaningful amount of variation in survival 
  #'  -> survival differs noticeably among origin, plantation and regeneration
  #' Provenance: survival differs strongly among alaska, north coast, and skeena river
  #' Type:Provenance: small. Effect of provenance is largely consistent across types. (provenance doesn't behave very differently across types)


drop1(model, test="Chisq")
class(model)
anova(model)
summary(glmerfit)

model2 <- glmmTMB(status_bin ~ Type + Provenance + (1|Family),
                  data=LP,
                  family=binomial)

model3 <- glmer(status_bin ~ Provenance * Type + (1|Family),
                data = LP,
                family = binomial)

install.packages("emmeans")

emmeans(model3, pairwise ~ Provenance | Type)
  #' none of provenances are significantly different in each Type
emmeans(model3, pairwise ~ Type | Provenance)


# Status by Family --------------------------------------------------------

length(unique(LP$Family))
  # 54 families

StatusFamily <- LP %>% 
  group_by(Family) %>% 
  summarise(total=n(),
            alive = sum(status_2=="alive", na.rm=T),
            percent_alive = alive / total * 100) 

ggplot(StatusFamily,
       aes(y=reorder(Family, percent_alive),
           x=percent_alive,
           fill=percent_alive)) +
  geom_col() +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  theme_minimal() +
  labs(
    x="Percentage alive",
    y="Family",
    fill="Percent alive"
  ) + 
  theme(legend.position = "none")

ggsave("StatusByFamily.png")

# top and bottom 10 families (based on percentage_alive)

ggplot(StatusFamily_sub, aes(x = reorder(Family, percent_alive), y = percent_alive, fill=percent_alive)) +
  geom_col() +
  coord_flip() +
  labs(
    title="Top and Bottom 10 Families",
    x = "Family",
    y = "Alive (%)"
  ) +
  theme(legend.position = "none") +
  guides(fill="none") +
  theme_minimal()

ggsave("StatusByFamily.png")


#  Distribution - Family -----------------------------------------------------

 #' 6 families have 50 ind.
 #' most have 10 ind.
 #' one has ~13 ind. 
 #' one family has ~8 ind. 

FamilyDist <- StatusFamily %>% 
  count(total, name = "num_families")

ggplot(FamilyDist,
       aes(x=factor(total), y=num_families)) +
  geom_col(fill="steelblue") +
  labs(
    x="Number of individuals per family",
    y="Number of families"
  ) +
  theme_minimal()

ggsave("FamilyDistribution.png")

ggplot(StatusFamily, aes(x = reorder(Family, -total), y = total, fill=total)) +
  geom_col() +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 45))

# Distribution -Type -------------------------------------------------------


TypeCount <- LP %>% 
  group_by(Type) %>% 
  summarise(n=n(), .groups="drop") %>% 
  mutate(
    x = case_when(
      Type =="Origin" ~1,
      Type=="Plantation"~2,
      Type=="Regeneration"~3
    )
  )

ggplot(LP, aes(x=Type, fill=Type)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c("Origin" = "#F08080", "Plantation" = "#528B8B", "Regeneration" = "#EEC900")
  ) +
  labs(y="Number of trees") +
  theme_minimal()

ggsave("TypeDistribution.png")



# Distribution - Provenance -------------------------------------------------

ProvenanceCount <- LP %>% 
  group_by(Provenance) %>% 
  summarise(n=n(), .groups="drop")

# Distribution of Types
ggplot(ProvenanceCount, aes(x=Provenance, y=n, fill=Provenance)) +
  geom_col(position = position_dodge(width=1)) +
  theme(legend.position = "none") +
  labs(y="Number of trees") +
  theme_minimal()

ggsave("ProvenanceDistribution.png")



# DBB ------------------------------------------------------------------ 
LP_long_DBB <- LP %>%
  filter(status_1 == "alive" & status_2 == "alive") %>%    # remove trees that are dead but still have DBB measurements
  pivot_longer(
    cols = c(DBB_1, DBB_2),
    names_to = "DBB_year",
    values_to = "DBB"
  )

DBB_counts <- LP_long_DBB %>% 
  filter(!is.na(DBB)) %>% 
  group_by(DBB_year) %>% 
  summarise(n=n(), .groups="drop") %>% 
  mutate(x_pos =ifelse(DBB_year == "DBB_1", 1, 2.8),
         y_pos=20)

  # 506 measurements

# calculate median heights
median_DBB <- LP_long_DBB %>% 
  filter(!is.na(DBB)) %>% 
  group_by(DBB_year) %>% 
  summarise(med = median(DBB), .groups="drop")

# comparative histogram
ggplot(LP_long_DBB, aes(x=DBB, fill = DBB_year)) +
  geom_histogram(alpha=0.8, position = "identity") +
  geom_text(
    data = DBB_counts, 
    aes(x=x_pos, y = 200,
        label = paste("n =", n), 
        fill=NULL),
    position = position_dodge(width=1),
    inherit.aes=F,
    vjust=-1
  ) +
  geom_text(
    data = median_DBB,
    aes(x=med + 0.6, 
        label = paste0("Median = ", round(med, 2))),
    y = 250,
    vjust=-0.5,
    show.legend = F
  ) +
  geom_vline(
    data = median_DBB,
    aes(xintercept = med, color =DBB_year),
    linewidth = 1,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values=c(
      "DBB_1" = "steelblue4",
      "DBB_2" = "tomato2"
    )
  ) +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "DBB in 2024 and 2025",
    x = "DBB (mm)",
    y="Frequency"
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("DBB_1" = "steelblue", "DBB_2" = "tomato"),
    labels = c("DBB_1" = "2024", "DBB_2" = "2025")
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) 

# 296 NAs
ggsave("DBB_histogram.png")


range(LP$DBB_1, na.rm = T)
range(LP$DBB_2, na.rm = T)
sum(is.na(LP$DBB_1))
sum(is.na(LP$DBB_2))

# Height ------------------------------------------------------------------

# long format + scaling into cm
LP_long_height <- LP %>%
  filter(status_1 == "alive" & status_2 == "alive") %>%    # remove trees that are dead but still have height measurements
  pivot_longer(
    cols = c( height_1, height_2),
    names_to = "height_year",
    values_to = "height"
  )

# if NAs not removed height_1 655 and height_2 509

LP_long_height %>% 
  filter(!is.na(height)) %>% 
  count(height_year, name="n")
  # height_1 506
  # height_2 505

# supposed to have ~626 for height_1 and ~510 for height_2

# count number of non-NA values & determine positions
counts_height <- LP_long_height %>% 
  # filter(!is.na(height)) %>% 
  group_by(height_year) %>% 
  summarise(n=n(), .groups="drop") %>% 
  mutate(x_pos =ifelse(height_year == "height_1",2.7,9),                  # position if height_year is height_1_scaled = 2.5, otherwise 9 (height_2)
         y_pos=ifelse(height_year == "height_1", 50, 2))
  # height_1 = 506 measurements
  #' height_2 = 505 measurements
  #' doesn't match with total alive 

nrow(LP %>% filter(status_2 == "alive" & !is.na(height_2)))
  #' 2024: 624 trees that are alive and have height measurement -> 2 missing height measurements
  #' 2025: 509 trees are alive and have height measurement -> 1 missing height measurement

nrow(LP %>%  filter(status_1=="alive"))
nrow(LP %>%  filter(!is.na(height_1)))


# calculate median heights
median_height <- LP_long_height %>% 
  filter(!is.na(height)) %>% 
  group_by(height_year) %>% 
  summarise(med = median(height), .groups="drop")

# comparative histogram
ggplot(LP_long_height, aes(x=height, fill = height_year)) +
  geom_histogram(alpha=0.6, position = "identity")+ 
  geom_text(
    data = counts_height, 
    aes(x=x_pos, y=y_pos,label = paste("n =", n), fill=NULL),
    position = position_dodge(width=1),
    inherit.aes=F,
    vjust=-1
  )+
  geom_vline(
    data = median_height,
    aes(xintercept = med, color =height_year),
    
    linewidth = 1,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  geom_text(
    data = median_height,
    aes(x=med + 2, 
        label = paste0("Median = ", round(med, 2))),
    y = 140,
    vjust=-0.5,
    show.legend = F
  ) +
  scale_color_manual(
    values=c(
      "height_1_scaled" = "black",
      "height_2" = "tomato2"
    )
  ) +
  labs(
    title = "Height in 2024 and 2025",
    x = "Height (mm)",
    y="Frequency"
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("height_1_scaled" = "black", "height_2" = "tomato1"),
    labels = c("height_1_scaled" = "2024", "height_2" = "2025")
  ) +
  theme_minimal()+
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) 

ggsave("height.png")


# height new attempt ------------------------------------------------------

LP_height <- LP %>% 
  filter(status_1 == "alive" & status_2 == "alive") %>% 
  pivot_longer(
    cols = c(height_1, height_2),
    names_to = "year",
    values_to = "height"
  )


median_heights <- LP_height %>% 
  group_by(year) %>% 
  summarise(median_height = median(height, na.rm = TRUE),
            .groups = "drop")

sum(is.na(LP_height$height))
  # 1 NA value

count_heights <- LP_height %>% 
  filter(!is.na(height)) %>%
  group_by(year) %>%
  summarise(n = n(),
            .groups = "drop")
  
  
  
ggplot(LP_height, aes(x = height, fill = year)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
  geom_vline(data = median_heights, aes(xintercept = median_height, color = year),
             linetype = "dashed", size = 1, show.legend = FALSE) +
  geom_text(data = median_heights,
            aes(x = median_height, 
                y = 150,
                label = paste0("Median = ", round(median_height, 1))),
            color = "black",
            vjust = -1.2,
            hjust = -0.2) +
  labs(
    x = "Height (mm)",
    y = "Count",
    fill = "Year"
  ) +
  theme_minimal() +
  scale_fill_manual(
    name = "Year",
    values = c("height_1" = "black", "height_2" = "#9BCD9B"),
    labels = c("height_1" = "2024", "height_2" = "2025")
  ) +
  scale_color_manual(values = c("height_1" = "grey", "height_2" = "#698B69")) +
  theme(
    legend.position = c(0.95,0.95),
    legend.justification = c("right", "top")
  ) 


ggsave("height_histogram.png")


c("#9BCD9B", "#698B69", "#6E8B3D")









# 396 NAs
# from dead trees

range(LP$height_1, na.rm = T)
range(LP$height_2, na.rm = T)
sum(is.na(LP$height_1))
sum(is.na(LP$height_2))


# Comparative boxplot: Height_2 & Provenance
unique(LP$Provenance)

ggplot(LP, aes(x = Provenance, y = height_2, fill=Provenance)) +
  geom_boxplot() +
  labs(title = "Height_2 by Provenance",
       x = "Provenance",
       y = "Height_2") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("height2_Provenance_boxplot.png")

 # Statistical tests
  # is height_2 normally distributed in each provenance lvel?
ggplot(LP, aes(x = height_2, fill = Provenance)) +
  geom_histogram(alpha = 0.6, bins = 20) +
  facet_wrap(~ Provenance) +
  labs(title = "Height 2 Distribution by Provenance", x = "Height 2", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Normality within groups (Shapiro-Wilk test)
LP %>%
  group_by(Provenance) %>%
  summarise(shapiro_p = shapiro.test(height_2)$p.value)
  # only North Coast is normally distributed

kruskal.test(height_2 ~ Provenance, data = LP)
   # highly significant

pairwise.wilcox.test(LP$height_2, LP$Provenance, p.adjust.method = "BH")
  #' NC-Alaska: sig
  #' NC-Skeena: sig
  #' Alaska-Skeena: not sig 
  

# Comparative boxplot: Height & Type
# count how many values for height per type


ggplot(LP, aes(x = Type, y = height_2, fill=Type)) +
  geom_boxplot() +
  labs(title = "Height_2 by Type",
       x = "Type",
       y = "Height_2") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c("Origin" = "#F08080", "Plantation" = "#528B8B", "Regeneration" = "#EEC900")
  )

ggsave("height2_Type_boxplot.png")

# Statistical tests
# is height_2 normally distributed in each type lvel?

ggplot(LP, aes(x = height_2, fill = Type)) +
  geom_histogram(alpha = 0.6, bins = 20) +
  facet_wrap(~ Type) +
  labs(title = "Height 2 Distribution by Type", x = "Height 2", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Normality within groups (Shapiro-Wilk test)
LP %>%
  group_by(Type) %>%
  summarise(shapiro_p = shapiro.test(height_2)$p.value)
# only Regen is normally distributed

kruskal.test(height_2 ~ Type, data = LP)
# highly significant

pairwise.wilcox.test(LP$height_2, LP$Type, p.adjust.method = "BH")
  #' all sig



# BUDSET -------------------------------------------------------------

# PROVENANCE

# Boxplot_ Budset x Provenance
ggplot(LP, aes(x = Provenance, y = Julian_budset)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Budset Timing by Provenance",
       x = "Provenance",
       y = "Julian Day of Budset") +
  theme_minimal()

ggsave("budset_boxplot.png")

# Statistical tests
# is budset normally distributed in each provenance level?

ggplot(LP, aes(x = Julian_budset, fill = Provenance)) +
  geom_histogram(alpha = 0.6, bins = 20) +
  facet_wrap(~ Type) +
  labs(title = "Height 2 Distribution by Provenance", x = "Budset (Julian days", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
  # nope

# Normality within groups (Shapiro-Wilk test)
LP %>%
  group_by(Provenance) %>%
  summarise(shapiro_p = shapiro.test(Julian_budset)$p.value)
# nope

kruskal.test(Julian_budset ~ Provenance, data = LP)
# just about significant

pairwise.wilcox.test(LP$Julian_budset, LP$Provenance, p.adjust.method = "BH")
#' North Coast-Skeena: somewhat sig



# Density plot: Budset x Provenance

ggplot(LP, aes(x = Julian_budset, color = Provenance, fill = Provenance)) +
  geom_density(alpha = 0.3) +
  labs(x = "Budset (Julian days)",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  )

ggsave("budset_provenance_density.png")

# TYPE

# Boxplot: Budset x Type

ggplot(LP, aes(x = Type, y = Julian_budset)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Type",
       y = "Julian Day of Budset") +
  theme_minimal()

ggsave("budset_Type_boxplot.png")

  # weird, origin has no values?
range(LP$Julian_budset[LP$Type=="Origin"], na.rm = T)
  # 1-125

sum(is.na(LP$Julian_budset[LP$Type=="Origin"]))
  # 115 NAs (trees without budset value) -> 185 trees have values
  # 38% don't have budset values


# Normality within groups (Shapiro-Wilk test)
LP %>%
  group_by(Type) %>%
  summarise(shapiro_p = shapiro.test(Julian_budset)$p.value)
# nope

kruskal.test(Julian_budset ~ Type, data = LP)
# very significant

pairwise.wilcox.test(LP$Julian_budset, LP$Type, p.adjust.method = "BH")
#' all but Regen-Plant = sig


# Density plot: Budset x Type

ggplot(LP, aes(x = Julian_budset, color = Type, fill = Type)) +
  geom_density(alpha = 0.3) +
  labs(x = "Budset (Julian days)",
       y="Density") +
  theme_minimal() +
  theme(
  legend.position = c(0.95,1),
  legend.justification = c("right", "top"))

ggsave("budset_Type_density.png")


# BUDBURST ----------------------------------------------------------------

# PROVENANCE

sum(is.na(LP$Julian_budburst[LP$Provenance=="North Coast"]))



# Boxplot_ Budburst x Provenance
ggplot(LP, aes(x = Provenance, y = Julian_budburst)) +
  geom_boxplot(fill = "steelblue") +
  labs(
       x = "Provenance",
       y = "Julian Day of Budburst") +
  theme_minimal()

ggsave("budburst_Provenance_boxplot.png")

# Statistical tests

# Normality within groups (Shapiro-Wilk test)
LP %>%
  group_by(Provenance) %>%
  summarise(shapiro_p = shapiro.test(Julian_budburst)$p.value)
# nope

kruskal.test(Julian_budburst ~ Provenance, data = LP)
# significant

pairwise.wilcox.test(LP$Julian_budburst, LP$Provenance, p.adjust.method = "BH")
#' NC-Alaska: 0.013
#' Skeena-NC: 0.01



# Density plot: Budburst x Provenance

ggplot(LP, aes(x = Julian_budburst, color = Provenance, fill = Provenance)) +
  geom_density(alpha = 0.3) +
  labs(x = "Budburst (Julian days)",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  )

ggsave("budburst_provenance_density.png")

# TYPE

# Boxplot: Budburst x Type

ggplot(LP, aes(x = Type, y = Julian_budburst)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Type",
       y = "Julian Day of Budburst") +
  theme_minimal()

ggsave("budburst_Type_boxplot.png")

# Normality within groups (Shapiro-Wilk test)
LP %>%
  group_by(Type) %>%
  summarise(shapiro_p = shapiro.test(Julian_budburst)$p.value)
# nope

kruskal.test(Julian_budburst ~ Type, data = LP)
# not significant


# Density plot: Budburst x Type

ggplot(LP, aes(x = Julian_budburst, color = Type, fill = Type)) +
  geom_density(alpha = 0.3) +
  labs(x = "Budburst (Julian day)",
       y="Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top"))

ggsave("budburst_Type_density.png")

    

# Budset & Budburst together ----------------------------------------------

# new Day 0 for Julian day
# first recorded budset day = 11/09/2023

LP$date_budset <- as.Date(LP$date_budset)
# date planted = 20-23/6/2023
# first date = 20/6/2023 i.e., 2023/06/20
LP$Julian_budset_2 <- julian(LP$date_budset, origin = as.Date("2023/06/20"))  
    
    
LP$date_budburst <- as.Date(LP$date_budburst)
LP$Julian_budburst_2 <- julian(LP$date_budburst, origin = as.Date("2023/06/20"))   
    
# BUDSET
ggplot(LP, aes(x = Julian_budset_2, color = Provenance, fill = Provenance)) +
  geom_density(alpha = 0.3) +
  labs(x = "Budset (Julian days)",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(1,1),
    legend.justification = c("right", "top")
  )

# TOGETHER - Provenance
ggplot() +
  geom_density(data = LP, 
               aes(x = Julian_budset_2, color = Provenance, fill = Provenance), 
               alpha = 0.3) +
  geom_density(data = LP, 
               aes(x = Julian_budburst_2, color = Provenance, fill = Provenance), 
               alpha = 0.3, linetype = "dashed") +
  labs(x = "Julian days", y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(1,1),
    legend.justification = c("right", "top")
  )

ggsave("BS&BB_Provenance.png")

# TOGETHER - Type
ggplot() +
  geom_density(data = LP, 
               aes(x = Julian_budset_2, color = Type, fill = Type), 
               alpha = 0.3) +
  geom_density(data = LP, 
               aes(x = Julian_budburst_2, color = Type, fill = Type), 
               alpha = 0.3, linetype = "dashed") +
  labs(x = "Julian days", y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(1,1),
    legend.justification = c("right", "top")
  )

ggsave("BS&BB_Type.png")

# Difference between budset and budburst
LP$Julian_difference <- LP$Julian_budburst_2 - LP$Julian_budset_2

# Histogram
ggplot(LP, aes(Julian_difference)) +
  geom_histogram(binwidth = 7, fill = "steelblue", color = "black") +
  labs(x = "Days between budset and budburst", 
       y = "Number of trees") +
  theme_minimal()

  # how are there trees with budburst before budset??
problem_trees <- LP %>% 
  filter(Julian_difference < 0)
  #' 6 trees with budset after budburst: no budset recorded, then budset & budburst simultaneously, then "second" budset afterwards??
  #' 4 Regen (Rowens, North Coast)
  #' 1 Plantation (Rowens, North Coast)
  #' 1 Origin (Alaska)


# create unique Tree ID column
LP$treeID <- paste(LP$Block, LP$Position, sep="-")



ggsave("BS_BB_difference.png")

# How many trees budset and budburst dates for?
  # How many values in LP$Julian_budset_2 and LP$Julian_budburstt_2?

sum(!is.na(LP$Julian_budset_2))
  # 549 trees have budset data

sum(!is.na(LP$Julian_budburst_2))
  # 575 trees have budburst data

# 26 didn't have noticeable buds?

# How many trees both budset and budburst dates for?
sum(!is.na(LP$Julian_budset_2) & !is.na(LP$Julian_budburst_2))
  # 529 have data for both


# MetOffice data ------------------------------------------------------------

  #' import all files in MetData folder
  #' as they're imported, bind them to each other (at bottom)

data_path <- "MetData"

# read & bind all datasets
MetData <- list.files(
  path = data_path,
  pattern = "\\.csv$",   
  full.names = TRUE
) %>%
  map_dfr(~ read_csv(.x, col_types = cols(
    `Report Date / Time` = col_datetime(format = "%Y-%m-%d %H:%M:%S")  # adjust format if needed
  )))

# separate date and time into two columns
colnames(MetData)
MetData <- MetData %>%
  separate(
    col="Report Date / Time",
    into = c("Report_Date", "Report_Time"),
    sep = " "
  ) %>% 
  mutate(
    Report_Date = as.Date(`Report_Date`),
    Report_Time = hms::as_hms(Report_Time)
  )

# how many NAs (were 1344, now 5)
sum(is.na(MetData$Report_Date))
  # 5

NA_summary <- MetData %>% 
  filter(is.na(Report_Date))

# take date from ID column
MetData %>%
  filter(is.na(Report_Date)) %>%
  select(Id, Report_Date) %>% 
  mutate(
    Report_Date = coalesce(
      Report_Date,                        
      as.Date(substr(Id, 1, 8), "%Y%m%d")
    )
  )

# repopulate date column
MetData <- MetData %>%
  mutate(
    # Only fill missing Report_Date values
    Report_Date = coalesce(
      Report_Date,                        
      as.Date(substr(Id, 1, 8), "%Y%m%d")
      )
    )
  # still 5 NAs -> fine

str(MetData$Report_Date)

# check whole range of dates has been imported
range(MetData$Report_Date, na.rm = TRUE)
  # 20.6.2023 to 15.6.2024
unique(MetData$Report_Date)

# remove all unecessary columns
MetData <- MetData %>% 
  select(where(~!all(is.na(.)))) %>% 
  filter(!is.na(Report_Date))

# produce daily average, min and max
MetData_sum <- MetData %>% 
  group_by(Report_Date) %>% 
  summarise(
    daily_avg = mean(`Air Temperature`, na.rm=T),
    daily_min = min(`Air Temperature`, na.rm = T),
    daily_max = max(`Air Temperature`, na.rm=T)
  ) %>% 
  ungroup() %>% 
  filter(Report_Date >= as.Date("2023/06/20") &
           Report_Date <= as.Date("2024/05/22")) %>% 
  mutate(
    Julian_Day_Met = as.integer(julian(Report_Date, origin = as.Date("2023/06/20")))   # add Julian day -> remove all dates before "2023/06/20" (=planting date)
  )

range(MetData_sum$Report_Date, na.rm=T)




# BUDSET & BUDBURST & TEMPERATURES ----------------------------------------

# count how many recorded trees per Julian Day
budset_counts <- LP %>%
  filter(!is.na(Julian_budset_2)) %>%
  count(Julian_Day_2 = Julian_budset_2) %>%   # counts how many trees have that Julian day
  rename(n_budset = n)

budburst_counts <- LP %>%
  filter(!is.na(Julian_budburst_2)) %>%
  count(Julian_Day_2 = Julian_budburst_2) %>%
  rename(n_budburst = n)

# join budset ad budburst counts together
bud_daily_counts <- full_join(budset_counts, budburst_counts, by = "Julian_Day_2") %>%
  # replace NAs with 0
  mutate(
    n_budset = ifelse(is.na(n_budset), 0, n_budset),
    n_budburst = ifelse(is.na(n_budburst), 0, n_budburst)
  ) %>% 
  mutate(Julian_Day_2 = as.integer(Julian_Day_2))

# add daily temperature
Bud_temp <- MetData_sum %>%
  left_join(bud_daily_counts, 
            by = c("Julian_Day_Met" = "Julian_Day_2")) %>% 
  # replace any remaining NAs in bud counts with 0
  mutate(
    n_budset   = ifelse(is.na(n_budset), 0, n_budset),
    n_budburst = ifelse(is.na(n_budburst), 0, n_budburst)
  )

scale_factor <- 25/300


ggplot(Bud_temp, aes(x = Julian_Day_Met)) +
  geom_area(aes(y=daily_avg), fill="#ADD8E6", alpha=0.2) +
  geom_line(aes(y = daily_avg), color = "#ADD8E6", size = 1) +
  geom_bar(aes(y = n_budset*scale_factor), 
           fill = "#CDAA7D", alpha = 0.9, width = 1.5,
           stat="identity", position="stack") +
  geom_bar(aes(y = n_budburst*scale_factor), 
           fill = "#A2CD5A", alpha = 0.9, width = 1.5,
           stat="identity", position="stack") +
  scale_y_continuous(
    name = "Temperature (°C)",
    limits=c(0,25),
    sec.axis = sec_axis(~ ./scale_factor, name = "Number of Trees")
  ) +
  scale_x_continuous(
    breaks = seq(min(Bud_temp$Julian_Day_Met),
                 max(Bud_temp$Julian_Day_Met),
                 by = 10)
  ) +
  theme_minimal() +
  labs(x = "Julian Day") +
  theme(
    axis.title.y.left = element_text(margin = margin(r = 15)),  
    axis.title.y.right = element_text(margin = margin(l = 15)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

ggsave("Bud_Temp.png")

c("#CDB79E", "#A2CD5A", "#6E8B3D", "#CDAA7D")


# Degree days
  #' Budset driven by cold exposure: days with T < 10
  #' Budburst driven by warm exposure: days with T > 5

Bud_temp <- Bud_temp %>%
  arrange(Julian_Day_Met) %>%
  mutate(
    chill_dd = pmax(10 - daily_avg, 0),
    warm_dd  = pmax(daily_avg - 5, 0),
    
    chill_sum = cumsum(chill_dd),
    warm_sum  = cumsum(warm_dd)
  )

ggplot(Bud_temp, aes(x = Julian_Day_Met)) +
  
  geom_area(aes(y = chill_sum), fill = "#7FCDBB", alpha = 0.35) +
  geom_line(aes(y = chill_sum), color = "#2C7FB8", size = 1) +
  
  geom_area(aes(y = warm_sum), fill = "#FDAE6B", alpha = 0.35) +
  geom_line(aes(y = warm_sum), color = "#E6550D", size = 1) +
  
  geom_bar(aes(y = n_budset), stat="identity",
           fill="#CDAA7D", alpha=0.9) +
  geom_bar(aes(y = n_budburst), stat="identity",
         fill="#A2CD5A", alpha=0.9)



