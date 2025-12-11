#' newLeaf - Rapid Adaptation
#' This project analyses rapid adaptation in P. sylvestris, P. contorta, and T. heterophylla
#' This script focuses on P. contorta (Lodgepole pine)
#' Experimental setup:
#' 1. Origin population in North America (Skeena river)
#' 2. Seed taken from origin populations and germinated in UK & planted at Rowens & Benmore -> mature plantations
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
  mutate(height_2 = height_2*10)
    # NAs introduced by coercion


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


# summary counts
TypeProvenanceCounts <- LP %>% 
  count(Provenance, Type)

StatusTypeProvenance <- LP %>% 
  count(Provenance, Type, status_2)

# Status across ------------------------------------------------------------------

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

# Status by Family --------------------------------------------------------


length((LP$Family[LP$Family=="sub for LPNC - UKA4 - actually LPNC - UKA1"]))

LP_2 <- LP %>% 
  mutate(
    Family=ifelse(Family=="sub for LPNC - UKA4 - actually LPNC - UKA1",
                  "LPNC - UKA1",
    Family
    )
  )

length(unique(LP_2$Family))
  # 54 families

StatusFamily <- LP_2 %>% 
  group_by(Family) %>% 
  summarise(
    total=n(),
    alive=sum(status_2 == "alive"),
    proportion_alive =alive/total) %>% 
  ungroup()

StatusFamily_sub <- StatusFamily %>% 
  arrange(desc(proportion_alive)) %>% 
  slice(c(1:10, (n()-9):n())) %>% 
  mutate(percent_alive = proportion_alive*100)

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

# TOGETHER
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

# Difference between budset and budburst
LP$Julian_difference <- LP$Julian_budburst_2 - LP$Julian_budset_2


