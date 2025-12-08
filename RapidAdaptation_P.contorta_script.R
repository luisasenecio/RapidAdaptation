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

# load libraries & import data --------------------------------------------
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
  mutate(across(c(DBB_1, height_1, needle_1, needle_2, height_2, DBB_2),as.numeric))
    # NAs introduced by coercion

# CONVERT INTO JULIAN DAYS

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

# STATUS ------------------------------------------------------------------

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

# STATUS BY TYPE

StatusTypeCount <- LP %>% 
  count(status_2, Type)

ggplot(StatusTypeCount, aes(x = status_2, y=n, fill=Type)) +
  geom_col(position = position_dodge(width=0.9)) +
  theme_minimal() +
  labs(title = "Tree status by Type", x = "Status_2", y = "Count") +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  ) +
  scale_fill_manual(
    values = c("Origin" = "#F08080", "Plantation" = "#528B8B", "Regeneration" = "#EEC900")
  )

ggsave("StatusByType.png")

# STATUS BY PROVENANCE

StatusProvenanceCounts <- LP %>% 
  count(Provenance, status_2)

ggplot(StatusTypeProvenance, aes(x = status_2, y=n, fill=Type)) +
  geom_col(position = position_dodge(width=0.9)) +
  theme_minimal() +
  labs(title = "Tree status by Provenance", x = "Provenance", y = "Count") +
  theme(
    legend.position = c(0.95,1),
    legend.justification = c("right", "top")
  )

ggsave("StatusByProvenance.png")


# STATUS BY BLOCK

StatusBlockCounts <- LP %>% 
  count(Block, status_2)

ggplot(StatusBlockCounts, aes(x=factor(Block), y=n, fill=status_2)) +
  geom_col(position="dodge") +
  theme_minimal() +
  labs(
    x="Block", y="Count",
    fill="Status"
  ) 

ggsave("StatusByBlock.png")

# STATUS BY FAMILY

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

StatusFamilyCounts <- LP_2 %>% 
  count(Family, status_2)

ggplot(StatusFamilyCounts, aes(x = Family, y = n, fill = status_2)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Tree Status per Family",
    x = "Family",
    y = "Number of Trees",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate family labels
  )


# Type distribution -------------------------------------------------------

colnames(LP)

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

# Distribution of Types
ggplot(LP, aes(x=Type, fill=Type)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c("Origin" = "#F08080", "Plantation" = "#528B8B", "Regeneration" = "#EEC900")
  ) +
  labs(y="Number of trees") +
  theme_minimal()

ggsave("TypeDistribution.png")



# Provenance distribution -------------------------------------------------

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
  pivot_longer(
    cols = c(DBB_1, DBB_2),
    names_to = "DBB_year",
    values_to = "DBB"
  )

counts <- LP_long_DBB %>% 
  filter(!is.na(DBB)) %>% 
  group_by(DBB_year) %>% 
  summarise(n=n(), .groups="drop") %>% 
  mutate(x_pos =ifelse(DBB_year == "DBB_1", 0.9, 2.74),
         y_pos=20)

# calculate median heights
median_DBB <- LP_long_DBB %>% 
  filter(!is.na(DBB)) %>% 
  group_by(DBB_year) %>% 
  summarise(med = median(DBB), .groups="drop")

# comparative histogram
ggplot(LP_long_DBB, aes(x=DBB, fill = DBB_year)) +
  geom_histogram(alpha=0.8, position = "identity") +
  geom_text(
    data = counts, 
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
  labs(title = "DBB in 2024", x = "DBB (cm)", y ="Frequency") +
  labs(
    title = "DBB in 2024 and 2025",
    x = "DBB (cm)"
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("DBB_1" = "steelblue", "DBB_2" = "tomato"),
    labels = c("DBB_1" = "2024", "DBB_2" = "2025")
  ) +
  theme_minimal()

# 296 NAs
ggsave("DBB_histogram.png")


range(LP$DBB_1, na.rm = T)
range(LP$DBB_2, na.rm = T)
sum(is.na(LP$DBB_1))
sum(is.na(LP$DBB_2))

# Height ------------------------------------------------------------------

# long format + scaling into cm
LP_long_height <- LP %>%
  mutate(height_1_scaled = height_1 / 10) %>% 
  pivot_longer(
    cols = c(height_1_scaled, height_2),
    names_to = "height_year",
    values_to = "height"
  )
 
# count number of non-NA values & determine positions
counts_height <- LP_long_height %>% 
  filter(!is.na(height)) %>% 
  group_by(height_year) %>% 
  summarise(n=n(), .groups="drop") %>% 
  mutate(x_pos =ifelse(height_year == "height_1_scaled",2.7,9),                  # position if height_year is height_1_scaled = 2.5, otherwise 9 (height_2)
         y_pos=ifelse(height_year == "height_1_scaled", 50, 2))

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
  labs(title = "Height in 2024", x = "Height (cm)") +
  labs(
    title = "Height in 2024 and 2025",
    x = "Height (cm)",
    y="Frequency"
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("height_1_scaled" = "black", "height_2" = "tomato1"),
    labels = c("height_1_scaled" = "2024", "height_2" = "2025")
  ) +
  theme_minimal()

ggsave("height.png")

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



# Julian days -------------------------------------------------------------

# BUDSET
ggplot(LP, aes(x=Julian_budset)) +
  geom_histogram() 
sum(is.na(LP$Julian_budset))
# 231 NAs

ggplot(LP, aes(x = Provenance, y = Julian_budset)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Budset Timing by Provenance",
       x = "Provenance",
       y = "Julian Day of Budset") +
  theme_minimal()

ggsave("budset_boxplot.png")

# Density plots to show distribution of budset between Origin type
ggplot(LP, aes(x = Julian_budset, color = Type, fill = Type)) +
  geom_density(alpha = 0.3) +
  labs(x = "Budset (Julian days)",
       title = "Density of Budset Days by Type") +
  theme_minimal()

ggsave("budset_density.png")

# BUDBURST

# Boxplot between Provenances
ggplot(LP, aes(x = Provenance, y = Julian_budburst)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Budurst Timing by Provenance",
       x = "Provenance",
       y = "Julian Day of Budurst") +
  theme_minimal()

ggsave("budburst_boxplot.png")

# Histogram of all values, no grouping
ggplot(LP, aes(x=Julian_budburst)) +
  geom_histogram(fill="#79CDCD") +
  labs(x="budburst (Julian days)", title ="Distribution of budburst days") +
  theme_minimal()

ggsave("budset_histogram.png")

# Density plots to show distribution of budburst between Origin type
ggplot(LP, aes(x = Julian_budburst, color = Type, fill = Type)) +
  geom_density(alpha = 0.3) +
  labs(x = "Budburst (Julian days)",
       title = "Density of Budburst Days by Type") +
  theme_minimal() +
  labs(
    x="Density"
  )

ggsave("budburst_density.png")


    
    
    
    
    
    
    
