#' newLeaf - Rapid Adaptation
#' This project analyses rapid adaptation in P. sylvestris, P. contorta, and T. heterophylla
#' This script focuses on P. contorta (Lodgepole pine)
#' Experimental setup:
#' 1. Origin population in North America (Skeena river)
#' 2. Seed germinated in UK & planted at Rowens & Benmore -> mature plantations
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
  labs(fill = "Status")

#' make histograms of DBB, height, budburst & budset?
#' across all blocks or separated by blocks?


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
  mutate(x_pos =ifelse(DBB_year == "DBB_1",0.5,2.5),
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
    aes(x=x_pos, y=15,label = paste("n =", n), fill=NULL),
    position = position_dodge(width=1),
    inherit.aes=F,
    vjust=-1
  )+
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
  labs(title = "DBB in 2024", x = "DBB (cm)") +
  labs(
    title = "DBB in 2024 and 2025",
    x = "DBB (cm)"
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("DBB_1" = "steelblue", "DBB_2" = "tomato"),
    labels = c("DBB_1" = "2024", "DBB_2" = "2025")
  )

# 296 NAs


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
  scale_color_manual(
    values=c(
      "height_1_scaled" = "black",
      "height_2" = "tomato2"
    )
  ) +
  labs(title = "Height in 2024", x = "Height (cm)") +
  labs(
    title = "Height in 2024 and 2025",
    x = "Height (cm)"
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("height_1_scaled" = "black", "height_2" = "tomato1"),
    labels = c("height_1_scaled" = "2024", "height_2" = "2025")
  )

# 396 NAs

range(LP$height_1, na.rm = T)
range(LP$height_2, na.rm = T)
sum(is.na(LP$height_1))
sum(is.na(LP$height_2))
