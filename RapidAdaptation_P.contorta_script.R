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
#' Do provenance and budburst/budset/height/root:shoot ratio correlate?
#' first need to know distribution of all variables and traits

# load libraries & import data --------------------------------------------

library(readxl)
data <- read_excel("P:/07793_newLEAF/Workfiles/WP4/RapidAdaptationTrial_MasterSheet.xlsx", sheet = "Pinus contorta")

head(data)
str(data)
ncol(data)
# 28 columns
colnames(data)
#' "Block"                "Position"             "Column"              
#' "Row"                  "Type"                 "Provenance"          
#' "Provenance region"    "Collection site (UK)" "Family"              
#' "Status"               "Date of budset"       "Date of budburst"    
#' "DBB (mm)...13"        "Height (cm)...14"     "Adjusted Height (mm)"
#' "Needle length 1 (mm)" "Needle length 1 (cm)" "Needle length 2 (mm)"
#' "Needle length 2 (cm)" "Date"                 "Ruler"               
#' "Offset"               "Status 22/09/2025"    "Size"                
#' "DBB (mm)...25"        "Date measured DBB"    "Height (cm)...27"    
#' "Date measured"       



# TO DO:
#' change date to days since... (especially for budburst and budset measurements)
#' check range & distribution of numerical values
#' remove NAs
#' distinguish better between first and second measurements (e.g., DBB_1, DBB_2)

hist(data$`DBB (mm)...13`)
range(data$`DBB (mm)...13`)
