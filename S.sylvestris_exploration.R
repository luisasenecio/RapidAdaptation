install.packages("plotly")
library(plotly)




SP <- read_excel("P:/07793_newLEAF/Workfiles/WP4/RapidAdaptationTrial_MasterSheet.xlsx", sheet = "Pinus sylvestris")

SP <- SP %>% 
  rename(
    totalMass = "Total dry mass (g)",
    rootMass = "Root dry mass (g)",
    height_2 = "Height (cm)...32")


colnames(SP)

# TOTAL MASS X ROOT MASS
SP %>%
  filter(rootMass == max(rootMass, na.rm = TRUE)) %>%
  select(rootMass, totalMass, Block, Position)


SP %>%
  filter(totalMass == max(totalMass, na.rm = TRUE)) %>%
  select(rootMass, totalMass, Block, Position)

plot_ly(
  data = SP,
  x = ~totalMass,
  y = ~rootMass,
  type = "scatter",
  mode = "markers",
  text = ~paste("Block:", Block, "<br>Position:", Position),
  hoverinfo = "text"
)


# TOTAL MASS x HEIGHT
SP$`Height (cm)...32` <- as.numeric(SP$`Height (cm)...32`)

plot_ly(
  data = SP,
  x = ~totalMass,
  y = ~height_2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Block:", Block, "<br>Position:", Position),
  hoverinfo = "text"
)

help(plot_ly)

  # 10/45 out of pattern: heavy but small DBB

# TOTAL MASS X DBB

plot_ly(
  data = SP,
  x = ~totalMass,
  y = ~`DBB (mm)`,
  type = "scatter",
  mode = "markers",
  text = ~paste("Block:", Block, "<br>Position:", Position),
  hoverinfo = "text"
)
  # 10/45 and 3/14 out of pattern
  # 7/102 just really heavy
