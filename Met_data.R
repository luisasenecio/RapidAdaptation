
require(data.table)
require(lubridate)
require(ggplot2)
require(magrittr)

dir<- "C:/Users/clason/OneDrive - UKCEH/Exmoor_Monitoring/Pre-monitoring assessment/"
setwd(dir)



## Air temp sefton ----

# read in data

# Folder for each wow site
"Exton" "Liscombe" "Porlock"
fldr <- "Exton"

# read in all the files
fns <- list.files(paste0("./MetData/",fldr), full.names = T,
                  pattern=".csv$")
dt <- lapply(fns, function(i) {
  fread(i)}) %>%
  rbindlist


## add in months
dt[,label_month := paste0(lubridate::month(`Report Date / Time`,
                                           label=T,abbr=T),
                        " ",  substring(year(`Report Date / Time`),3))]


## Temp ----
ggplot()+
  geom_boxplot(data = dt,
               aes(
                 reorder(label_month, `Report Date / Time`, FUN = median),
                 `Air Temperature`),
               fill = "orange",
               color = "darkorange3") +
  ylab("Air Temperature (C)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,
                                   angle = 90),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10))

ggsave(paste0("./Figures/",fldr,
              "_Temp_boxplot_Nov24_Oct25.png"),
       width = 4, height = 3)




# wind roses ----

wind_lkup <- data.table(comp=c("N","NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                               "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW","N"),
                        deg_mn = c(0,seq(11.25,348.75,22.5)),
                        deg_mx = c(seq(11.25,348.75,22.5),360))

# filter to just wind info
df <- dt[,c(5,22,23,62)] %>% setnames(names(dt[,c(5,22,23,62)]),
                                  c('date','ws','wd','monthlab'))
# get wind categories
df[, wd_cat := cut(wd, breaks = c(0,wind_lkup$deg_mx),
                   labels  = c(wind_lkup$comp),
                   include.lowest = T)]
df <- df[ws > 0,] # filter to only wind times
# wind speed categories
df[, ws_cat := cut(ws, breaks = c(0,2,4,6,12,20,50),
                         labels = c("0 - 2","> 2 - 4","> 4 - 6",
                                    "> 6 - 12","> 12 - 20",
                                    "> 20 - 50"),
                         include.lowest = T)]
# month labels
df[,Month := format(date, "%b %y")]
df[,Month := factor(Month, levels = unique(df[order(date),Month]))]

# frequency counts
x <- df[, .(freq = .N),by = .(wd_cat,ws_cat)]
x[,sum_meas := sum(freq)]
x[, pct := (freq/sum_meas)*100]

######### for ests ###########
y <- df[, .(freq = .N),by = .(wd_cat)]
y[,sum_meas := sum(freq)]
y[, pct := (freq/sum_meas)*100]
###########################

######### for mtnhs ###########
z <- df[, .(freq = .N),by = .(wd_cat,ws_cat,Month)]
# z <- df[date<as.Date("2023-09-06")] %>% 
#   .[, .(freq = .N),by = .(wd_cat,ws_cat,Month)]
z[,sum_meas := sum(freq),by = .(Month)]
z[, pct := (freq/sum_meas)*100]
###########################

# this is horrible and manual rn
mnths <- unique(z$Month) 

strt_mnth <- mnths[1]
intrvl <- 4

ls <- split(mnths, ceiling(seq_along(mnths)/intrvl))
ls_dt <- split(z[order(z$Month),], by = "Month")

for (i in 1:length(ls)){
  print(ls[[i]])
  
  mnths <- ls[[i]]

  time_rng <- mnths %>%
    .[c(1,length(.))] %>%
    gsub(" ","",.) %>%
    paste(collapse = "_to_")
  
  
  p <- ggplot() +
    geom_bar(data = z[Month %in% mnths,],
             aes(x = wd_cat,
                 y = pct,
                 fill = forcats::fct_rev(ws_cat)),
             stat = "identity",
             color = "black",
             linewidth = 0.3) +
    facet_wrap(~Month, ncol = 2) +
    coord_polar(start = -(pi/16)) +
    scale_fill_brewer(name = bquote("Wind\nSpeed (m"~s^-1*")"),
                      palette = "YlOrRd",
                      direction = -1) +
    scale_x_discrete(breaks = unique(wind_lkup$comp),
                     labels = c("N","","","",
                                "E","","","",
                                "S","","","",
                                "W","","",""))
  
  p <- p + theme(axis.title.y=element_blank(),
                 axis.title.x=element_blank(),
                 strip.background = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.text.x = element_text(size = 14,
                                            color = "black",
                                            face = "bold"),
                 panel.background = element_blank(),
                 legend.text = element_text(size = 10,
                                            color = "black"),
                 legend.title = element_text(size = 10,
                                             color = "black",
                                             face = "bold"),
                 legend.position = "bottom",
                 strip.text = element_text(size = 10,
                                           color = "black",
                                           face = "bold"),
                 panel.border = element_blank(),
                 panel.grid.major = element_line(color = "grey",size = 0.2),
                 panel.grid.minor = element_blank()) 
  
  p <- p+guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  p
  
  ggsave(paste0("./Figures/",fldr,
                "_windrose_", time_rng,".png"),
         height = 5.2)
}


## precipitation baby ----
# read in data
# need to be carefule with rainfall if its accumulation or total
# and if its accumulation on what time unit - days?
# also is it mm or m?
dt[,"Rainfall Rate" := as.numeric("Rainfall Rate")]
rain_summary <- dt[, .(total_rain = sum("Rainfall Rate", na.rm = TRUE),
                          mn_date = mean(TIMESTAMP)),
                      by = label_month]

# seeting the months as fatcor in the order i want them in
# so it plots nicely 
rain_summary[, orig_label_month := 
               factor(rain_summary$orig_label_month,
                      unique(rain_summary[order(rain_summary$mn_date),]$orig_label_month))]


ggplot(data = rain_summary,
       aes(x = orig_label_month,
           y = total_rain)) +
  geom_bar(stat = "identity",
           fill = "dodgerblue2")+
  ylab("Precipitation (mm)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,
                                   angle = 90),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10))

ggsave("./Figures/CorsFochno_precipitation.png",
       height = 4)

