library(data.table)
library(ggplot2)

runoff_stations <- readRDS('data/runoff_stations.rds')
runoff_day <- readRDS('data/runoff_day.rds')

str(runoff_day)
str(runoff_stations)
table(runoff_stations$country) # For one variable
table(runoff_stations$station)



apply(X = runoff_stations, MARGIN = 2, FUN = table)

runoff_stats <- runoff_day[, .(mean_day = round(mean(value), 0),
                               sd_day = round(sd(value), 0),
                               min_day = round(min(value), 0),
                               max_day = round(max(value), 0)), by = sname]

#Visualize our daily time seires 
ggplot(data = runoff_day, aes(value)) +
  geom_histogram(fill = "#97B8C2")+
  facet_wrap(~sname, scales = 'free') +
  theme_bw()

# understanding quatiles of the data 
runoff_stats[, quantile(mean_day)]

#subset data by selecting columns 

runoff_stats_class <- runoff_stats[, .(sname, mean_day)]
str(runoff_stats_class)

#Classify our data into low, medium and high values 
runoff_stats_class[, runoff_class := factor("low")]
runoff_stats_class
runoff_stats_class[mean_day >= 1000 & mean_day < 2000, runoff_class := factor("medium") ]
runoff_stats_class[mean_day >= 2000 , runoff_class := factor("high") ]
runoff_stats_class

#random classification based assumed threshold 
runoff_stations[, area_class := factor('small')]
runoff_stations[area >= 30000 & area < 130000, area_class := factor("medium")]
runoff_stations
runoff_stations[area >= 130000, area_class := factor("high")]
runoff_stations

#altitude classification 

runoff_stations[, alt_class := factor('low')]
runoff_stations[altitude >= 50 & altitude < 400, alt_class := factor('medium')]
runoff_stations[altitude >= 400, alt_class := factor('high')]
runoff_stations

# subsetting and merging the runoff data 
to_merge <- runoff_stats_class[, .(sname, runoff_class)]
runoff_summary <- runoff_stations[to_merge, on = 'sname']
runoff_summary

# Agreegation of te data 
runoff_day

#adding year column to runoff_day 
runoff_day[, year := year(date)]
runoff_day
#adding month column to runoff_day
runoff_day[, month := month(date)]
runoff_day

#monthly runoff 
runoff_month <- runoff_day[, .(value = mean(value)), by = .(month, year, sname)]
runoff_month
runoff_month[, date := as.Date(paste0(year,"-", month, "-1"))]
runoff_month

#monthly runoff analysis 
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

ggplot(data = runoff_month, aes(x = factor(month), y = value)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~sname, scales = 'free') + 
  theme_bw()

#yearly aggregation of runoff data 
runoff_year <- runoff_day[, .(value = mean(value)), by = .(year, sname)]
runoff_year

#analysing the annual time seires 
ggplot(runoff_year[year > 1980], aes(x = year, y = value)) +
  geom_line(col =  colset_4[1]) +
  geom_point(col = colset_4[1]) + 
  facet_wrap(~sname, scales = 'free') +
  theme_minimal()

#Seasonal aggregation 
runoff_day[month == 12 | month == 1 | month == 2, season := "winter"]
runoff_day
runoff_day[month == 3 | month == 4 | month == 5, season := 'spring']
runoff_day[month == 6 | month == 7 | month == 8, season := 'summer']
runoff_day[month == 9 | month == 10 | month == 11, season := 'autumn']
runoff_day
str(runoff_day)
#runoff_day[, season := factor(season)]
str(runoff_day)

runoff_day[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]
str(runoff_day)


runoff_winter <- runoff_day[season == 'winter', .(value = mean(value)), by = .(sname, year)]
runoff_winter
runoff_summer <- runoff_day[season == 'summer', .(value = mean(value)), by = .(sname, year)]
runoff_summer

#normalixation 
runoff_year[, value_norm := (value - mean(value)) / sd(value), by = sname]
n_stations <- nrow(runoff_summary)

#visulalization of normalization of the data 
ggplot(runoff_year[year > 1980], aes(x = year, y = value_norm, col = sname)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  theme_bw()

#spatial visualization of stations 
library(mapview) 
library(sf) 

station_coords_sf <- st_as_sf(runoff_summary, 
                              coords = c("lon", "lat"), 
                              crs = 4326)
mapview(station_coords_sf, map.types = 'Stamen.TerrainBackground')
#spatial plot of stations 
mapview(station_coords_sf, map.types = mapviewGetOption("basemaps"))

#increasing or decreasing order for altitude
runoff_summary <- runoff_summary[order(-altitude)]
runoff_summary
runoff_summary <- runoff_summary[c(1:15, 17:16)]
runoff_summary <- cbind(order_id = 1:17, runoff_summary)
runoff_summary[, sname := reorder(sname, order_id)]

runoff_day[, sname := factor(sname, levels = runoff_summary$sname)]

str(runoff_month)
runoff_month[, sname := factor(sname, levels = runoff_summary$sname)]
str(runoff_month)
runoff_summer[, sname := factor(sname, levels = runoff_summary$sname)]
runoff_winter[, sname := factor(sname, levels = runoff_summary$sname)]
runoff_year[, sname := factor(sname, levels = runoff_summary$sname)]


dt <- runoff_summary[, .(sname, area, alt_class)]
to_plot <- runoff_stats[dt, on = 'sname']
to_plot <- to_plot[, alt_class := factor(alt_class, levels = c("low", "medium", "high"))]
str(to_plot)


# plotting alitiude, mean day at each station 
ggplot(data = to_plot, aes(x = mean_day, y = area, col = sname, cex = alt_class)) +
  geom_point() +
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  theme_bw()

#saving rds file 
saveRDS(runoff_summary, './data/runoff_summary.rds')
saveRDS(runoff_stats, './data/runoff_stats.rds')
saveRDS(runoff_day, './data/runoff_day.rds')
saveRDS(runoff_month, './data/runoff_month.rds')
saveRDS(runoff_summer, './data/runoff_summer.rds')
saveRDS(runoff_winter, './data/runoff_winter.rds')
saveRDS(runoff_year, './data/runoff_year.rds')
