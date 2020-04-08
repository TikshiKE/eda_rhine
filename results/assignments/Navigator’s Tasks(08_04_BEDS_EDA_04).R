##

#preparing enviroment
library(data.table)
library(ggplot2)

names <-  c("MEAN", "MEDIAN", "MIN", "MAX")

runoff_stats <- readRDS('data/runoff_stats.rds')
runoff_day <- readRDS('./data/runoff_day.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_summary <- readRDS('./data/runoff_summary.rds')

## scatter plot of mean, median, maximum and minimum of runoff_stats (q1)

runoff_median <- runoff_day[,.(round(median(value),0)), by = sname]

runoff_stats$median <- runoff_median$V1

ggplot(runoff_stats) + 
  geom_point(aes(x = sname, y = mean_day,col = names[1]),shape = 15) +
  geom_point(aes(x = sname, y = median,col = names[2]),shape = 16) +
  geom_point(aes(x = sname, y = min_day,col = names[3]),shape = 17) +
  geom_point(aes(x = sname, y = max_day,col = names[4]),shape = 18)

## calculating and adding skewness and coefficient of variation (q2)

runoff_stats$skewness <- round((runoff_stats$mean_day - runoff_stats$median)/runoff_stats$sd_day, d = 3)

runoff_stats$coef_of_var <- round((runoff_stats$sd_day/runoff_stats$mean_day), d = 3)

new_dt_skew_and_Vcoef <- runoff_stats[,c(1,7,8)]

##daily runoff by stations(q4)

ggplot(runoff_day, aes(x = sname, y = value)) +
  geom_boxplot() +
  theme_bw()

## we see that stations DOMA have the lovest value and LOBI the biggest. It may have connections with stations placement on the river(LOBI and REES located at the Alps) or with altitude of stations.

##Creating own classes for area and altitude and ploting them(q5)

runoff_stats[, quantile(altitude)]

runoff_stations[, area_class := factor('small')]
runoff_stations[area >= 10000 & area < 100000, area_class := factor('medium')]
runoff_stations[area >= 100000, area_class := factor('large')]

runoff_stations[, alt_class := factor('low')]
runoff_stations[altitude >= 100 & altitude < 300, alt_class := factor('medium')]
runoff_stations[altitude >= 300, alt_class := factor('high')]

runoff_stations$mean_day <- runoff_stats$mean_day

ggplot(runoff_stations, aes(x = mean_day, y = area, col = area_class, cex = alt_class)) +
  geom_point()

##Expl's q4(not finished)

runoff_summer <- readRDS('./data/runoff_summer.rds')
runoff_winter <- readRDS('./data/runoff_winter.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_year <- readRDS('./data/runoff_year.rds')


##код бесполезный, не хейлайтит месяцы

runoff_max <- data.table(value1 = runoff_winter[,max(value),by = sname], value2 = runoff_summer[,max(value),by = sname], value3 = runoff_month[,max(value),by = sname], value4 = runoff_year[,max(value),by = sname])
runoff_max

runoff_max[,c('value2.sname','value3.sname','value4.sname'):=NULL]

colnames(runoff_max) <- c('sname','winter','summer','month','year')


runoff_min <- data.table(value1 = runoff_winter[,min(value),by = sname], value2 = runoff_summer[,min(value),by = sname], value3 = runoff_month[,min(value),by = sname], value4 = runoff_year[,min(value),by = sname])
runoff_min

runoff_min[,c('value2.sname','value3.sname','value4.sname'):=NULL]

colnames(runoff_min) <- c('sname','winter','summer','month','year')


min(runoff_summer$value)

runoff_summer[runoff_summer[,I[value == max(value)], by = sname]$V1] ##попробовать это
