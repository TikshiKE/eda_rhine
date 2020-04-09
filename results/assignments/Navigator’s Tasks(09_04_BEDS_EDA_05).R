##

library(data.table)
library(ggplot2)

runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_day <- readRDS('data/runoff_day.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

#Nav's q1

year_thres <- 2000

runoff_year_key[year < year_thres, period := factor('pre_2000')]
runoff_year_key[year >= year_thres, period := factor('aft_2000')]
runoff_month_key[year < year_thres, period := factor('pre_2000')]
runoff_month_key[year >= year_thres, period := factor('aft_2000')]

to_plot <- rbind(cbind(runoff_year_key, season = factor('year')), 
                 cbind(runoff_month_key, season = factor('month')),fill=TRUE) 

ggplot(to_plot, aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

###We can se that for stations BARS and KOEL annual runoff has increased after 2000, but decreased for the station DOMA.

##Nav's q2

quantiles <- runoff_day[,.(quantile(value, c(0.1)),quantile(value, c(0.9))), by = sname]

runoff_day_new <- merge(runoff_day, quantiles, by = 'sname')
runoff_days_rating <- data.table(above_high = runoff_day_new[value >= V2,.N], below_low = runoff_day_new[value < V1, .N])
runoff_days_rating

