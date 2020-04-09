##

library(data.table)
library(ggplot2)

runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')

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



