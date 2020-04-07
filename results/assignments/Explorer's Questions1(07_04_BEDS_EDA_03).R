##

library(data.table)
library(ggplot2)

runoff_stations <- readRDS('./data/runoff_stations.rds')
runoff_day <- readRDS('./data/runoff_day.rds')

avg_catchment <- mean(runoff_stations[,area])

avg_catchment ##average catchment of Rhine

avg_runoff <- mean(runoff_day[,value])

avg_runoff ##average runoff of Rhine

###Which is the average runoff in each station? Present them in a graph.


runoff_day_mean <- aggregate(runoff_day[,4],list(runoff_day$sname), mean)

colnames(runoff_day_mean) <- c('Name','Value')


ggplot(data = runoff_day_mean, aes(x = Name, y = Value,)) +
  geom_point() +
  geom_text(aes(label=round(Value, digits = 1)), hjust=0, vjust=0)