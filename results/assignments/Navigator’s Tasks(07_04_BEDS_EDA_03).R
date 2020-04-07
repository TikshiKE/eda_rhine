###

library(data.table)
library(ggplot2)

runoff_stations <- fread('./data/raw/runoff_stations.csv')

##first graph

runoff_stations[, sname := factor(abbreviate(station))]
runoff_stations[, id := factor(id)]
runoff_stations[, altitude := round(altitude, 0)]

runoff_stations_firstG <- runoff_stations[,.(sname, area, altitude)]

runoff_stations_firstG$size <- runoff_stations_firstG[,.((area/altitude)*10/2)]

ggplot(data = runoff_stations_firstG, 
       aes(x = area, y = altitude, color = size)) +
  geom_point() +
  geom_text(aes(label=sname),hjust=0, vjust=0)

###second graph
runoff_stations[, lat := round(lat, 3)]
runoff_stations[, lon := round(lon, 3)]

runoff_stations_secondG <- runoff_stations[,.(sname, lon, lat)]
runoff_stations_secondG$altitude <- runoff_stations_firstG[,altitude]

ggplot(data = runoff_stations_secondG, 
       aes(x = lon, y = lat, color = altitude)) +
  geom_point() +
  geom_text(aes(label=sname),hjust=0, vjust=0) +
  scale_color_gradient(low="darkgreen", high="darkred")

###third graph(first try)

runoff_day_compG <- readRDS('./data/runoff_day.rds')

rees_dier_runoff_day <- runoff_day_compG[sname == 'REES' | sname == 'DIER' | sname == 'DUES' | sname == 'KOEL' | sname == 'ANDE' | sname == 'KAUB' | sname == 'MAIN' | sname == 'SPEY' | sname == 'WORM' | sname == 'MAXA' | sname == 'RHEI' | sname == 'LOBI' | sname == 'BASE' | sname == 'BASR' | sname == 'BASS' | sname == 'RHEM' | sname == 'REKI' | sname == 'NEUF' | sname == 'DOMA' | sname == 'FELS' ]

ggplot(data = rees_dier_runoff_day) +
  geom_line(aes(x = date, y = value, col = sname))

###third graph(second try and the best one)

ggplot(data = runoff_day_compG)+
  geom_boxplot(aes(x = sname, y = date))