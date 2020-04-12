##
#Exp's q1
#The house is not representative of the station. One of the reasons may be the location of the station, namely the presence of a large reservoir nearby.

##Exp's q2

library(data.table)
library(ggplot2)

precip_day <- readRDS('data/precip_day.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

df <- precip_day
df[, year := year(date)]
df[, men_year := mean(value), by = year]
to_plot <- unique(df[,c('year','men_year')])
to_plot <- to_plot[1:253,] ##deleting the mean value for 2019, since it's = 0


ggplot(to_plot[year>1900], aes(x = year, y = men_year)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Years") +
  ylab(label = "Precipitations") +
  theme_bw()

ggplot(to_plot, aes(x = year, y = men_year)) +
  geom_boxplot(data = to_plot[year>1920 & year<1940]) +
  geom_boxplot(data = to_plot[year>1940 & year<1960]) +
  geom_boxplot(data = to_plot[year>1960 & year<1980]) +
  geom_boxplot(data = to_plot[year>1980 & year<2000]) +
  geom_boxplot(data = to_plot[year>2000])
#As expected, precipitation is well distributed on average.

#Exp's q3
#We observed direct changes over time in the river flow in the summer and winter periods, which were predicted by the research team earlier.

#Exp's q4
#Perhaps more emphasis should be placed on studying the impact of the technical and agricultural complex on the flow of the river. Factors such as the presence of dams, water intake for irrigation of fields, and other economic activities can affect the flow of the river.