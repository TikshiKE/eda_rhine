##Explorer’s Questions 06.04. BEDS_EDA_02

RhineCath = 185000 ##km2 Rhine basin size

precipitation_day <- (5*24)/1000 #m/d

avg_discharge_day <- 2900 * 60 * 60 * 24 #m3/d

avg_discharge_change <- precipitation_day * (RhineCath * 1000000) #m3/d
