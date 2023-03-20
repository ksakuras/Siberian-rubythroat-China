library(geosphere)

#5D6####
gdl <- "5D6"
load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
X5D6 = as.data.frame(matrix(,22,2))
colnames(X5D6) = c("lon","lat")
X5D6$lat = shortest_path$lat
X5D6$lon = shortest_path$lon
X5D6$season = c("autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn",
                "spring","spring","spring","spring","spring","spring","spring","spring","spring","spring")
for(i in 2:nrow(X5D6)){
  X5D6$dist[i] = distm(c(X5D6$lon[i-1],X5D6$lat[i-1]),c(X5D6$lon[i],X5D6$lat[i]),fun = distHaversine)
  X5D6$dist[i] = X5D6$dist[i]/1000
}
#beeline distance
beeline = distm(c(X5D6$lon[12],X5D6$lat[12]),c(X5D6$lon[22],X5D6$lat[22]),fun = distHaversine)/1000
#early migration distance
X5D6$dist[1] = distm(c(X5D6$lon[1],X5D6$lat[1]),c(X5D6$lon[22],X5D6$lat[22]),fun = distHaversine)/1000

#total seasonal distance
sum(subset(X5D6, season == "autumn")$dist)
sum(subset(X5D6, season == "spring")$dist)

#extended distance


#5D7 ####
gdl <- "5D7"
load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
X5D7 = as.data.frame(matrix(,20,2))
colnames(X5D7) = c("lon","lat")
X5D7$lat = shortest_path$lat
X5D7$lon = shortest_path$lon
X5D7
X5D7$season = c("autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn",
                "spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring")
for(i in 2:nrow(X5D7)){
  X5D7$dist[i] = distm(c(X5D7$lon[i-1],X5D7$lat[i-1]),c(X5D7$lon[i],X5D7$lat[i]),fun = distHaversine)
  X5D7$dist[i] = X5D7$dist[i]/1000
}
#beeline distance
beeline = distm(c(X5D7$lon[9],X5D7$lat[9]),c(X5D7$lon[20],X5D7$lat[20]),fun = distHaversine)/1000
#early migration distance
# X5D7$dist[1] = distm(c(X5D7$lon[1],X5D7$lat[1]),c(X5D7$lon[20],X5D7$lat[20]),fun = distHaversine)/1000

#total seasonal distance
sum(subset(X5D7, season == "autumn")$dist)
sum(subset(X5D7, season == "spring")$dist)

#5D8 ####
gdl <- "5D8"
load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
X5D8 = as.data.frame(matrix(,19,2))
colnames(X5D8) = c("lon","lat")
X5D8$lat = shortest_path$lat
X5D8$lon = shortest_path$lon
X5D8
X5D8$season = c("autumn","autumn","autumn","autumn","autumn","autumn","autumn",
                "spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring")
for(i in 2:nrow(X5D8)){
  X5D8$dist[i] = distm(c(X5D8$lon[i-1],X5D8$lat[i-1]),c(X5D8$lon[i],X5D8$lat[i]),fun = distHaversine)
  X5D8$dist[i] = X5D8$dist[i]/1000
}
#beeline distance
beeline = distm(c(X5D8$lon[7],X5D8$lat[7]),c(X5D8$lon[19],X5D8$lat[19]),fun = distHaversine)/1000
#early migration distance
X5D8$dist[1] = distm(c(X5D8$lon[1],X5D8$lat[1]),c(X5D8$lon[19],X5D8$lat[19]),fun = distHaversine)/1000

#total seasonal distance
sum(subset(X5D8, season == "autumn")$dist)
sum(subset(X5D8, season == "spring")$dist)

#5E5 ####
gdl <- "5E5"
load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
X5E5 = as.data.frame(matrix(,14,2))
colnames(X5E5) = c("lon","lat")
X5E5$lat = shortest_path$lat
X5E5$lon = shortest_path$lon
X5E5
X5E5$season = c("autumn","autumn","autumn","autumn","autumn",
                "spring","spring","spring","spring","spring","spring","spring","spring","spring")
#early migration distance
X5E5$dist[1] = distm(c(X5E5$lon[1],X5E5$lat[1]),c(X5E5$lon[14],X5E5$lat[14]),fun = distHaversine)/1000
for(i in 2:nrow(X5E5)){
  X5E5$dist[i] = distm(c(X5E5$lon[i-1],X5E5$lat[i-1]),c(X5E5$lon[i],X5E5$lat[i]),fun = distHaversine)
  X5E5$dist[i] = X5E5$dist[i]/1000
}
#beeline distance
beeline = distm(c(X5E5$lon[5],X5E5$lat[5]),c(X5E5$lon[14],X5E5$lat[14]),fun = distHaversine)/1000
beeline

#total seasonal distance
sum(subset(X5E5, season == "autumn")$dist)
sum(subset(X5E5, season == "spring")$dist)

#5E7 ####
gdl <- "5E7"
load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
X5E7 = as.data.frame(matrix(,28,2))
colnames(X5E7) = c("lon","lat")
X5E7$lat = shortest_path$lat
X5E7$lon = shortest_path$lon
X5E7
X5E7$season = c("autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn","autumn",
                "spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring","spring")
#early migration distance
X5E7$dist[1] = distm(c(X5E7$lon[1],X5E7$lat[1]),c(X5E7$lon[28],X5E7$lat[28]),fun = distHaversine)/1000
for(i in 2:nrow(X5E7)){
  X5E7$dist[i] = distm(c(X5E7$lon[i-1],X5E7$lat[i-1]),c(X5E7$lon[i],X5E7$lat[i]),fun = distHaversine)
  X5E7$dist[i] = X5E7$dist[i]/1000
}
#beeline distance
beeline = distm(c(X5E7$lon[12],X5E7$lat[12]),c(X5E7$lon[28],X5E7$lat[28]),fun = distHaversine)/1000
beeline

#total seasonal distance
sum(subset(X5E7, season == "autumn")$dist)
sum(subset(X5E7, season == "spring")$dist)
