library(GeoPressureR)
library(leaflet)
library(leaflet.extras)
library(raster)
library(igraph)

# Define which track to work with
#gdl <- "5D6"

debug <- T

# Load
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))
load(paste0("data/5_wind_graph/", gdl, "_grl.Rdata"))

# Movement model
bird <- flight_bird(gpr$scientific_name)
speed <- seq(0, 80)
prob <- flight_prob(speed,
  method = "power", bird = bird, low_speed_fix = 20,
  fun_power = function(power) {
    (1 / power)^3
  }
)
par(mfrow = c(1,1))
plot(speed, prob, type = "l", xlab = "Airspeed [km/h]", ylab = "Probability")

# Convert to probability
grl$p <- grl$ps * flight_prob(grl$as, method = "power", bird = bird, low_speed_fix = 20)
str(grl$as)

# Marginal map ----
static_prob_marginal <- graph_marginal(grl)


# Shortest path ----
g <- graph_from_data_frame(data.frame(
  from = grl$s,
  to = grl$t,
  weight = -log(grl$p)
))

retrieval <- which.max(as.matrix(static_prob_marginal[[length(static_prob_marginal)]])) + grl$sz[1] * grl$sz[2] * (grl$sz[3] - 1)
stopifnot(retrieval %in% grl$retrieval)
sp <- shortest_paths(g, from = paste(grl$equipment), to = paste(retrieval))

# Convert igraph representation to lat-lon
shortest_path <- graph_path2lonlat(as.numeric(sp$vpath[[1]]$name), grl)
shortest_path_df <- as.data.frame(shortest_path)
shortest_path_timeserie <- geopressure_ts_path(shortest_path_df, pam$pressure, include_flight = c(0, 1))

# Simulation ----
nj <- 30
path_sim <- graph_simulation(grl, nj = nj)


# Rapid visual check
fun_marker_color <- function(norm){
  if (norm < 20){
    "darkpurple"
  } else if (norm < 35){
    "darkblue"
  } else if (norm < 50){
    "lightblue"
  } else if (norm < 60){
    "lightgreen"
  } else if (norm < 80){
    "yellow"
  } else if (norm < 100){
    "lightred"
  } else {
    "darkred"
  }
}
fun_NSEW <- function(angle){
  angle <- angle  %% (pi* 2)
  angle <- angle*180/pi
  if (angle < 45/2){
    "E"
  } else if (angle < 45*3/2){
    "NE"
  } else if (angle < 45*5/2){
    "N"
  } else if (angle < 45*7/2){
    "NW"
  } else if (angle < 45*9/2){
    "W"
  } else if (angle < 45*11/2){
    "SW"
  } else if (angle < 45*13/2){
    "S"
  }else if (angle < 45*15/2){
    "SE"
  } else {
    "E"
  }
}

sta_duration <- unlist(lapply(static_prob_marginal,function(x){as.numeric(difftime(metadata(x)$temporal_extent[2],metadata(x)$temporal_extent[1],units="days"))}))

m <-leaflet(width = "100%") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%  addFullscreenControl() %>%
  addPolylines(lng = shortest_path$lon, lat = shortest_path$lat, opacity = 1, color = "#808080", weight = 3) %>%
  addCircles(lng = shortest_path$lon, lat = shortest_path$lat, opacity = 1, color = "#000", weight = sta_duration^(0.3)*10)

for (i_s in seq_len(grl$sz[3]-1)){
  if (grl$flight_duration[i_s]>5){
    edge <- which(grl$s == shortest_path$id[i_s] & grl$t == shortest_path$id[i_s+1])

    label = paste0( i_s,': ', grl$flight[[i_s]]$start, " - ", grl$flight[[i_s]]$end, "<br>",
                    "F. dur.: ", round(grl$flight_duration[i_s]), ' h <br>',
                    "GS: ", round(abs(grl$gs[edge])), ' km/h, ',fun_NSEW(Arg(grl$gs[edge])),'<br>',
                    "WS: ", round(abs(grl$ws[edge])), ' km/h, ',fun_NSEW(Arg(grl$ws[edge])),'<br>',
                    "AS: ", round(abs(grl$as[edge])), ' km/h, ',fun_NSEW(Arg(grl$as[edge])),'<br>')

    iconArrow <- makeAwesomeIcon(icon = "arrow-up",
                                 library = "fa",
                                 iconColor = "#FFF",
                                 iconRotate = (90 - Arg(grl$ws[edge])/pi*180) %% 360,
                                 squareMarker = TRUE,
                                 markerColor = fun_marker_color(abs(grl$ws[edge])))

    m <- m %>% addAwesomeMarkers(lng = (shortest_path$lon[i_s] + shortest_path$lon[i_s+1])/2,
                                 lat = (shortest_path$lat[i_s] + shortest_path$lat[i_s+1])/2,
                                 icon = iconArrow, popup = label)
  }
}
m
#save image
saveWidget(m, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "5D7_wind.png",
        cliprect = "viewport")

#speed plot
edge <- t(graph_path2edge(path_sim$id, grl))
nj <- ncol(edge)
nsta <- ncol(path_sim$lon)

speed_df <- data.frame(
  as = abs(grl$as[edge]),
  gs = abs(grl$gs[edge]),
  ws = abs(grl$ws[edge]),
  sta_id_s = rep(head(grl$sta_id,-1), nj),
  sta_id_t = rep(tail(grl$sta_id,-1), nj),
  flight_duration = rep(head(grl$flight_duration,-1), nj),
  dist = geosphere::distGeo(
    cbind(as.vector(t(path_sim$lon[,1:nsta-1])), as.vector(t(path_sim$lat[,1:nsta-1]))),
    cbind(as.vector(t(path_sim$lon[,2:nsta])),   as.vector(t(path_sim$lat[,2:nsta])))
  ) / 1000
) %>% mutate(
  name = paste(sta_id_s,sta_id_t, sep="-")
)

plot1 <- ggplot(speed_df, aes(reorder(name, sta_id_s), gs)) + geom_boxplot() + theme_bw() +scale_x_discrete(name = "")
plot2 <- ggplot(speed_df, aes(reorder(name, sta_id_s), ws)) + geom_boxplot() + theme_bw() +scale_x_discrete(name = "")
plot3 <- ggplot(speed_df, aes(reorder(name, sta_id_s), as)) + geom_boxplot() + theme_bw() +scale_x_discrete(name = "")
plot4 <- ggplot(speed_df, aes(reorder(name, sta_id_s), flight_duration)) + geom_point() + theme_bw() +scale_x_discrete(name = "")

subplot(ggplotly(plot1), ggplotly(plot2), ggplotly(plot3), ggplotly(plot4), nrows=4, titleY=T)


if (debug) {

  # In depth analysis with GeoPressureViz
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  #load(paste0("data/2_light/", gdl, "_light_prob.Rdata"))
  load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

  sta_marginal <- unlist(lapply(static_prob_marginal, function(x) raster::metadata(x)$sta_id))
  sta_pres <- unlist(lapply(pressure_prob, function(x) raster::metadata(x)$sta_id))
  #sta_light <- unlist(lapply(light_prob, function(x) raster::metadata(x)$sta_id))
  pressure_prob <- pressure_prob[sta_pres %in% sta_marginal]
  #light_prob <- light_prob[sta_light %in% sta_marginal]


  geopressureviz <- list(
    pam = pam,
    static_prob = static_prob,
    static_prob_marginal = static_prob_marginal,
    pressure_prob = pressure_prob,
    #light_prob = light_prob,
    pressure_timeserie = shortest_path_timeserie
  )
  save(geopressureviz, file = "~/geopressureviz.RData")

  shiny::runApp(system.file("geopressureviz", package = "GeoPressureR"),
    launch.browser = getOption("browser")
  )
}


# Save ----
save(
  path_sim,
  shortest_path,
  static_prob_marginal,
  shortest_path_timeserie,
  file = paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata")
)
