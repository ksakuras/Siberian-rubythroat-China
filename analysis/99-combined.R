# 1-pressure####
# Set debug T to see all check and set to F once everything is correct
debug <- T

# Define the geolocator data logger id to use
gdl <- "5D6"

# Read its information from gpr_settings.xlsx
gpr <- read_excel("data/gpr_settings.xlsx") %>%
  filter(gdl_id == gdl)

#for CAnMove logger
raw <- read.csv(paste0("data/0_PAM/", "/", gdl, "_act_pres_temp.csv"))

# change timestamp back to correct format
raw$timestamp = as.POSIXct(raw$timestamp,tz = "GMT", format = c("%Y-%m-%d %H:%M:%S")) #capital sensitive!
#for 56C only
#raw$timestamp = as.POSIXct(raw$timestamp,tz = "GMT", format = c("%Y-%m-%d %H:%M")) #capital sensitive!

#if (debug){
head(raw)
#CHECK whether it is GMT!
#unique(raw$timestamp)
#summary(raw)
#str(raw)
#}

# extract info
pressure.mid = subset(raw, series == "pressure")
acto.mid = subset(raw, series == "actoscore")
temp.mid = subset(raw, series == "temperature")

# combine all information into pam variable
pam <- list(length = 4)  #create a list variable
pam$pressure <- data.frame(pressure.mid[,c(2,3)])  #copy the column 2-3 to the list
pam$acceleration <- data.frame(acto.mid[,c(2,3)])
pam$temperature <- data.frame(temp.mid[,c(2,3)])
pam$light <- data.frame(acto.mid[,c(2,3)])   #create a list of light with acto schedule
pam$light[,2] <- rep(0,length(pam$light[,2])) #force all data to 0 as we don't have light data
names(pam) <- c("id","pressure","acceleration","temperature","light") #change "length" list to name "id"
pam$id <- as.character(gdl)
colnames(pam$pressure) <- c("date","obs")  #change column name for each list
colnames(pam$acceleration) <- c("date","obs")
colnames(pam$light) <- c("date","obs")
colnames(pam$temperature) <- c("date","obs")

#if (debug){
summary(pam)
str(pam)
#}

# Read the label and compute the stationary info
trainset_write(pam,pathname = "data/1_pressure/labels/")  #auto-generate a .csv file
pam <- trainset_read(pam, "data/1_pressure/labels/")
pam <- pam_sta(pam)

# define the discrete colorscale. Used at multiple places.
col <- rep(RColorBrewer::brewer.pal(9, "Set1"), times = ceiling((nrow(pam$sta) + 1) / 9))
col <- col[1:(nrow(pam$sta) + 1)]
names(col) <- levels(factor(c(0, pam$sta$sta_id)))


#if (debug) {
# Test 1 ----
pam$sta %>%
  mutate(
    duration = difftime(end, start, units = "hours"),
    next_flight_duration = difftime(lead(start), end, units = "hours")
  ) %>%
  filter(duration < 3) %>%
  arrange(duration)


# Test 2 ----
pressure_na <- pam$pressure %>%
  mutate(obs = ifelse(isoutlier | sta_id == 0, NA, obs))

p <- ggplot() +
  geom_line(data = pam$pressure, aes(x = date, y = obs), col = "grey") +
  geom_line(data = pressure_na, aes(x = date, y = obs, color = factor(sta_id))) +
  # geom_point(data = subset(pam$pressure, isoutlier), aes(x = date, y = obs), colour = "black") +
  theme_bw() +
  scale_color_manual(values = col) +
  scale_y_continuous(name = "Pressure (hPa)")

ggplotly(p, dynamicTicks = T) %>% layout(showlegend = F)
#}


# Filter stationary period based on the number of pressure datapoint available
thr_dur <- gpr$thr_dur # 24*4 # duration in hour. Decrease this value down to gpr$thr_dur
res <- as.numeric(difftime(pam$pressure$date[2], pam$pressure$date[1], units = "hours"))

sta_id_keep <- pam$pressure %>%
  filter(!isoutlier & sta_id > 0) %>%
  count(sta_id) %>%
  filter(n * res > thr_dur) %>%
  .$sta_id

# Duplicate the pam data to avoid issue after filtering, and put NA on the sta to not consider
pam_short <- pam
pam_short$pressure <- pam_short$pressure %>%
  mutate(sta_id = ifelse(sta_id %in% sta_id_keep, sta_id, NA))

head(pam_short$sta)
# Query pressure map
# We overwrite the setting parameter for resolution to make query faster at first
pressure_maps <- geopressure_map(pam_short$pressure,
                                 extent = c(gpr$extent_N, gpr$extent_W, gpr$extent_S, gpr$extent_E),
                                 scale = gpr$map_scale,
                                 max_sample = gpr$map_max_sample,
                                 margin = gpr$map_margin
)

# Convert to probability map
pressure_prob <- geopressure_prob_map(pressure_maps,
                                      s = gpr$prob_map_s,
                                      thr = gpr$prob_map_thr
)


#if (debug) {
# Compute the path of the most likely position
path <- geopressure_map2path(pressure_prob)

# Query timeserie of pressure based on these path
pressure_timeserie <- geopressure_ts_path(path, pam_short$pressure, include_flight = c(0, 1))

# Test 3 ----
p <- ggplot() +
  geom_line(data = pam$pressure, aes(x = date, y = obs), colour = "grey") +
  geom_point(data = subset(pam$pressure, isoutlier), aes(x = date, y = obs), colour = "black") +
  geom_line(data = pressure_na, aes(x = date, y = obs, color = factor(sta_id)), size = 0.5) +
  geom_line(data = do.call("rbind", pressure_timeserie) %>% filter(!is.na(sta_id)), aes(x = date, y = pressure0, col = factor(sta_id)), linetype = 2) +
  theme_bw() +
  scale_colour_manual(values = col) +
  scale_y_continuous(name = "Pressure(hPa)")

ggplotly(p, dynamicTicks = T) %>% layout(showlegend = F)


# Test 4 ----
par(mfrow = c(5, 6), mar = c(1, 1, 3, 1))
for (i_r in seq_len(length(pressure_timeserie))) {
  if (!is.null(pressure_timeserie[[i_r]])) {
    i_s <- unique(pressure_timeserie[[i_r]]$sta_id)
    df3 <- merge(pressure_timeserie[[i_r]], subset(pam$pressure, !isoutlier & sta_id == i_s), by = "date")
    df3$error <- df3$pressure0 - df3$obs.x
    hist(df3$error, main = i_s, xlab = "", ylab = "")
    abline(v = 0, col = "red")
  }
}

# Map the most likely position
sta_duration <- unlist(lapply(pressure_prob, function(x) {
  as.numeric(difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days"))
}))
pal <- colorFactor(col, as.factor(seq_len(length(col))))
leaflet() %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  addFullscreenControl() %>%
  addPolylines(lng = path$lon, lat = path$lat, opacity = 0.7, weight = 1, color = "#808080") %>%
  addCircles(lng = path$lon, lat = path$lat, opacity = 1, color = pal(factor(path$sta_id, levels = pam$sta$sta_id)), weight = sta_duration^(0.3) * 10)
#}

# Save ----
save(
  pressure_timeserie, # can be removed in not in debug mode
  pressure_prob,
  pam,
  gpr,
  file = paste0("data/1_pressure/", gpr$gdl_id, "_pressure_prob.Rdata")
)

# 3-static ####
#install.packages("shinyjs")
#install.packages("shinyWidgets")

debug <- T
# Define the geolocator data logger id to use
#gdl <- "5D7"

# Load the pressure file, also contains set, pam, col
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))

# Defint the threashold of the stationary period to consider
thr_sta_dur <- gpr$thr_dur # in hours

sta_pres <- unlist(lapply(pressure_prob, function(x) raster::metadata(x)$sta_id))
sta_thres <- pam$sta$sta_id[difftime(pam$sta$end, pam$sta$start, units = "hours") > thr_sta_dur]

# Get the sta_id present on all three data sources
sta_id_keep <- intersect(sta_pres, sta_thres)

# Filter pressure map
pressure_prob <- pressure_prob[sta_pres %in% sta_id_keep]


# Flight
flight <- list()
for (i_f in seq_len(length(sta_id_keep) - 1)) {
  from_sta_id <- sta_id_keep[i_f]
  to_sta_id <- sta_id_keep[i_f + 1]
  flight[[i_f]] <- list(
    start = pam$sta$end[seq(from_sta_id, to_sta_id - 1)],
    end = pam$sta$start[seq(from_sta_id + 1, to_sta_id)],
    sta_id = seq(from_sta_id, to_sta_id - 1)
  )
}
flight[[i_f + 1]] <- list()


# static prob
static_prob <- mapply(function(pressure, flight) {
  # define static prob as the product of light and pressure prob
  static_prob <- pressure

  # replace na by zero
  # tmp <- values(static_prob)
  # tmp[is.na(tmp)] <- 0
  # values(static_prob) <- tmp

  # define metadata
  metadata(static_prob) <- metadata(pressure)
  metadata(static_prob)$flight <- flight

  # return
  static_prob
}, pressure_prob, flight)




# Overwrite prob at calibration
# Get lat lon
lat <- seq(raster::ymax(static_prob[[1]]), raster::ymin(static_prob[[1]]), length.out = nrow(static_prob[[1]]) + 1)
lat <- lat[seq_len(length(lat) - 1)] + diff(lat[1:2]) / 2
lon <- seq(raster::xmin(static_prob[[1]]), raster::xmax(static_prob[[1]]), length.out = ncol(static_prob[[1]]) + 1)
lon <- lon[seq_len(length(lon) - 1)] + diff(lon[1:2]) / 2


if (!is.na(gpr$calib_1_start)) {
  lon_calib_id <- which.min(abs(gpr$calib_lon - lon))
  lat_calib_id <- which.min(abs(gpr$calib_lat - lat))

  stopifnot(metadata(static_prob[[1]])$sta_id == 1)
  tmp <- as.matrix(static_prob[[1]])
  tmp[!is.na(tmp)] <- 0
  tmp[lat_calib_id, lon_calib_id] <- 1
  values(static_prob[[1]]) <- tmp
}

if (!is.na(gpr$calib_2_start)) {
  if (!is.na(gpr$calib_2_lat)) {
    lon_calib_id <- which.min(abs(gpr$calib_2_lon - lon))
    lat_calib_id <- which.min(abs(gpr$calib_2_lat - lat))
  }
  tmp <- as.matrix(static_prob[[length(static_prob)]])
  tmp[!is.na(tmp)] <- 0
  tmp[lat_calib_id, lon_calib_id] <- 1
  values(static_prob[[length(static_prob)]]) <- tmp
}



# Get pressure timeserie at the best match of static
path <- geopressure_map2path(static_prob)
static_timeserie <- geopressure_ts_path(path, pam$pressure)


#if (debug) {
# GeopressureViz
geopressureviz(
  pam = pam,
  static_prob = static_prob,
  pressure_prob = pressure_prob,
  pressure_timeserie = static_timeserie
)

# Check 1
static_prob_n <- lapply(static_prob, function(x) {
  probt <- raster::as.matrix(x)
  probt[is.na(probt)] <- 0
  probt / sum(probt, na.rm = T)
})
tmp <- unlist(lapply(static_prob_n, sum)) == 0
if (any(tmp)) {
  warning(paste0(
    "The `static_prob` provided has a probability map equal to ",
    "zero for the stationay period: ", which(tmp)
  ))
}


## Check 2
for (i_s in seq_len(length(static_prob) - 1)) {
  cur <- as.matrix(static_prob[[i_s]]) > 0
  cur[is.na(cur)] <- F
  nex <- as.matrix(static_prob[[i_s + 1]]) > 0
  nex[is.na(nex)] <- F

  mtf <- metadata(static_prob[[i_s]])
  flight_duration <- as.numeric(sum(difftime(mtf$flight$end, mtf$flight$start, unit = "hours"))) # hours
  resolution <- mean(res(static_prob[[1]])) * 111 # assuming 1°= 111km
  thr_gs <- # Assuming a max groundspeed of 150km/h
    # Accepting a minimium of 3 grid resolution for noise/uncertainty.
    flight_duration <- pmax(flight_duration, resolution * 3 / gpr$thr_gs)

  # Check possible position at next stationary period
  possible_next <- (EBImage::distmap(!cur) * resolution / flight_duration) < gpr$thr_gs

  if (sum(possible_next & nex) == 0) {
    stop(paste("There are no possible transition from stationary period", i_s, "to", i_s + 1, ". Check part 1 process (light and pressure)", sep = " "))
  }
}
#}

## Save ----
save(
  static_prob,
  static_timeserie,
  file = paste0("data/3_static/", gpr$gdl_id, "_static_prob.Rdata")
)


# 4-basic ####

#debug <- T
# Define which track to work with
#gdl <- "5D6"

# Load static prob
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

# Build the graph ---- #having problem at this line:

grl <- graph_create(static_prob,
                    thr_prob_percentile = gpr$thr_prob_percentile,
                    thr_gs = gpr$thr_gs # threshold km/h
)
# If you get an error with trimming, use geopressureviz from end of 3.static.R

#visualize the probability of speed ####
speed <- seq(1, 120)
low_speed_fix <- 20 # minimum speed allowed
prob <- flight_prob(speed, method = "gamma", shape = 7, scale = 7, low_speed_fix = low_speed_fix)
par(mfrow = c(1,1))
plot(speed, prob, type = "l", xlab = "Groundspeed [km/h]", ylab = "Probability")
abline(v = low_speed_fix)

# Add probability of each edge ####
grl$p <- grl$ps * flight_prob(grl$gs, method = "gamma", shape = 7, scale = 7, low_speed_fix = gpr$low_speed_fix)


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
nj <- 14
path_sim <- graph_simulation(grl, nj = nj)

head(shortest_path$lat)
head(shortest_path$lon)
scatter.smooth(shortest_path$lat~shortest_path$lon)

# visualize the simulation ####
col <- rep(RColorBrewer::brewer.pal(9, "Set1"), times = ceiling(grl$sz[3] / 9))
m <- leaflet(width = "100%") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  addFullscreenControl()
for (i in seq_len(nj)) {
  m <- m %>%
    addPolylines(lng = path_sim$lon[i, ], lat = path_sim$lat[i, ], opacity = 0.7, weight = 1, color = "#808080")
}
for (i in seq_len(grl$sz[3])) {
  m <- m %>%
    addCircles(lng = path_sim$lon[, i], lat = path_sim$lat[, i], opacity = .4, weight = 10, color = col[i])
}
m <- m %>% addLegend(position="bottomright", colors = col[1:grl$sz[3]], labels = seq_len(grl$sz[3]), title = "stationary period", opacity = 1 )
m

# ####
#if (debug) {

# Rapid visual check
sta_duration <- unlist(lapply(static_prob_marginal, function(x) {
  as.numeric(difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days"))
}))

m <- leaflet(width = "100%") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  addFullscreenControl()
for (i in seq_len(nj)) {
  m <- m %>%
    addPolylines(lng = path_sim$lon[i, ], lat = path_sim$lat[i, ], opacity = 0.7, weight = 1, color = "#808080") %>%
    addCircles(lng = path_sim$lon[i, ], lat = path_sim$lat[i, ], opacity = 1, weight = 1, color = "#000")
}

m <- m %>%
  addPolylines(lng = shortest_path$lon, lat = shortest_path$lat, opacity = 1, color = "#808080", weight = 3) %>%
  addCircles(lng = shortest_path$lon, lat = shortest_path$lat, opacity = 1, color = "#125678", weight = sta_duration^(0.3) * 8)
m
#save image
#saveWidget(m, "temp.html", selfcontained = FALSE)
#webshot("temp.html", file = "5D7_basic.png",
#        cliprect = "viewport")

# Light comparison
#load(paste0("data/2_light/", gdl, "_light_prob.Rdata"))
#raw_geolight <- pam$light %>%
#  transmute(
#    Date = date,
#    Light = obs
#  )
#lightImage(
#  tagdata = raw_geolight,
#  offset = gpr$shift_k / 60 / 60
#)
#tsimagePoints(twl$twilight,
#  offset = -gpr$shift_k / 60 / 60, pch = 16, cex = 1.2,
#  col = ifelse(twl$deleted, "grey20", ifelse(twl$rise, "firebrick", "cornflowerblue"))
#)
#for (ts in shortest_path_timeserie) {
#  twl_fl <- twl %>%
#    filter(twilight > ts$date[1] & twilight < tail(ts$date, 1))
#  tsimageDeploymentLines(twl_fl$twilight,
#    lon = ts$lon[1], ts$lat[1],
#    offset = gpr$shift_k / 60 / 60, lwd = 3, col = adjustcolor("orange", alpha.f = 0.5)
#  )
#}

# In depth analysis with GeoPressureViz
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
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
#}

# Save
save( # grl, we are excluding grl because of its size on this repo. Feel free to keep it in your own project
  path_sim,
  shortest_path,
  static_prob_marginal,
  shortest_path_timeserie,
  file = paste0("data/4_basic_graph/", gpr$gdl_id, "_basic_graph.Rdata")
)

#Marginal probability map
load(paste0("data/4_basic_graph/", gpr$gdl_id, "_basic_graph.Rdata"))
sta_duration <- unlist(lapply(static_prob_marginal, function(x) {
  as.numeric(difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days"))
}))
li_s <- list()
l <- leaflet(width = "100%") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  addFullscreenControl()
for (i_r in seq_len(length(static_prob_marginal))) {
  i_s <- metadata(static_prob_marginal[[i_r]])$sta_id
  info <- metadata(static_prob_marginal[[i_r]])$temporal_extent
  info_str <- paste0(i_s, " | ", info[1], "->", info[2])
  li_s <- append(li_s, info_str)
  l <- l %>%
    addRasterImage(static_prob_marginal[[i_r]], colors = "OrRd", opacity = 0.8, group = info_str)
}
l %>%
  addPolylines(lng = shortest_path$lon, lat = shortest_path$lat, opacity = .5, color = "#808080", weight = 3) %>%
  addCircles(lng = shortest_path$lon, lat = shortest_path$lat, opacity = .4, weight = sta_duration^(0.3) * 10, popup = paste0("sta_id=", shortest_path$sta_id), color = "#000") %>%
  addLayersControl(
    overlayGroups = li_s,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(tail(li_s, length(li_s) - 1))



# 5-1 download wind graph ####
library(ecmwfr)

# Define which track to work with
#gdl <- "5D6"

# Load
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

# Set credential
Sys.setenv( cds_key="cb832111-2dba-4312-801c-cf1c3ca65371")
Sys.setenv( cds_user="174688")

# You can see them with
#usethis::edit_r_environ()
cds_key <- Sys.getenv("cds_key")
cds_user <- Sys.getenv("cds_user")

graph_download_wind(pam,
                    area = static_prob,
                    # cds_key="Insert_your_CDS_API_KEY_here"
                    # cds_user="Insert_your_CDS_UID_here"
)

# Check request at https://cds.climate.copernicus.eu/cdsapp#!/yourrequests



# 5-2 create wind graph ####

# Load
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

# Create graph
grl <- graph_create(static_prob,
                    thr_prob_percentile = gpr$thr_prob_percentile,
                    thr_gs = gpr$thr_gs # threshold km/h
)

# Add wind
filename <- paste0("data/5_wind_graph/", gdl, "/", gdl, "_")

# Define the pressure of flight (see below for method to do that)
pres <- 800

# Make a copy of your pressure data that you will use for graph_add_wind
pressure_modif <- pam$pressure

# loop through each flight of each group of flight
for (i_s in seq_len(length(grl$flight))){
  for (i_ss in seq_len(length(grl$flight[[i_s]]$sta_id))){
    # find the index in pam$pressure corredponding to this flight
    id_flight <- pam$pressure$date >= grl$flight[[i_s]]$start[i_ss] &  pam$pressure$date <= grl$flight[[i_s]]$end[i_ss]
    if (sum(id_flight)==0){ # meaning, if there are no pressure data for this flight
      # define the time on which to add the data
      date <- seq(round.POSIXt(grl$flight[[i_s]]$start[i_ss] - 30 * 60, units = "hours"),
                  round.POSIXt(grl$flight[[i_s]]$end[i_ss] + 30 * 60, units = "hours"),
                  by = 60 * 60
      )
      # merge new fake pressure data with pressure data.frame
      pressure_modif = rbind(
        pressure_modif,
        data.frame(
          date = date,
          obs = rep(pres,length(date)),
          isoutlier=rep(TRUE,length(date)),
          sta_id = rep(grl$flight[[i_s]]$sta_id[i_ss],length(date))
        )
      )
    }
  }
}

# Sort by date
pressure_modif <- pressure_modif[order(pressure_modif$date),]

# Run the function with `pressure_modif` isntead of `pam$pressure`
grl <- graph_add_wind(grl,
                      pressure = pressure_modif,
                      filename,
                      thr_as = gpr$thr_as
)


#only for 56C
#grl <- graph_add_wind(grl,
#  pressure = pam$pressure, filename,
#  thr_as = gpr$thr_as
#)

save(grl,
     file = paste0("data/5_wind_graph/", gdl, "_grl.Rdata")
)

#extra unknown functions
# Get index of all flights
id_flight <- lapply(head(grl$flight, -1), function(x) {
  pam$pressure$date >= x$start &  pam$pressure$date <= x$end
})

# Median pressure of all timestep in flight
pres <- median(pam$pressure$obs[Reduce("|", id_flight )])
# Median pressure of all flight's mean pressure
pres <- median(unlist(lapply(id_flight, function(x){ mean(pam$pressure$obs[x])})), na.rm =TRUE)

# View on a plot
plot(pam$pressure$date[Reduce("|", id_flight )], pam$pressure$obs[Reduce("|", id_flight )])
abline(h=pres, col="red")


# 5-3 wind analysis ####

# Define which track to work with
#gdl <- "5D6"

#debug <- T

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

load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
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
#saveWidget(m, "temp.html", selfcontained = FALSE)
#webshot("temp.html", file = "5D7_wind.png",
#        cliprect = "viewport")

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
