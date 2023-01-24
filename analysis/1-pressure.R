library(GeoPressureR)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(raster)
library(readxl)

#install.packages("distill")
# Set debug T to see all check and set to F once everything is correct
debug <- T

# Define the geolocator data logger id to use
gdl <- "5D6"

# Read its information from gpr_settings.xlsx
gpr <- read_excel("data/gpr_settings.xlsx") %>%
  filter(gdl_id == gdl)

raw <- read.csv(paste0("data/0_PAM/", "/", gdl, "_act_pres_temp.csv"))

# change timestamp back to correct format
raw$timestamp = as.POSIXct(raw$timestamp,tz = "GMT", format = c("%Y-%m-%d %H:%M:%S")) #capital sensitive!
if (debug){
  head(raw)
  #CHECK whether it is GMT!
  unique(raw$timestamp)
  summary(raw)
  str(raw)
}

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
pam$id <- as.character("5D6")
colnames(pam$pressure) <- c("date","obs")  #change column name for each list
colnames(pam$acceleration) <- c("date","obs")
colnames(pam$light) <- c("date","obs")
colnames(pam$temperature) <- c("date","obs")

if (debug){
  summary(pam)
  str(pam)
}

# Read the label and compute the stationary info
trainset_write(pam,pathname = "data/1_pressure/labels/")  #auto-generate a .csv file
pam <- trainset_read(pam, "data/1_pressure/labels/")
pam <- pam_sta(pam)

# define the discrete colorscale. Used at multiple places.
col <- rep(RColorBrewer::brewer.pal(9, "Set1"), times = ceiling((nrow(pam$sta) + 1) / 9))
col <- col[1:(nrow(pam$sta) + 1)]
names(col) <- levels(factor(c(0, pam$sta$sta_id)))


if (debug) {
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
}


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


if (debug) {
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
}

# Save ----
save(
  pressure_timeserie, # can be removed in not in debug mode
  pressure_prob,
  pam,
  gpr,
  file = paste0("data/1_pressure/", gpr$gdl_id, "_pressure_prob.Rdata")
)


