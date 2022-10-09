
raw.5D7 <- read.csv("data/0_PAM/5D7_act_pres_temp.csv")

# change timestamp back to correct format
raw.5D7$timestamp = as.POSIXct(raw.5D7$timestamp,tz = "GMT", format = c("%Y-%m-%d %H:%M:%S"))
head(raw.5D7)
#CHECK whether it is GMT!
unique(raw.5D7$timestamp)
summary(raw.5D7)
str(raw.5D7)

# extract info
pressure.5D7.mid = subset(raw.5D7, series == "pressure")
acto.5D7.mid = subset(raw.5D7, series == "actoscore")
temp.5D7.mid = subset(raw.5D7, series == "temperature")


# combine all information into pam variable
pam.5D7 <- list(length = 4)  #create a list variable
pam.5D7$pressure <- data.frame(pressure.5D7.mid[,c(2,3)])  #copy the column 2-3 to the list
pam.5D7$acceleration <- data.frame(acto.5D7.mid[,c(2,3)])
pam.5D7$temperature <- data.frame(temp.5D7.mid[,c(2,3)])
pam.5D7$light <- data.frame(acto.5D7.mid[,c(2,3)])   #create a list of light with acto schedule
pam.5D7$light[,2] <- rep(0,length(pam.5D7$light[,2])) #force all data to 0 as we don't have light data
names(pam.5D7) <- c("id","pressure","acceleration","temperature","light") #change "length" list to name "id"
pam.5D7$id <- as.character("5D7")
colnames(pam.5D7$pressure) <- c("date","obs")  #change column name for each list
colnames(pam.5D7$acceleration) <- c("date","obs")
colnames(pam.5D7$light) <- c("date","obs")
colnames(pam.5D7$temperature) <- c("date","obs")

summary(pam.5D7)
str(pam.5D7)

#now the format is compatible, we can follow the manual!
trainset_write(pam.5D7,pathname = "data/1_pressure/labels/")  #auto-generate a .csv file
pam.5D7 <- trainset_read(pam.5D7, pathname = "data/1_pressure/labels/") #auto-find the -labeled.csv file
pam.5D7 <- pam_sta(pam.5D7)
knitr::kable(head(pam.5D7$sta))
str(pam.5D7)

pressure_na <- pam.5D7$pressure
pressure_na$obs[pressure_na$isoutlier | pressure_na$sta_id == 0] <- NA
p <- ggplot() +
  geom_line(data = pam.5D7$pressure, aes(x = date, y = obs), col = "grey") +
  geom_line(data = pressure_na, aes(x = date, y = obs, col = as.factor(sta_id))) +
  geom_point(data = subset(pam.5D7$pressure, isoutlier), aes(x = date, y = obs), colour = "black") +
  theme_bw() +
  scale_y_continuous(name = "Pressure (hPa)") +
  scale_colour_manual(values = rep(RColorBrewer::brewer.pal(9, "Set1"), times = 8))

ggplotly(p, dynamicTicks = T) %>%
  layout(
    showlegend = F,
    legend = list(orientation = "h", x = -0.5),
    yaxis = list(title = "Pressure [hPa]")
  )

pressure_maps <- geopressure_map(
  pam.5D7$pressure,
  extent = c(50, 60, 0, 120), # coordinates of the map to request (N, W, S, E)
  scale = 2, # request on a 1/2=0.5° grid to make the code faster
  max_sample = 250, # limit the query to the first 250 data-points.
  margin = 30 # roughly equivalent to 3hPa
)

pressure_prob <- geopressure_prob_map(
  pressure_maps,
  s = 1, # standard deviation of pressure
  thr = 0.9 # threshold of the threshold proportion value acceptable
)

i_r <- 2
leaflet(width = "100%") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  addFullscreenControl() %>%
  addRasterImage(pressure_prob[[i_r]], opacity = 0.8, colors = "OrRd", group = "Probability") %>%
  addRasterImage(pressure_maps[[i_r]][[1]], opacity = 0.8, colors = "OrRd", group = "Mismatch") %>%
  addRasterImage(pressure_maps[[i_r]][[2]], opacity = 0.8, colors = "OrRd", group = "Threashold") %>%
  # addLegend(pal = pal, values = values(v[[i_s]][[3]]), title = "Probability") %>%
  addLayersControl(
    overlayGroups = c("Probability", "Mismatch", "Threashold"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Mismatch", "Threashold"))

li_s <- list()
l <- leaflet(width = "100%") %>%
  addProviderTiles(providers$Stamen.TerrainBackground) %>%
  addFullscreenControl()
for (i_r in seq_len(length(pressure_prob))) {
  i_s <- metadata(pressure_prob[[i_r]])$sta_id
  info <- pam.5D7$sta[pam.5D7$sta$sta_id == i_s, ]
  info_str <- paste0(i_s, " | ", format(info$start, "%d-%b %H:%M"), "->", format(info$end, "%d-%b %H:%M"))
  li_s <- append(li_s, info_str)
  l <- l %>% addRasterImage(pressure_prob[[i_r]], opacity = 0.8, colors = "OrRd", group = info_str)
}
l %>%
  addLayersControl(
    overlayGroups = li_s,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(tail(li_s, length(li_s) - 1))


pt <- geopressure_map2path(pressure_prob[1])
pressure_timeserie_1 <- geopressure_ts(pt$lon, pt$lat, pressure = subset(pam.5D7$pressure, sta_id == 1))

Lb <- -0.0065
R <- 8.31432
g0 <- 9.80665
M <- 0.0289644
T0 <- 273.15 + 15
P0 <- 1013.25
pressure_timeserie_1$altitude_baro <- T0 / Lb * ((pressure_timeserie_1$pressure / P0)^(-R * Lb / g0 / M) - 1)

p <- ggplot() +
  geom_line(data = as.data.frame(pressure_timeserie_1), aes(x = date, y = altitude, col = as.factor("Corrected elevation with ERA5"))) +
  geom_line(data = as.data.frame(pressure_timeserie_1), aes(x = date, y = altitude_baro, col = as.factor("Uncorrected elevation"))) +
  labs(col = "") +
  theme_bw()

ggplotly(p) %>%
  layout(legend = list(orientation = "h", x = -0.5))

View(pressure_timeserie_1)

path <- geopressure_map2path(pressure_prob)
p <- ggplot() +
  geom_line(data = do.call("rbind", pressure_timeserie), aes(x = date, y = altitude)) +
  theme_bw() +
  scale_y_continuous(name = "Altitude (m)")
ggplotly(p, dynamicTicks = T) %>% layout(showlegend = F)

col <- rep(RColorBrewer::brewer.pal(9, "Set1"), times = ceiling((nrow(pam.5D7$sta) + 1) / 9))
col <- col[1:(nrow(pam.5D7$sta) + 1)]
names(col) <- levels(factor(c(0, pam.5D7$sta$sta_id)))

#Q: Where did this pressure_timeserie come from? It combined my dataset with the example set

p <- ggplot() +
  geom_line(data = pam.5D7$pressure, aes(x = date, y = obs), colour = "grey") +
  geom_point(data = subset(pam.5D7$pressure, isoutlier), aes(x = date, y = obs), colour = "black") +
  # geom_line(data = pressure_na, aes(x = date, y = obs, color = factor(sta_id))) +
  geom_line(data = subset(do.call("rbind", pressure_timeserie), sta_id != 0), aes(x = date, y = pressure0, col = factor(sta_id))) +
  theme_bw() +
  scale_colour_manual(values = col) +
  scale_y_continuous(name = "Pressure (hPa)")

ggplotly(p, dynamicTicks = T) %>% layout(showlegend = F)

save(
  pressure_timeserie,
  pressure_prob,
  pam,
  file = "data/1_pressure/5D7_pressure_prob.Rdata"
)
