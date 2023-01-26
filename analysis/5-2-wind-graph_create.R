library(GeoPressureR)

# Define which track to work with
gdl <- "5D6"

# Load
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

# Create graph
grl <- graph_create(static_prob,
  thr_prob_percentile = gpr$thr_prob_percentile,
  thr_gs = gpr$thr_gs # threshold km/h
)

# Fix missing pressure, see https://github.com/Rafnuss/GeoPressureR/discussions/62

# Get index of all flights
id_flight <- lapply(head(grl$flight, -1), function(x) {
  pam$pressure$date >= x$start &  pam$pressure$date <= x$end
})

# Median pressure of all timestep in flight
# pres_med <- median(pam$pressure$obs[Reduce("|", id_flight )])
# Median pressure of all flight's mean pressure
pres <- median(unlist(lapply(id_flight, function(x){ mean(pam$pressure$obs[x])})), na.rm =TRUE)


plot(pam$pressure$date[Reduce("|", id_flight )], pam$pressure$obs[Reduce("|", id_flight )])
abline(h=pres_med, col="red")


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

# Run the function with `pressure_modif` instead of `pam$pressure`
filename <- paste0("data/5_wind_graph/", gdl, "/", gdl, "_")
grl <- graph_add_wind(grl,
                      pressure = pressure_modif,
                      filename,
                      thr_as = gpr$thr_as
)



save(grl,
  file = paste0("data/5_wind_graph/", gdl, "_grl.Rdata")
)
