
# Set debug T to see all check and set to F once everything is correct
debug <- F

# Define the geolocator data logger id to use
# gdl <- "5D6"

raw <- read.csv(paste0("data/0_PAM/", gdl, "/", gdl, "_act_pres_temp.csv"))

# change timestamp back to correct format
raw$timestamp = as.POSIXct(raw$timestamp,tz = "GMT", format = c("%Y-%m-%d %H:%M:%S"))
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
