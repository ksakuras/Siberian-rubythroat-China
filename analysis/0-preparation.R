
raw.5D6 <- read.csv("data/0_PAM/5D6_act_pres_temp.csv")

# change timestamp back to correct format
raw.5D6$timestamp = as.POSIXct(raw.5D6$timestamp,tz = "GMT", format = c("%Y-%m-%d %H:%M:%S"))
head(raw.5D6)
#CHECK whether it is GMT!
unique(raw.5D6$timestamp)
summary(raw.5D6)
str(raw.5D6)

# extract info
pressure.5D6.mid = subset(raw.5D6, series == "pressure")
acto.5D6.mid = subset(raw.5D6, series == "actoscore")
temp.5D6.mid = subset(raw.5D6, series == "temperature")


# combine all information into pam variable
pam.5D6 <- list(length = 4)  #create a list variable
pam.5D6$pressure <- data.frame(pressure.5D6.mid[,c(2,3)])  #copy the column 2-3 to the list
pam.5D6$acceleration <- data.frame(acto.5D6.mid[,c(2,3)])
pam.5D6$temperature <- data.frame(temp.5D6.mid[,c(2,3)])
pam.5D6$light <- data.frame(acto.5D6.mid[,c(2,3)])   #create a list of light with acto schedule
pam.5D6$light[,2] <- rep(0,length(pam.5D6$light[,2])) #force all data to 0 as we don't have light data
names(pam.5D6) <- c("id","pressure","acceleration","temperature","light") #change "length" list to name "id"
pam.5D6$id <- as.character("5D6")
colnames(pam.5D6$pressure) <- c("date","obs")  #change column name for each list
colnames(pam.5D6$acceleration) <- c("date","obs")
colnames(pam.5D6$light) <- c("date","obs")
colnames(pam.5D6$temperature) <- c("date","obs")

summary(pam.5D6)
str(pam.5D6)
