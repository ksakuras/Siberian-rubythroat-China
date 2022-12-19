#using the stats data set for analysis
getwd()
stats = read.csv("stats.csv")
str(stats)

stats$start = as.POSIXct(stats$start,tz = "GMT", format = c("%Y-%m-%d %H:%M")) #capital sensitive!
stats$end = as.POSIXct(stats$end,tz = "GMT", format = c("%Y-%m-%d %H:%M")) #capital sensitive!

china = subset(stats, stats$logger != "5CF")
china = subset(china, stats$order > 0)

unique(china$logger)
china.sta = subset(china, china$behavior == "stopover")
china.fly = subset(china, china$behavior == "flight")


mean(china.sta$duration.day)
hist(subset(china.sta, china.sta$season == "autumn")$duration.day, xlab = "number of days", main = "distribution",xlim = c(0,30))
hist(subset(china.sta, china.sta$season == "spring")$duration.day, xlab = "number of days", main = "distribution", xlim = c(0,30))
?hist

boxplot(china.sta$duration.day~china.sta$logger)

boxplot(china.fly$duration~china.fly$logger)

plot(subset(china.fly, china.fly$season == "autumn")$duration~subset(china.fly, china.fly$season == "autumn")$order,
     ylab = "duration", xlab = "migration process", main = "autumn migration flight hour")
autumn.fly = lm(subset(china.fly, china.fly$season == "autumn")$duration~subset(china.fly, china.fly$season == "autumn")$order)
summary(autumn.fly)
abline(autumn.fly)

plot(subset(china.fly, china.fly$season == "spring")$duration~subset(china.fly, china.fly$season == "spring")$order,
     ylab = "duration", xlab = "migration process", main = "spring migration flight hour")
spring.fly = lm(subset(china.fly, china.fly$season == "spring")$duration~subset(china.fly, china.fly$season == "spring")$order)
summary(spring.fly)
abline(spring.fly)

china.fly$logger = as.factor(china.fly$logger)
p <- ggplot(subset(china.fly, china.fly$season == "spring"), aes(x=logger, y=duration)) +
  geom_violin() +
  ylim(0,16)
p
