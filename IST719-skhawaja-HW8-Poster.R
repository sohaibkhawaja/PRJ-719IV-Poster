# IST719 Spring 2017
# Author: Sohaib Khawaja SUID 395608518
# Purpose: HW8 - Final Project

#ASSIGNMENT INFORMATION
#Due Date: Tuesday, April 21, 2017 9:30 AM

#install.packages("RPostgreSQL")
library(RPostgreSQL)

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-prod.cr4nrslb1lw7.us-east-1.rds.amazonaws.com", port=5432, user="aact", password="aact")

# tables: facilities countries conditions interventions studies 

s.facilities <- dbGetQuery(con, "SELECT * FROM Facilities")
s.countries <- dbGetQuery(con, "SELECT * FROM Countries")
s.conditions <- dbGetQuery(con, "SELECT * FROM Conditions")
s.interventions <- dbGetQuery(con, "SELECT * FROM Interventions")

s.count <- dbGetQuery(con, "SELECT COUNT(*) FROM studies")

s.studies <- data.frame(dbGetQuery(con, 
                          "SELECT nct_id, start_date, completion_date, primary_completion_date, study_type, acronym, brief_title, overall_status, phase, source, is_fda_regulated_drug, is_fda_regulated_device 
                          FROM studies"))

#print(aact_open)
#write.csv(aact_sample, file='aact_sample.csv')

setwd("C:/DataScience/datasets/PRJ-719IV/HW8")

data.raw <- read.csv("studies-all.csv"
               , header = TRUE, sep = ","
               , stringsAsFactors = FALSE
               , skip = 0L)

str(data.raw)
colnames(data.raw)

my.raw <- data.raw[,c(1:11,13,18,20,24,26)]
colnames(my.raw) <- c("Rank", "NCT.Number", "Title", "Recruitment", "Study.Results"
                      , "Conditions", "Interventions", "Sponsor.Collaborators", "Gender", "Age"
                      , "Phases", "Funded.Bys", "Start.Date", "Last.Updated"
                      , "Primary.Completion.Date", "URL")
colnames(my.raw)

fac.vars <- c('Recruitment','Study.Results', 'Gender', 'Age', 'Phases')

my.raw[fac.vars] <- lapply(data.raw[fac.vars], function(x) as.factor(x))

sum(is.na(my.raw$Start.Date))
#my.raw$Startyr <- as.Date(strptime(my.raw$Start.Date, "%Y"))

s <- my.raw

str(s)

unique(s$Study.Results)

# studies: results status
s.hRes <- s[s$Study.Results == "Has Results",]

# studies: no results available
s.nRes <- s[s$Study.Results == "No Results Available",]


# studies recruitment status
unique(s$Recruitment)

# potential open studies available to patients
s.open <- s[s$Recruitment == "Not yet recruiting" |
            s$Recruitment == "Recruiting" |
            s$Recruitment == "Active, not recruiting" |
            s$Recruitment == "Enrolling by invitation" |
            s$Recruitment == "Available", ]

# studies with unknown recruitment satus
s.unk <- s[s$Recruitment == "Unknown status", ]

# products approved for marketing
s.app <- s[s$Recruitment == "Approved for marketing", ]


data.raw$Date <- as.Date(as.POSIXct(data.raw$Date, "%d/%m/%Y",tz = "UTC"))

par(mar= c(5, 5, 5, 3)+0.1, mfrow=c(1,1))

pen.col <- rep("darkgray", dim(soil)[1])

# Air Temperature over Time 
plot(soil$Date , soil$Air.Temperature.Max
     , type = "l"
     , fg = "gray"
     , col = "red"
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "Air Temperature (degrees F)", ylim = c(05, 95))
par(new=TRUE)
plot(soil$Date , soil$Air.Temperature.Min
     , type = "l"
     , fg = "gray"
     , col = "blue"
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "", ylim = c(05, 95))
mtext(text = "Air Temperature over Time", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service | Tahoe City Cross data", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")

legend("topright", bty ="n", c("Max Temp", "Min Temp"), lty =c(1,1), col = c("red", "blue"))


# Soil Temperature over Time
plot(soil$Date , soil$Soil.Temperature.02in
     , type = "l"
     , fg = "gray"
     , col = pen.col 
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "Soil Temperature (degrees F)", ylim = c(25,75))

par(new=TRUE)
plot(soil$Date , soil$Soil.Temperature.08in
     , type = "l"
     , fg = "gray"
     , col = "blue"
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "", ylim = c(25,75))

par(new=TRUE)
plot(soil$Date , soil$Soil.Temperature.20in
     , type = "l"
     , fg = "gray"
     , col = "red"
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "", ylim = c(25,75))
mtext(text = "Soil Temperature over Time", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
legend("topleft", bty = "n", c("2in depth", "8in depth", "20in depth"), lty =c(1,1,1), col = c("gray", "blue", "red"))
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service | Tahoe City Cross data", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")


# Soil Moisture over Time
plot(soil$Date , soil$Soil.Moisture.02in
     , type = "l"
     , fg = "gray"
     , col = pen.col
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "Soil Moisture", ylim = c(0,50))

par(new=TRUE)
plot(soil$Date , soil$Soil.Moisture.08in
     , type = "l"
     , fg = "gray"
     , col = "blue"
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "", ylim = c(0,50))

par(new=TRUE)
plot(soil$Date , soil$Soil.Moisture.20in
     , type = "l"
     , fg = "gray"
     , col = "red"
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "", ylim = c(0,50))
mtext(text = "Soil Moisture over Time", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
legend("topleft", bty = "n", c("2in depth", "8in depth", "20in depth"), lty =c(1,1,1), col = c("gray", "blue", "red"))
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service | Tahoe City Cross data", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")

# Precipitation Increment over Time
plot(soil$Date , soil$Precipitation.Increment
     , type = "l"
     , fg = "gray"
     , col = "black"
     , lwd = .5
     , pch = 46
     , cex = 1
     , las = 1
     , xlab = ""
     , ylab = "Precipitation Increment")
mtext(text = "Precipitation Increment over Time", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service | Tahoe City Cross data", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")


## scatter plots with regression line. 

## Air Temperature ~ Soil Temperature 2in and Trend line
plot(soil$Soil.Temperature.02in ~ soil$Air.Temperature.Max, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = pen.col,
     xlim = c(15,95), xlab = "Air Temperature (degrees F)",
     ylim = c(25,75), ylab = "Soil Temperature (degrees F)")

## Air Temperature ~ Soil Temperature 8in and Trend line
par(new=TRUE)
plot(soil$Soil.Temperature.08in ~ soil$Air.Temperature.Max, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "dodgerblue",
     xlim = c(15,95), xlab = "",
     ylim = c(25,75), ylab = "")

## Air Temperature ~ Soil Temperature 20in and Trend line
par(new=TRUE)
plot(soil$Soil.Temperature.20in ~ soil$Air.Temperature.Max, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "brown",
     xlim = c(15,95), xlab = "",
     ylim = c(25,75), ylab = "")
abline(r2a <- lm(soil$Soil.Temperature.02in ~ soil$Air.Temperature.Max), col = "darkgray", lwd=2)
abline(r2b <- lm(soil$Soil.Temperature.08in ~ soil$Air.Temperature.Max), col="blue", lwd=2)
abline(r2c <- lm(soil$Soil.Temperature.20in ~ soil$Air.Temperature.Max), col="brown", lwd=2)
mtext(text = "Air Temperature ~ Soil Temperature", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")

legend("topleft", bty="n", legend = c("at 2in depth", "at 8in depth", "at 20in depth"), col = c("gray", "blue", "brown"), pch =16, cex = 0.9)
legend(32,77, bty="n", legend=paste("R2 at 2in is", format(summary(r2a)$adj.r.squared, digits=4)), cex = 0.9)
legend(32,75, bty="n", legend=paste("R2 at 8in is", format(summary(r2b)$adj.r.squared, digits=4)), cex = 0.9)
legend(32,73, bty="n", legend=paste("R2 at 20in is", format(summary(r2c)$adj.r.squared, digits=4)), cex = 0.9)


## Air Temperature ~ Soil Moisture 2in and Trend line
plot(soil$Soil.Moisture.02in ~ soil$Air.Temperature.Max, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = pen.col,
     ylim = c(0,50), xlab = "Air Temperature (degrees F)",
     xlim = c(15,100), ylab = "Soil Moisture")

## Air Temperature ~ Soil Moisture 8in and Trend line
par(new=TRUE)
plot(soil$Soil.Moisture.08in ~ soil$Air.Temperature.Max, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "dodgerblue",
     ylim = c(0,50), xlab = "",
     xlim = c(15,100), ylab = "")

## Air Temperature ~ Soil Moisture 20in and Trend line
par(new=TRUE)
plot(soil$Soil.Moisture.20in ~ soil$Air.Temperature.Max, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "brown",
     ylim = c(0,50), xlab = "",
     xlim = c(15,100), ylab = "")
abline(r2a <- lm(soil$Soil.Moisture.02in ~ soil$Air.Temperature.Max), col = pen.col, lwd=2)
abline(r2b <- lm(soil$Soil.Moisture.08in ~ soil$Air.Temperature.Max), col = "blue", lwd=2)
abline(r2c <- lm(soil$Soil.Moisture.20in ~ soil$Air.Temperature.Max), col = "brown", lwd=2)
mtext(text = "Air Temperature ~ Soil Moisture", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")

legend(22,52, bty="n", legend = c("at 2in depth", "at 8in depth", "at 20in depth"), col = c("gray", "blue", "brown"), pch =16, cex = 0.9)
legend(50,52, bty="n", legend=paste("R2 at 2in is", format(summary(r2a)$adj.r.squared, digits=4)), cex = 0.9)
legend(50,50, bty="n", legend=paste("R2 at 8in is", format(summary(r2b)$adj.r.squared, digits=4)), cex = 0.9)
legend(50,48, bty="n", legend=paste("R2 at 20in is", format(summary(r2c)$adj.r.squared, digits=4)), cex = 0.9)



## Soil Temperature 2in ~ Soil Moisture 2in and Trend line
plot(soil$Soil.Temperature.02in ~ soil$Soil.Moisture.02in, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = pen.col,
     xlim = c(0,50), xlab = "Soil Moisture",
     ylim = c(25,80), ylab = "Soil Temperature (degrees F)")

# Soil Temperature 8in ~ Soil Moisture 8in and Trend line
par(new=TRUE)
plot(soil$Soil.Temperature.08in ~ soil$Soil.Moisture.08in, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "dodgerblue",
     xlim = c(0,50), xlab = "",
     ylim = c(25,80), ylab = "")
abline(r2 <- lm(soil$Soil.Temperature.08in ~ soil$Soil.Moisture.08in), col="blue", lwd=2)

# Soil Temperature 20in ~ Soil Moisture 20in and Trend line
par(new=TRUE)
plot(soil$Soil.Temperature.20in ~ soil$Soil.Moisture.20in, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "brown",
     xlim = c(0,50), xlab = "",
     ylim = c(25,80), ylab = "")
abline(r2a <- lm(soil$Soil.Temperature.02in ~ soil$Soil.Moisture.02in), col="darkgray", lwd=2)
abline(r2b <- lm(soil$Soil.Temperature.08in ~ soil$Soil.Moisture.08in), col="blue", lwd=2)
abline(r2c <- lm(soil$Soil.Temperature.20in ~ soil$Soil.Moisture.20in), col="brown", lwd=2)
mtext(text = "Soil Temperature ~ Soil Moisture", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")

legend("topleft", bty="n", legend = c("at 2in depth", "at 8in depth", "at 20in depth"), col = c("gray", "blue", "brown"), pch =16, cex = 0.9)
legend(10,82, bty="n", legend=paste("R2 at 2in is", format(summary(r2a)$adj.r.squared, digits=4)), cex = 0.9)
legend(10,80, bty="n", legend=paste("R2 at 8in is", format(summary(r2b)$adj.r.squared, digits=4)), cex = 0.9)
legend(10,78, bty="n", legend=paste("R2 at 20in is", format(summary(r2c)$adj.r.squared, digits=4)), cex = 0.9)



## Precipitation Increment ~ Soil Temperature 2in and Trend line

plot(soil$Soil.Temperature.02in ~ soil$Precipitation.Increment, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = pen.col,
     xlim = c(0, 5.5), xlab = "Precipitation Increment",
     ylim = c(25,75), ylab = "Soil Temperature (degrees F)")

## Precipitation Increment ~ Soil Temperature 8in and Trend line
par(new = TRUE)
plot(soil$Soil.Temperature.08in ~ soil$Precipitation.Increment, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "dodgerblue",
     xlim = c(0, 5.5), xlab = "",
     ylim = c(25,75), ylab = "")

## Precipitation Increment ~ Soil Temperature 20in and Trend line
par(new = TRUE)
plot(soil$Soil.Temperature.20in ~ soil$Precipitation.Increment, data = soil,
     pch = 16,
     cex = 1,
     las = 1,
     col = "brown",
     xlim = c(0, 5.5), xlab = "",
     ylim = c(25,75), ylab = "")
abline(r2a <- lm(soil$Soil.Temperature.02in ~ soil$Precipitation.Increment), col="darkgray", lwd=2)
abline(r2b <- lm(soil$Soil.Temperature.08in ~ soil$Precipitation.Increment), col="blue", lwd=2)
abline(r2c <- lm(soil$Soil.Temperature.20in ~ soil$Precipitation.Increment), col="brown", lwd=2)
mtext(text = "Soil Temperature ~ Precipitation Increment", side = 3, line = 1, adj = 0, cex = 2, col = "brown")
mtext(text = "Source: US Department of Agriculture | Natural Resources Conservation Service", side = 1, line = 3, adj = 1, cex = .5, col = "darkgray")

legend(3.5, 75, bty="n", legend = c("at 2in depth", "at 8in depth", "at 20in depth"), col = c("gray", "blue", "brown"), pch =16, cex = 0.9)
legend(4.5,75, bty="n", legend=paste("R2 at 2in is", format(summary(r2a)$adj.r.squared, digits=4)), cex = 0.75)
legend(4.5,73, bty="n", legend=paste("R2 at 8in is", format(summary(r2b)$adj.r.squared, digits=4)), cex = 0.75)
legend(4.5,71, bty="n", legend=paste("R2 at 20in is", format(summary(r2c)$adj.r.squared, digits=4)), cex = 0.75)

