library(jsonlite)
library(plotly)
library(quiltr)
library(ggplot2)

# Data from the electricity company
dat <- readLines("C:/_Eivarmisteta/sähködata2016_2019_05D.json")
#dat <- fromJSON("C:/_Eivarmisteta/sähködata2016_2019_05D.json")

dat <- readLines("C:/_Eivarmisteta/sähköNiuvantie10B.json")

dat <- gsub("'xDim': 'kWh',", "'xDim': 'kWh'", dat)
dat <- gsub("'", "\"", dat)
#head(dat)
dat <- as.data.frame(fromJSON(dat))
dat <- data.frame(
  Date = as.POSIXct(dat$electricity.timeStamp, format="%e.%m.%Y"),
  Time = dat$electricity.timeStamp,
  Hour = substr(dat$electricity.xTime, nchar(dat$electricity.xTime)-4, 100),
  Result = dat$electricity.kulutus
)
dat$Time <- as.POSIXct(paste(dat$Time, dat$Hour), format="%e.%m.%Y %H:%M", tz="EET")
dat$Hour <- as.numeric(substr(dat$Hour, 1,2))
dat$Weekday <- weekdays.Date(dat$Date)
dat$Month <- months.Date(dat$Date)
dat$Month <- factor(dat$Month, levels=c(
  "tammikuu","helmikuu","maaliskuu","huhtikuu","toukokuu","kesäkuu",
  "heinäkuu","elokuu","syyskuu","lokakuu","marraskuu","joulukuu"
))
dat <- na.omit(dat)
dat <- dat[c(1:3,5:6,4)]

# Store data to Quilt
qbuild("jtuomsto/cnh/power_consumption_example", dat)
qpush("jtuomsto/cnh", public=TRUE)

tmp <- dat
tmp$Time <- as.character(tmp$Time)
tmp$Date <- as.character(tmp$Date)
write.csv(tmp, "D:/NiuvantienTilastoja/Sähkönkulutus.csv")

########################

# devtools::install_github("rOpenGov/rwfs") # Install these if you don't have them already
# devtools::install_github("rOpenGov/fmi")
# library(fmi) # You don't need this package if you use the download service https://ilmatieteenlaitos.fi/havaintojen-lataus#!/

library(quiltr)
library(ggplot2)
library(plotly)

# qinstall("jyrjola/fmi") # You have to install the package to your own computer when using it for the first time.
dasol <- qload("jyrjola/fmi","solar_radiation_savilahti")
#> colnames(dasol)
#[1] "time"                       "Diffuse radiation"          "Global radiation"          
#[4] "Long wave solar radiation " "Sunshine duration"          "Ultraviolet irradiance"    

colnames(dasol) <- c("Time","Diffuse","Global","Longwave","Sun","UV")

dat <- qload("jtuomsto/cnh", "power_consumption_example")

dat <- merge(dat, dasol, all.x=TRUE)

# Data loaded from  https://ilmatieteenlaitos.fi/havaintojen-lataus#!/ about Kuopio Savilahti station
# on 28.6.2019 for 2016-01-01 - 2019-06-28
temp <- rbind(
  read.csv("C:/Users/jtue/AppData/Local/Temp/csv-4657f65d-bde8-49e4-b6a6-da11b6cbd0be.csv"),
  read.csv("C:/Users/jtue/AppData/Local/Temp/csv-da3dd839-c93b-4353-8726-f252db004726.csv")
)

temp$Time <- as.POSIXct(paste0(temp$Vuosi, "-", temp$Kk, "-", temp$Pv, " ", temp$Klo), format="%Y-%m-%d %H:%M", tz="UTC")
colnames(temp)
temp[1:5] <- NULL
colnames(temp) <- c("Rain","Snow","Temperature","Dewpoint","Winddirection","Windmax","Windspeed","Time")

dat <- merge(dat, temp, all.x = TRUE)
dat$Temp_heat <- pmin(0, dat$Temperature - 17)
dat$Hourf <- as.factor(dat$Hour)

ggplot(dat, aes(x=Time, y=Result))+geom_line(colour="red")+geom_line(aes(y=Temperature), colour="blue")

ggplot(dat, aes(x=Temperature, y= Result))+geom_point(size=0.1)

dat2 <- na.omit(dat[c("Time","Date","Temp_heat","Hourf","Weekday","Windspeed","Result")])
lin <- lm(Result ~ Temp_heat + Hourf * Weekday + Windspeed, dat=dat2)
summary(lin)

dat2 <- cbind(dat2, Prediction = predict(lin))

plot_ly(x=1:4, y=3:6, type="scatter",mode="line")

plot_ly(dat2, x=~Time, y=~Result, type="scatter", mode="line", name="Measured") %>%
  add_trace(y=~Prediction, mode="markers", name="Prediction")

plot_ly(aggregate(dat2[c("Result","Prediction")], by=dat2["Date"], mean),
        x=~Date, y=~Result, type="scatter", mode="line") %>%
  add_trace(y=~Prediction, mode="markers")
  layout(title="Päivittäinen sähkönkulutus",yaxis=list(title="kWh/d"))

cor(dat2[c("Result","Prediction")])^2
cor(aggregate(dat2[c("Result","Prediction")], by=dat2["Date"], sum)[2:3])^2

ggplot(aggregate(dat["Result"], by=dat["Date"], sum),
       aes(x=Date, y=Result/24))+geom_line()+
  labs(title="Päivittäinen keskimääräinen sähkötehonnkulutus",y="kW")

ggsave("Päivittäinen keskimääräinen sähkötehonkulutus.png", width=10, height=7)

plot_ly(aggregate(dat["Result"], by=dat[c("Hour","Weekday")], mean),
        x=~Hour, y=~Result, color=~Weekday, type="scatter", mode="line") %>%
  layout(title="Sähkönkulutusteho tunneittain eri viikonpäivinä", yaxis=list(title="kW"))

ggplot(aggregate(dat["Result"], by=dat[c("Hour","Weekday")], mean),
       aes(x=Hour, y=Result, color=Weekday))+geom_line()+
  labs(title="Sähkönkulutusteho tunneittain eri viikonpäivinä", y="kW")

ggsave("Sähkönkulutus tunneittain eri viikonpäivinä.png", width=10, height=7)

plot_ly(aggregate(dat["Result"], by=dat[c("Month")], mean),
        x=~Month, y=~Result, type="scatter",mode="line") %>%
  layout(title="Sähkönkulutusteho kuukausittain",yaxis=list(title="kW"))

ggplot(aggregate(dat["Result"], by=dat[c("Month")], mean),
       aes(x=Month, y=Result))+geom_point()+
  labs(title="Sähkönkulutusteho kuukausittain", y="kW")

ggsave("Sähkönkulutusteho kuukausittain.png",width=10,height=7)

ggplot(dat, aes(x=Result,color=Month))+stat_ecdf()+
  labs(title="Sähkönkulutus tunneittain, kertymäjakauma", x="kW",y="Kertymätodennäköisyys")

ggsave("Sähkönkulutus tunneittain kertymä.png", width=10, height=7)

