library(rgdal)
library(dplyr)
library(lubridate)
library(leaflet)

# Boundary data from https://github.com/datasets/geo-countries

countries <- readOGR("../../data/countries.geojson")
m <- leaflet(countries) %>% addTiles() %>% addPolygons(weight = 2, color = "black")
m
m <- leaflet(countries) %>% addPolygons(weight = 1, color = "black", label = countries$ADMIN)

codata <- countries@data

# Downloaded COVID data 04/01/20, 8:30 AM EDT.

# download.file("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", "data/casedistribution.csv", mode = "wb")

cov <- read.csv("data/casedistribution.csv", stringsAsFactors = FALSE)

# Research for diffs in codes (2 digit)

cn.ids <- sort(unique(countries$ISO_A2))
cv.ids <- sort(unique(cov$geoId))
length(cn.ids)
length(cv.ids)

testid.1 <- full_join(data.frame(x=cn.ids), data.frame(x=cv.ids))
testid.1 <- full_join(data.frame(x=cn.ids,cnid= cn.ids), data.frame(x=cv.ids, cvid = cv.ids))
x <- as.character(testid.1$cvid[is.na(testid.1$cnid)])
id3.fix <- unique(filter(cov, geoId %in% x)$countryterritoryCode)
codata[codata$ISO_A3 %in% id3.fix,]

# diffs in 2 digit country codes
# GB = UK
# GR = EL
# Kosovo has no codes in countries, but it does have a region.
# JP... in cov is for an international conveyance


# Fixes for known differences in codes
cov$geoId[cov$geoId == "UK"] <- "GB"
cov$geoId[cov$geoId == "EL"] <- "GR"


names(cov)[1] <- "date"
cov$date <- dmy(cov$date)
last.report <- max(cov$date)
base.end <- last.report - 7
base.start <- base.end - 13
covuniq <- sort(unique(cov$geoId))
cov.base <- sapply(covuniq, function(x) sum(filter(cov, between(date, base.start, base.end), geoId == x)$cases))
cov.growth <- cov.add / (cov.base)
cov.data <- data.frame(ISO_A2 = covuniq, base = as.vector(cov.base), add = as.vector(cov.add), growth = as.vector(cov.growth))
codata <- codata %>% left_join(cov.data)
codata$base[is.na(codata$base)] <- 0
codata$add[is.na(codata$add)] <- 0
codata$growth[is.na(codata$growth)] <- 0

codata$loggrowth <- log(codata$growth + 1)

pal <- colorNumeric(
    palette = "Blues",
    domain = codata$loggrowth
)

qpal <- colorQuantile(
    "Blues",
    codata$growth,
    n = 8
)

palbin <- colorBin(
    palette = "YlOrRd",
    bins <- length(bins),
    domain = bins
)

m4 <- leaflet(co2) %>% addPolygons(weight = 1, color = "black", fillColor = ~palbin(co2$growth), label = countries$ADMIN)

labels <- sprintf("<strong>%s</strong><br />Growth Rate: %g<br />Prev: %g / New: %g", codata$ADMIN, round(codata$growth * 100, 1), codata$base, codata$add) %>% lapply(htmltools::HTML)
m4 <- leaflet(co2) %>% addPolygons(weight = 1, color = "black", fillColor = ~palbin(co2$growth), fillOpacity = 1, label = labels)

labels <- sprintf("<strong>%s</strong><br />New Case Rate: %g%%<br />Prev: %g / New: %g", codata$ADMIN, round(codata$growth * 100, 1), codata$base, codata$add) %>% lapply(htmltools::HTML)
head(labels)
m4 <- leaflet(co2) %>% addPolygons(weight = 1, color = "black", fillColor = ~palbin(co2$growth), fillOpacity = 1, label = labels)
m4


