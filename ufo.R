#ufo

#import data

ufo_raw <- read.csv("ufo_sightings/UFO_sightings.csv", na.strings = "")

#characterize
head(ufo_raw)
dim(ufo_raw)
sapply(ufo_raw, class)

table(as.factor(ufo_raw$state))
table(as.factor(ufo_raw$country))
table(as.factor(ufo_raw$shape))


sum(complete.cases(ufo_raw))
nrow(ufo_raw)-sum(complete.cases(ufo_raw))

#dates
date.posted <- strptime(ufo_raw$date.posted, "%m/%d/%Y")
datetime <- strptime(ufo_raw$datetime, "%m/%d/%Y %H:%M")

#get time zone
timezones <- tz_lookup_coords(latitude, ufo_raw$longitude, method = "accurate")

#location
state <- as.factor(ufo_raw$state)
sum(is.na(state))

country <- as.factor(ufo_raw$country)
sum(is.na(country))

#missing state - check city
missing.state <- ufo_raw$city[is.na(state)]



#missing value datetime
ufo_raw$datetime[is.na(datetime)]

#missing value latitude
ufo_raw[is.na(as.numeric(ufo_raw$latitude)),]



#complete country by state
all.states <- unique(ufo_raw$state)

#test api google
api.key <- "AIzaSyANMWKJEsIE6rjjYPHGbwLVWfJU0LChYGE"
library(jsonlite)
library(httr)
library(XML)
library(RJSONIO)
latlngStr <- gsub(' ','%20', paste(round(as.numeric(ufo_raw[1, 10:11]),2), collapse="%20"))
url   <- "https://maps.google.com"
connectStr <- paste('https://maps.google.com/maps/api/geocode/json?',
                    'sensor=false&latlng=',latlngStr, 
                    '&key=', api.key, sep="")
con <- url(connectStr)
data.json <- fromJSON(paste(readLines(con), collapse=""))
close(con)
data.json <- unlist(data.json)

path  <- "/maps/api/geocode/json"
query <- list(sensor="false",latlng=q)
response <- GET(url, path=path, query=query)
xml    <- xmlInternalTreeParse(content(response,type="text"))
status <- xmlValue(getNodeSet(xml,"//status")[[1]])

test.url <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=',q, sep="")
##DIDN'T WORK - REQUIRES BILLING


#TRY OTHER LIBRARIES
install.packages(c("sp", "rworldmap"))
library(sp)
library(rworldmap)

coords2country = function(points)
{  
        countriesSP <- getMap(resolution='low')
        #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
        
        # convert our list of points to a SpatialPoints object
        
        # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
        
        #setting CRS directly to that from rworldmap
        pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
        
        
        # use 'over' to get indices of the Polygons object containing each point 
        indices = over(pointsSP, countriesSP)
        
        # return the ADMIN names of each country
        #indices$ADMIN  
        indices$ISO3 # returns the ISO3 code 
        #indices$continent   # returns the continent (6 continent model)
        #indices$REGION   # returns the continent (7 continent model)
}
points = data.frame(lon=c(0, 5, 10, 15, 20), lat=c(51.5, 50, 48.5, 47, 44.5))
coords2country(data.frame(lng,lat)[1:5,])

lat <- round(as.numeric(ufo_raw$latitude),2)
lng <- round(as.numeric(ufo_raw$longitude),2)

#
#show info by decade
#show info by time
#show info in map
#show info in map by freq city


#comments
#fix time zone in datetime