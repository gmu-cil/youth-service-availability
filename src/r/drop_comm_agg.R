library(ggmap)
library(ggplot2)
library(stringr)
library(readr)
library(dplyr)
library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(classInt)
library(data.table)

setwd("/Users/jinyikim/Desktop/YouthRisk/Git/youth")
# Dr. Lee's api2 <- "Google API key"
register_google(key = api)

# Extracting geo_unique_all points data as SpatialPointsDataFrame
#total <- read.csv(file.choose())
total <- read_csv("geo_unique_all.csv",col_names = T )
total$ID <- row.names(total)
total$ID <- as.integer(total$ID)
xy <- total[,c("lon","lat")]
points <- SpatialPointsDataFrame(coords = xy, data = total,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(points)
View(points@data)

# VA Boundary as a polygon
va_boundary <- readOGR("Virginia_City_County_Census_Boundaries/SDE_USDC_CENSUS_VA_COUNTY.shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
map <- get_map(location = 'Virgina', zoom = 7, color = "bw")
mapPoints <- ggmap(map) + geom_polygon(aes(x=long, y=lat, group=group), 
                                       data = va_boundary, color='red', alpha=0.5) + ggtitle("VA Boundary")
mapPoints


## Intersecting between two layers to aggregate points to the polygon layer.
intersections <- raster::intersect(x = points, y = va_boundary)
View(intersections@data)

point_counts <- intersections@data %>% group_by(GEOID) %>% summarise(num_service=n())
View(point_counts)

va_boundary@data <- va_boundary@data %>% left_join(point_counts, by=c("GEOID"))

va_boundary@data$num_service_per_unit_area <- va_boundary@data$num_service / area(va_boundary) * 10000 * 10000

write.csv(va_boundary, "clinic_num_area_bycounty.csv")

# writeOGR(va_boundary, "aggregation.geojson", layer="polygons", driver="GeoJSON", check_exists = FALSE)

#### Data join
dropout <-read_csv("stu_info_130.csv",col_names = T)
View(dropout)
communities <-read_csv("communities_130.csv",col_names = T)
View(communities)
dropout_communities <- dropout %>% left_join (communities, by ="Division") 
View(dropout_communities)

va_boundary@data<- dropout_communities %>% left_join(va_boundary@data, by=c("Division"="NAMELSAD"))
View(va_boundary@data)

write.csv(va_boundary@data, "clinic_drop_communities_bycounty.csv")

# writeOGR(va_boundary, "dropout_attached.geojson", layer="polygons", driver="GeoJSON", check_exists = FALSE)

names(va_boundary@data)
