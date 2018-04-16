# load packages
require(rvest)
require(tidyverse)
require(rgdal)
require(raster)
require(cowplot) #dev version
require(scales)
require(extrafont)
require(animation)
require(zoo)

# load more fonts for plotting (via extrafont package)
loadfonts(device = "win")

# set locale to have dates in english language (e.g. january NOT Januar)
Sys.setlocale("LC_TIME", "C")

# load filenames from latest date folder (copy paste from: https://archive.luftdaten.info/2018-03-27/)
filenames = read.table("filenames.txt", sep = "\t", header=TRUE)

# filter on SDS011 sensors
filenames = filenames[grepl("*sds01*", filenames$Name),]

# format variables
filenames$path = paste("https://archive.luftdaten.info/2018-03-27/",filenames$Name, sep = "")
filenames$sensorid = gsub(".*sensor_*","", filenames$Name)
filenames$sensorid = gsub(".csv","", filenames$sensorid)

# download files once and put to sensor folder
for(i in seq_along(filenames$path)){
  download.file(filenames$path[i], paste(i,".csv",sep = ""), mode="wb")
}

# load sensor data and add to filenames 
datalist = list()
for (j in 1:nrow(filenames)) {
  
  sensor = read.csv(paste(j,".csv",sep = ""), sep = ";")
  sensor = sensor[1,]
  datalist[[j]] = sensor
}
sensor = do.call(rbind, datalist)
filenames = merge(filenames, sensor, by.x = "sensorid", by.y = "sensor_id")
rm(sensor, j, datalist)

# select columns
filenames = select(filenames, Name, lat, lon)

# format date
filenames$Name = gsub("2018-03-27_","",filenames$Name)
filenames$activation_date = "2010-01-01"

# load foldernames (copy paste from: https://archive.luftdaten.info/)
setwd("C:/Users/akruse/Documents/")
foldernames = read.table("foldernames.txt", sep = "\t")

# format foldernames
foldernames$link = paste("https://archive.luftdaten.info/",foldernames$V2, sep = "")
foldernames$V2 = gsub("/","",foldernames$V2)
foldernames$seq = seq(1:nrow(foldernames))
foldernames = foldernames[order(-foldernames$seq),] 

# add earliest date of sensor appearence in folders for each sensor (for new html structure)
for (j in 440:444){
  
  url = foldernames$link[j]
  tables = getURL(url,.opts = list(ssl.verifypeer = FALSE) )
  tables = readHTMLTable(tables)
  tables = as.data.frame(tables[[1]])
  print(j)
  
  for (i in 1:nrow(filenames)) {
    
    filenames$check[i] = sum(ifelse(grepl(paste("*",filenames$Name[i],"*", sep = ""), tables$Name), 1, 0))
    filenames$activation_date[filenames$check == 1] = foldernames$V2[j]
    
  }
}

# add earliest date of sensor appearence in folders for each sensor (for old html structure)
for (j in 445:nrow(foldernames)) {
  
  url = foldernames$link[j]
  doc = xml2::read_html(url)
  print(j)
  
  for (i in 1:nrow(filenames)) {
    
    filenames$check[i] = as.character(grepl(paste("*",filenames$Name[i],"*", sep = ""), doc))
    filenames$activation_date[filenames$check == "TRUE"] = foldernames$V2[j]
    
  }
}

# select columns
mydata = dplyr::select(filenames, lat, lon, activation_date)

# format date
mydata$activation_date = as.Date(mydata$activation_date, "%Y-%m-%d") 

# reverse geocode to get name of city of each 
#data_geo = reverse_geocode_coords(mydata$lat, mydata$lon, key = "")

# add geo info to dataframe
data_geo$lat = NULL
data_geo$lon = NULL
data_geo = cbind(mydata,data_geo)

# filter on german cities
city_data = filter(data_geo, state == "Hamburg" | state == "Berlin" | city == "Munich" | city == "Stuttgart" | city == "Cologne")
city_data$group[city_data$city == "Munich"] = "Munich"
city_data$group[city_data$city == "Cologne"] = "Cologne"
city_data$group[city_data$city == "Stuttgart"] = "Stuttgart"
city_data$group[city_data$state == "Berlin"] = "Berlin"
city_data$group[city_data$state == "Hamburg"] = "Hamburg"
city_data = dplyr::select(city_data, activation_date, group)

# sort dataframe
city_data = city_data[order(city_data$activation_date),]

# add count variable
city_data$count = with(city_data, ave(group, group, FUN = seq_along))

# get first date of sensor for each city and last date of date (for later use)
nope = filter(city_data, count == 1) %>% dplyr::select(activation_date)
nope = c(as.vector(nope$activation_date),max(city_data$activation_date))

# create empty dataframe for missing dates for each city
seq_hh = as.data.frame(seq(min(city_data$activation_date),max(city_data$activation_date), by = 1))
colnames(seq_hh) = "activation_date"
seq_hh$group = "Hamburg"

seq_col = as.data.frame(seq(min(city_data$activation_date),max(city_data$activation_date), by = 1))
colnames(seq_col) = "activation_date"
seq_col$group = "Cologne"

seq_stu = as.data.frame(seq(min(city_data$activation_date),max(city_data$activation_date), by = 1))
colnames(seq_stu) = "activation_date"
seq_stu$group = "Stuttgart"

seq_ber = as.data.frame(seq(min(city_data$activation_date),max(city_data$activation_date), by = 1))
colnames(seq_ber) = "activation_date"
seq_ber$group = "Berlin"

seq_muc = as.data.frame(seq(min(city_data$activation_date),max(city_data$activation_date), by = 1))
colnames(seq_muc) = "activation_date"
seq_muc$group = "Munich"

# bind everything togehter
seq_city = rbind(seq_hh,seq_muc,seq_col,seq_ber,seq_stu)
seq_city = merge(seq_city,city_data, by = c("activation_date","group"), all.x = T)
seq_city$count = as.numeric(seq_city$count)

# order dataframe
seq_city = seq_city[with(seq_city, order(group, activation_date, count)), ]

# fill up rows with NAs
seq_city$count[seq_city$group == "Berlin" & seq_city$activation_date == "2016-08-11"] = 0
seq_city$count[seq_city$group == "Hamburg" & seq_city$activation_date == "2016-08-11"] = 0
seq_city$count[seq_city$group == "Cologne" & seq_city$activation_date == "2016-08-11"] = 0
seq_city$count[seq_city$group == "Munich" & seq_city$activation_date == "2016-08-11"] = 0
seq_city$count = na.locf(seq_city$count)

# remove duplicates
seq_city = seq_city %>% group_by(activation_date, group) %>% top_n(1, count)

# convert dates without sensor to NA
seq_city$count[seq_city$count == 0] = NA 
seq_city$date_num = as.numeric(seq_city$activation_date)

# defince colorscale
cols <- c("Hamburg"="#3b518bff","Berlin"="#21908dff","Stuttgart"="#440154ff","Cologne"="#fde725ff","Munich"="#5cc863ff")

# define order of factor levels
seq_city$group <- factor(seq_city$group, levels = c("Stuttgart","Hamburg","Berlin","Munich","Cologne"))
seq_city = as.data.frame(seq_city)

# subset dataframe to less dates (GIFs) and add important dates (from above)
seq_city = subset(seq_city, date_num %in% c(seq(min(seq_city$date_num),max(seq_city$date_num), 3),nope))

# plot function
make_plot_appear <- function(i = min(seq_city$date_num), maxi = max(seq_city$date_num)){
  data = filter(seq_city, date_num <= i)
  ggplot(data, aes(x = activation_date, y = count)) +
  geom_line(aes(color = group)) +
  geom_point(data=. %>% dplyr:: filter(date_num==max(date_num)), mapping=aes(x=activation_date, y=count, color=group), size=3) +
  geom_point(data=. %>% filter(date_num==max(date_num)), mapping=aes(x=activation_date, y=count), color="#f5f5f2", size=2) +
  ylim(1, 250) +
  scale_x_date(date_breaks = "3 month", labels=date_format("%b %y"), limits = as.Date(c(min(seq_city$activation_date),max(seq_city$activation_date)))) +
  theme(text=element_text(family = "Tw Cen MT"),
        axis.text.x = element_text(color = "#4e4d47", size = 10),
        axis.text.y = element_text(color = "#4e4d47", size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.5, size=18, face="bold"),
        plot.caption = element_text(hjust=0.5, size = 8, color = "#4e4d47"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        plot.subtitle = element_text(color = "#4e4d47", hjust = 0.5),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_blank()) +
    theme(plot.margin = unit(c(0.2,2,0.1,1),"cm")) +
    labs(caption = "The graphic displays SDS011 sensors (from German cities with more than 50 sensors) which went live before 27.03.2018 as part \n of the airrohr project. Source: archive.luftdaten.info.") +
    labs(title = paste0("The Number of Fine Dust Sensors in German Cities"), subtitle = "Airrohr ~ Citizen Science ~ Luftdaten.info")
}

# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/akruse/Documents/gif", "/plot_",i ,".png")
  ggsave(filename=file_path, make_plot_appear(i))
  
}

# save plot
map(unique(seq_city$date_num), plot.save)
