# load packages
require(rvest)
require(tidyverse)
require(rgdal)
require(raster)
require(cowplot) #dev version
require(scales)
require(extrafont)
require(animation)

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

# filter on sensors in Hamburg
mydata = filter(mydata, lat > 53.35 & lat < 53.75 & lon > 9.7 & lon < 10.4)

# order by date and add count variable
mydata = mydata[order(mydata$activation_date),] 
mydata$count = seq(1:nrow(mydata))

# get shapefile of Hamburg
hh_shape = readOGR(".","HH_ALKIS_Bezirke")

# Remove sensors outside of shapefile of Hamburg
inout = over(
  SpatialPoints(mydata[,c("lon","lat")],proj4string=CRS(projection(hh_shape))),
  as(hh_shape,"SpatialPolygons")
)

# transform shapefile to dataframe
hh_shape = fortify(hh_shape)

# remove islands outside from Hamburg from shapefile
hh_shape = filter(hh_shape, long > 9.7 & long < 10.4 & lat > 53.35 & lat < 53.75)
mydata = mydata[!is.na(inout),]

# extract month from date
mydata$month =  format(mydata$activation_date, "%b %y") 

# add sequence for plot function
mydata = transform(mydata, month.num=match(month, unique(month)))

# plot sensors on map (one map for every month)
p.progress = function(i=min(mydata$month.num),maxi = max(mydata$month.num)){
ggplot() + 
  geom_polygon(data = hh_shape, 
               aes(x = long, y = lat, group = group),
               color = 'white', fill = '#ededed') +
  geom_point(data = mydata[mydata$month.num < i & !is.na(mydata$lat), ], aes(x = lon, y = lat), color =  "#4e4d47") +
  geom_point(data = mydata[mydata$month.num == i & !is.na(mydata$lat), ], aes(x = lon, y = lat), color =  "#00b702") +
  theme_void() +
  theme(text=element_text(size=20, color =  "#00b702", family = "Tw Cen MT")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = paste0("FINE DUST SENSORS IN HAMBURG: ",max(mydata$count[mydata$month.num == i & !is.na(mydata$lat)]),sep = ""), subtitle = "Airrohr ~ Citizen Science ~ Luftdaten.info") +
  theme(plot.subtitle = element_text(color = "#4e4d47", hjust = 0.5)) +
  coord_fixed(ratio = 1.5/1)
}

# group data for barplot
mydata_line = mydata %>% group_by(month) %>% summarise(count = max(count))

# order by month
mydata_line = mydata_line[order(mydata_line$count),] 

# add sequence for plot function
mydata_line$month.num = seq(1:nrow(mydata_line))

# change order of factors for plot
mydata_line$month = factor(mydata_line$month, levels = mydata_line$month[order(mydata_line$count)])

# line break in label for plot
levels(mydata_line$month) = gsub(" ", "\n", levels(mydata_line$month))

# barplot (one plot for every month)
g.progress= function(i=1,maxi = max(mydata_line$month.num)){
ggplot(mydata_line[mydata_line$month.num <= i,], aes(x = month, y = count)) +
  geom_bar(fill =  "#4e4d47", stat="identity") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major=element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="#4e4d47"),
        axis.line.y = element_line(color="#4e4d47")) +
  xlab("") +
  coord_cartesian(ylim=c(1, 164), xlim = c(1, 17)) +
  theme(plot.title = element_text(hjust=0.5, size=14, face="bold"),
        plot.caption = element_text(hjust=0.5, size = 9, color = "#4e4d47")) +
  theme(text=element_text(family = "Tw Cen MT"),
        axis.text.x = element_text(color = "#4e4d47", size = 10),
        axis.text.y = element_text(color = "#4e4d47", size = 10)) +
  theme(plot.margin=unit(c(0,2,0,1),"cm")) +
  labs(caption = "The data is available at archive.luftdaten.info. The graphic shows SDS011 sensors which went live before 27.03.2018.")
}

# use plot_grid to combine map and barplot
plotf = function(i=1){plot_grid(p.progress(i),g.progress(i), rel_heights=c(6,2),ncol=1)}

# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/akruse/Documents/gif", "/plot_",i ,".png")
  ggsave(filename=file_path, plotf(i))

}

# save plot
map(1:17, plot.save)
