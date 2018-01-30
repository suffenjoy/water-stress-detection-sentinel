#ndvi of the field over time
path_ndvi <- "C:/Users/tangz/Box Sync/Zhehan/sentinel water stress detection"

#read the available sentinel image
wal_sen_20170503 <- read_sentinel(path_ndvi, "wal_sen_20170503_10m.tif")
wal_sen_20170510 <- read_sentinel(path_ndvi, "wal_sen_20170510_10m.tif")
wal_sen_20170523 <- read_sentinel(path_ndvi, "wal_sen_20170523_10m.tif")
wal_sen_20170619 <- read_sentinel(path_ndvi, "wal_sen_20170619_10m.tif")
wal_sen_20170702 <- read_sentinel(path_ndvi, "wal_sen_20170702_10m.tif")
wal_sen_20170709 <- read_sentinel(path_ndvi, "wal_sen_20170709_10m.tif")
wal_sen_20170717 <- read_sentinel(path_ndvi, "wal_sen_20170717_10m.tif")
wal_sen_20170722 <- read_sentinel(path_ndvi, "wal_sen_20170722_10m.tif")
wal_sen_20170724 <- read_sentinel(path_ndvi, "wal_sen_20170724_10m.tif")
wal_sen_20170729 <- read_sentinel(path_ndvi, "wal_sen_20170729_10m.tif")
wal_sen_20170811 <- read_sentinel(path_ndvi, "wal_sen_20170811_10m.tif")
wal_sen_20170813 <- read_sentinel(path_ndvi, "wal_sen_20170813_10m.tif")
wal_sen_20170818 <- read_sentinel(path_ndvi, "wal_sen_20170818_10m.tif")
wal_sen_20170826 <- read_sentinel(path_ndvi, "wal_sen_20170826_10m.tif")
wal_sen_20170831 <- read_sentinel(path_ndvi, "wal_sen_20170831_10m.tif")
wal_sen_20170922 <- read_sentinel(path_ndvi, "wal_sen_20170922_10m.tif")
wal_sen_20171017 <- read_sentinel(path_ndvi, "wal_sen_20171017_10m.tif")
wal_sen_20171106 <- read_sentinel(path_ndvi, "wal_sen_20171106_10m.tif")

#store in a big list
sen_list <- list("20170503"=wal_sen_20170503, "20170510"=wal_sen_20170510, "20170523"=wal_sen_20170523, "20170619"=wal_sen_20170619, "20170702"=wal_sen_20170702, "20170709"=wal_sen_20170709, "20170717"=wal_sen_20170717,"20170722"=wal_sen_20170722,"20170724"=wal_sen_20170724, "20170729"=wal_sen_20170729, "20170811"=wal_sen_20170811,"20170813"=wal_sen_20170813, "20170818"=wal_sen_20170818, "20170826"=wal_sen_20170826,"20170831"=wal_sen_20170831, "20170922"=wal_sen_20170922,"20171017"=wal_sen_20171017,"20171106"=wal_sen_20171106)

#ndvi function
ndvi <- function(sentinel){
  band_red <- sentinel$red 
  band_nir <- sentinel$nir
  ndvi <- (band_nir-band_red)/(band_nir+band_red)
  return(ndvi)
}

#calculate ndvi for the field
WalSen_ndvi_list <- lapply(sen_list, ndvi)

ndvi_date <- data.frame(ndvi_mean = sapply(WalSen_ndvi_list, function(x) extract(x, y=wal_field, fun=mean)), 
                        ndvi_median = sapply(WalSen_ndvi_list, function(x) extract(x, y=wal_field, fun=median)),
                        date = as.Date(names(sen_list), "%Y%m%d"))

#draw the plot
ggplot(ndvi_date, aes(date, ndvi_mean)) + geom_point(color="blue")+ geom_line(color="blue")+scale_x_date(breaks = ndvi_date$date, date_labels = "%m.%d") +ggtitle("Average NDVI of the entire field")+scale_y_continuous(limits = c(0.6,1))
ggplot(ndvi_date, aes(date, ndvi_median)) + geom_point(color="red")+ geom_line(color="red")+scale_x_date(breaks = ndvi_date$date, date_labels = "%m.%d") +ggtitle("Median NDVI of the entire field")+scale_y_continuous(limits = c(0.6,1))



