######################################
#Code to extract AgERA5 data from locally stored files
#
#
#David Brown - WUR - ABC
#Createad 2020-05-08
#
######################################


library(raster)

#set AgERA5 data fodler
agera5_folder<-"G:/env/AgERA5"

# Available temperature statistics
#Max-24h
#Max-Day-Time
#Mean-24h
#Mean-Day-Time
#Mean-Night-Time
#Min-24h
#Min-Night-Time



list.files("F:/environmental_data/AgERA5/temp/1990")


#Function to extract each variable data for one location for one date
agera5_extract<-function(.date,.location_xy,.var,.statistic){
  file_path<-get_filepath(.var,.statistic,.date)
  agera5_brick<-brick(file_path)
  data_out<-raster::extract(agera5_brick,.location_xy)
  date_to_extract<-gsub(x=.date,pattern = "-",replacement = ".")
  date_to_extract<-paste0("X",date_to_extract)
  extracted_data<-data_out[1,date_to_extract]
  return(extracted_data)
}


extract_period<-function(.start_date,.end_date,.location_xy,.var,.statistic){
  
  time_span<-seq.Date(from = .start_date,to = .end_date, by = "days")
  data_out_period<-array(dim=c(1,length(time_span)))
  for(i in 1:length(time_span)){
    data_out_period[,i]<-agera5_extract(time_span[i],.location_xy,.var,.statistic)
  }
  colnames(data_out_period)<-as.character(time_span)
  data_out_period<-data_out_period-273.15
  return(data_out_period)
}

time_span_t<-seq.Date(from = as.Date("10/11/2001",format = "%m/%d/%Y"),to = as.Date("12/31/2002",format = "%m/%d/%Y"), by = "days")
data_out_period_t<-array(dim =c(1,length(time_span_t)))

test_data_3<-extract_period(as.Date("10/11/2001",format = "%m/%d/%Y"),as.Date("12/31/2001",format = "%m/%d/%Y"),
                            data.frame(lon=locations_imtp_blsd$longitude[i],lat=locations_imtp_blsd$latitude[i]),"temp",2)

test_data_3
median(test_data_3)
hist(test_data_3)

test_data_3-273.15


data_test<-NULL
for(i in seq(locations_imtp_blsd)){
data_test[i]<-agera5_extract(as.Date(locations_imtp_blsd$planting_date[i],format = "%m/%d/%Y"),
                               data.frame(lon=locations_imtp_blsd$longitude[i],lat=locations_imtp_blsd$latitude[i]),"temp")
}
data_test-273.15

days_to

test_data_2<-agera5_extract(as.Date(locations_imtp_blsd$planting_date[1],format = "%m/%d/%Y"),
                            data.frame(lon=locations_imtp_blsd$longitude[1],lat=locations_imtp_blsd$latitude[1]),"temp")

as.numeric(test_data_2)

test_data<-agera5_extract(as.Date(locations_imtp_blsd$planting_date[1],format = "%m/%d/%Y"),
               as.Date(locations_imtp_blsd$planting_date[1]+350,format = "%m/%d/%Y"),
               data.frame(lon=locations_imtp_blsd$longitude[1],lat=locations_imtp_blsd$latitude[1]))

date_to_extract<-gsub(x=as.Date(locations_imtp_blsd$planting_date[1],format = "%m/%d/%Y"),pattern = "-", replacement = ".")
date_to_extract<-paste0("X",date_to_extract)

test

as.Date(locations_imtp_blsd$planting_date[1],format = "%m/%d/%Y")

test_data[1,"X1995.10.02"]

get_filepath<-function(.var,.statistic,.date_to_search){
  # Available temperature statistics
  #Max-24h
  #Max-Day-Time
  #Mean-24h
  #Mean-Day-Time
  #Mean-Night-Time
  #Min-24h
  #Min-Night-Time
  if(.statistic==1)stat_code<-"Max-24h"
  if(.statistic==2)stat_code<-"Max-Day-Time"
  if(.statistic==3)stat_code<-"Mean-24h"
  if(.statistic==4)stat_code<-"Mean-Day-Time"
  if(.statistic==5)stat_code<-"Mean-Night-Time"
  if(.statistic==6)stat_code<-"Min-24h"
  if(.statistic==7)stat_code<-"Min-Night-Time"
  
  var_to_search<-.var
  
  temp_prefix<-paste0("Temperature-Air-2m-",stat_code,"_C3S-glob-agric_AgERA5_daily_")
  
  year_to_search<-format(.date_to_search, "%Y")
  
  month_to_search<-format(.date_to_search, "%m")
  
  date_pattern <- paste0(year_to_search,month_to_search)
  
  agera5_file_pattern<-paste0(temp_prefix,date_pattern)
  
  files_<-list.files(paste(agera5_folder,var_to_search,year_to_search,sep = "/"))
  print(date_pattern)
  print("__________________________")
  print(agera5_file_pattern)
  print("__________________________")
  print(files_)
  
  file_name<-files_[str_detect(files_,agera5_file_pattern)]
  print(file_name)
  agera5_file_path<-paste(agera5_folder,var_to_search,year_to_search,file_name,sep="/")  
  print(agera5_file_path)
  return(agera5_file_path)
}

get_filepath("temp",as.Date(locations_imtp_blsd$planting_date[1],format = "%m/%d/%Y"))



