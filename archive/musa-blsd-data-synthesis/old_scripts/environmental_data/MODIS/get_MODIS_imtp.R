library(MODISTools)


getCollection("M*D11")


VNP21A2

view(locations_imtp_bw[32:144,])

library(MODISTools)
nrow(locations_imtp_bw)
locations_imtp_3<-locations_imtp_bw[32:144,]

#
imtp_3_modis_day<-NULL
for (i in 1:nrow(locations_imtp_3)){
  
     imtp_3_modis_day[[i]]<-mt_subset(product = "MOD11A2",
                       lat = locations_imtp_3$latitude[i],
                       lon = locations_imtp_3$longitude[i],
                       band = "LST_Day_1km",
                       start = locations_imtp_3$planting_date[i],
                       end = locations_imtp_3$planting_date[i]+locations_imtp_3$max_time_planting_harvest[i],
                       progress = T)
     print(i)
}

imtp_3_modis_day
names(imtp_3_modis_day[[1]])
""
for (i in 1:length(imtp_3_modis_day)){
  write_csv(imtp_3_modis_day[[i]],paste(i,"day.csv",sep = "_"))  
}


imtp_3_modis_night<-NULL
for (i in 1:nrow(locations_imtp_3)){
  imtp_3_modis_night[[i]]<-mt_subset(product = "MOD11A2",
                              lat = locations_imtp_3$latitude[i],
                              lon = locations_imtp_3$longitude[i],
                              band = "LST_Night_1km",
                              start = locations_imtp_3$planting_date[i],
                              end = locations_imtp_3$planting_date[i]+locations_imtp_3$max_time_planting_harvest[i],
                              progress = T)
  print(i)
}

length(imtp_3_modis_night)

save(imtp_3_modis_day,file = "imtp_3_modis_day.rda")

mutate(imtp_3_modis_day,value)

save(imtp_3_modis_day,imtp_3_modis_night,file = "imtp_3_modis.rda")

for (i in 1:length(imtp_3_modis_night)){
  write_csv(imtp_3_modis_night[[i]],paste(i,"night.csv",sep = "_"))  
}

day_loc_1<-mutate(imtp_3_modis_day[[1]],value_c=ifelse(value>0,value*0.02-273.15,0))
night_loc_1<-mutate(imtp_3_modis_night[[1]],value_c=ifelse(value>0,value*0.02-273.15,0))



dim(night_loc_1$value_c)
dim(day_loc_1)

temp_loc_1<-array(c(day_loc_1$value_c,night_loc_1$value_c),dim = c(1,,1),dimnames = list(NULL,day_loc_1$calendar_date,c(1,2)))
names(temp_loc_1[,,2])<-night_loc_1$calendar_date

view(temp_loc_1)

temp_test_1<-temperature(temp_loc_1,day.one = locations_imtp_3$planting_date[1],
            span =locations_imtp_3$max_time_planting_harvest[1])

temp_test_1<-temperature(modis_approx,day.one = as.Date(names(modis_approx[1,,1][1])),
                         span =300)
view(temp_test_1)
length(modis_approx)

temp_loc_1[,,1]

row.names(temp_loc_1)<-night_loc_1$calendar_date
length(temp_loc_1)
length(dim(temp_loc_1))
if(dim(temp_loc_1)[2]==2)print("HI")

dim(modis)
str(modis)
str(temp_loc_1)
dimnames(modis)
modis[,,]
nrow(modis[50,,])

modis[1:10,1:10,1:2]

view(modis)
chirps
length(modis)
vector1 <- c(2,18,30)
vector2 <- c(10,14,17,13,11,15,22,11,33)
column.names <- c("COL1","COL2","COL3","COL4")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")
array(c(vector1,vector2),dim = c(3,4,2),dimnames = list(row.names,column.names,
                                                        matrix.names ))


library(MODIStsp)


MODIStsp()

