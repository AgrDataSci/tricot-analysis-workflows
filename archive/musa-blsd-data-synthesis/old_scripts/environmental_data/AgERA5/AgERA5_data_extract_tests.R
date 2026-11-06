library(ncdf4)

jan_1990_temp_filename<-"D:/environmental_data/AgERA5/temp/1990/Temperature-Air-2m-Max-24h_C3S-glob-agric_AgERA5_daily_19900101-19900131_final-v1.0.nc"

nc <-nc_open(jan_1990_temp_filename)

nc_close(nc)

print(nc)

temp_jan_1990<-ncvar_get(nc,"Temperature_Air_2m_Max_24h")

lon <- ncvar_get(nc,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nc,"lat")
nlat <- dim(lat)
head(lat)

range(lat)
c(01:12)

time <- ncvar_get(nc,"time")
head(time)



temp_jan_1990[[1000]]


#temp_20_jan<-
  temp_jan_1990[100,1530,3]
#temp_20_jan


library(stars)
jan_1990_temp<-read_ncdf(jan_1990_temp_filename)
jan_1990_temp$Temperature_Air_2m_Max_24h[]

library(raster)
extract(jan_1990_temp_filename,)


coordinates(temp_jan_1990[1,1,])


temp_20_jan_r<-brick(jan_1990_temp_filename)

raster::extract(temp_20_jan_r,data.frame(-87.2333333,14.0833333))-273

temp_20_jan_r$X1990.01.01

plot(temp_20_jan_r)

temp_20_jan_r_values<-getValues(temp_20_jan_r)

system.file("nc/bcsd_obs_1999.nc", package = "stars") %>%
  read_stars("data/full_data_daily_2013.nc") -> w
## pr, tas,
w
w$pr
w$tas


test_custom_area_fn<-"d:/environmental_data/AgERA5/temp/1990/Temperature-Air-2m-Max-24h_C3S-glob-agric_AgERA5_daily_19900201-19900228_final-v1.0.nc"
test_custom_area_r<-brick(test_custom_area_fn)
test_custom_area_r$X1990.02.01
plot(test_custom_area_r[[1]])

names(test_custom_area_r[[1]])

test_custom_area<-nc_open(test_custom_area_fn)
print(test_custom_area)
lon_1<-ncvar_get(test_custom_area,"lon")
dim(lon_1)

nc_close(test_custom_area)

plot(test_custom_area_r$X2001.01.01)

extract



