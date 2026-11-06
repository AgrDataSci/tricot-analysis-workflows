#use as a RStudio job

library(agera5)

#change as required
years_list <- 2000:2020


#months <- formatC(x = c(6:12), width = 2, flag = 0)



ag5_download(variable = "2m_relative_humidity",
             day = "all",
             month = "all",
             year = years_list,
             time = "15_00",
             path = "F:/env/AgERA5/rhum/"
             )







