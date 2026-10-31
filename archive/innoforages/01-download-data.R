# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required
# load packages
library("ClimMobTools")
library("gosset")

# read file with API key indicating the key(s) and the server from 
# which the data should be retrieved. 
# Its a .txt file named .Renviron
server = "1000farms"
key = Sys.getenv("CLIMMOB_API_KEY")
projects = getProjectsCM(key, server = server)

projects

# select the projects to merge 
sel = c("NS25", "SS25")
keep = projects$project_code %in% sel

projects = projects[keep, ]

# some empty objects to fill in
jsonlt = list()

# Run across projects 
for(i in seq_along(projects$project_id)) {
  print(i)
  y = getDataCM(key, 
                project = projects$project_code[i], 
                server = projects$server[i],
                userowner = projects$user_owner[i],
                as.data.frame = FALSE)
  
  jsonlt[[i]] = y
  
}

dat = lapply(jsonlt, as.data.frame)

dat = rowbind(dat)

save(jsonlt, dat, file = "data/data.rda")




