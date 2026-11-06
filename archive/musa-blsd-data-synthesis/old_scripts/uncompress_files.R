####################################################
#Uncrompress files from a given folder using gunzip
#David Brown 
#WUR
#
###################################################
library(R.utils)
#Modify path as required
input_path<-"F:/environmental_data/CHIRPS_0.05/2019/"

uncompress_files(input_path)

uncompress_files<-function(folder_path){

files_to_extract<-list.files(path = folder_path ,pattern = "*.gz")
files_to_extract
for (i in seq_along(files_to_extract)) {
  current_file_path<-paste0(folder_path,files_to_extract[i])
  gunzip(current_file_path)
  
}

}

