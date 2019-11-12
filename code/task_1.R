##############################

"TODOs: 
  - finish questions
  - alternatives to codes:
     - fread?
  - set relative path / working directory"

##############################  
  
# ======= SET UP ======= 

# load packages 
library(data.table)
library(rvest)
library(httr)

setwd("/Users/PatrikMuench/Desktop/group-examination-system-time/group-examination-system-time")

# define input parameters
BASE.URL <- "http://datacommons.s3.amazonaws.com/subsets/td-20140324/contributions.fec.1990.csv.zip"
output.path <- "./data/fec.csv"
start.date <- "1990"
end.date <- "2014"

# ======= BUILD URLS ======= 

# parse base url
base.url <- gsub("1990.csv.zip", "", BASE.URL)

# build sequence of required urls 
years <- seq(from = start.date,
             to = end.date,
             by = 2)

data.urls <- paste0(base.url, years, ".csv.zip")

# ===== FETCH AND STACK CSVS =====

# download, unzip and parse all files; write them to one csv 
for (url in data.urls) {
  
  # download to temporary file and unzip
  tmp.file <- tempfile()
  download.file(url, tmp.file)
  file.path <- unzip(tmp.file, exdir = "./data")

  # parse downloaded file, write to output csv, remove tempfile
  csv.parsed <- fread(file.path[1])
  fwrite(csv.parsed,
         file = output.path,
         append = TRUE)
  unlink(tmp.file)
}

# delete redundant individual csv files and README file
sapply(paste0("./data/contributions.fec.", years, ".csv"), unlink)
sapply(paste0("./data/README", unlink)

# ===== COMPRESS CSV FILE =====

# compress csv file using zip command
zip("./data/fec.csv.zip", "./data/fec.csv")
  
# delete uncompressed csv file
sapply(paste0("./data/fec.csv"), unlink)
  
##############################
  
### Exposition of the solution
  
"Considering the multiple files which have to be downloaded, a simple for loop is set up.
First, we define a base URL, parse it to its main elements and create a sequence of years 
based on the available data sets which cover a time period of 2 years each. With that we create
a character string consisting of all URLs linking to the individual file batches. Within the for
loop, we first create a temporary file to which we download the zip file. We then define a file 
path, given by the data folder, to which we extract the zipped file. Next, we parse the unzipped 
file and append it to our previously defined csv file which gathers all individual files. The 
temporary file is deleted. As soon as all iterations of the loop are done, we delete the redundant
individual files. Finally, the resulting fec.csv file is compressed using the zip command. While
the fec.csv file takes up 7.8GB of memory, the zipped version is compressed to 1.4GB.In order to
benefit from the limited use of storage we finally delete the uncompressed csv file.

The approach of downloading individual batches has its advantages over downloading one large 
zip file. First, if only data from a smaller time period is relevant or if only a sample is 
required, one can download only selected batches of the data rather than one large file. In
addition, unzipping large files is computationally intense. The standard unzip() command in R 
e.g. only works reliable with files smaller than 4GB.
  
The purpose of unzipping and compressing the data is given by the significantly lower memory
space used to store the data. Further it reduces the transmission time when distributing or
downloading the data."
  





