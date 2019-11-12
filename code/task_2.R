##############################

"TODOs: 
  - chack: field.types and indices
  - scrapping with html_table --> remove NA row?
  - Questions"

##############################  

# ======= SET UP ======= 

# load packages 
library(data.table)
library(RSQLite)
library(DBI)
library(rvest)
library(dplyr)
library(tidyverse)

# ======= CREATE NEW DATABASE =======

# initiate the database
fec <- dbConnect(SQLite(), paste(getwd(),"data/fec.sqlite", sep="/"))

# import data into current R sesssion
donations <- fread(paste0("unzip -cq ", paste(getwd(),"data/fec.csv.zip", sep="/")))

  # scrapping transactiontypes from html page
  transaction.types.url <- read_html("https://classic.fec.gov/finance/disclosure/metadata/DataDictionaryTransactionTypeCodes.shtml")
  transaction.types.text <- transaction.types.url %>%
    html_nodes("table") %>% 
    html_table(fill = TRUE)
  transactiontypes <- as.data.frame(transaction.types.text)

industrycodes <- fread("http://assets.transparencydata.org.s3.amazonaws.com/docs/catcodes.csv")

# change headers of input tables
colnames(donations) [1] <- c("transaction_id")
colnames(donations) [5] <- c("transaction_id_fec_nimsp")
colnames(transactiontypes) <- c("transaction_type", "transaction_type_description")
colnames(industrycodes) <- c("source", "industry_subcategory_id", "industry_subcategory", "industry_category", "industry_category_id")

# add tables to database and defining field.types if possible
dbWriteTable(fec, "donations", donations, 
             field.types = c(
               transaction_id = "numeric(9,0)",
               import_reference_id = "numeric(3,0)",
               cycle = "numeric(4,0)",
               transaction_type = "varchar(3)",
               filing_id = "numeric(12,0)",
               is_amendment = "char(1)",
               amount = "numeric",
               date = "date",
               contributor_type = "char(1)",
               contributor_gender = "char(1)",
               contributor_zipcode = "numeric",
               contributor_category = "char(5)",
               recipient_ext_id = "char(9)",
               recipient_party = "char(1)",
               recipient_type = "char(1)",
               recipient_category = "char(5)",
               committee_ext_id = "char(9)",
               committee_party = "char(1)",
               candidacy_status = "char(1)",
               seat_status	= "char(1)",
               seat_result = "char(1)"))
              
dbWriteTable(fec, "transactiontypes", transaction.types, 
             field.types = c(
               transaction_type = "varchar(3)"))

dbWriteTable(fec, "industrycodes", industry.codes, 
             field.types = c(
               source = "varchar(5)", 
               industry_subcategory_id = "char(5)", 
               industry_category_id = "char(3)"))

# adding indices to database
dbExecute(fec, "CREATE INDEX transaction_id ON donations (transaction_id)")
dbExecute(fec, "CREATE INDEX industry_category ON industrycodes (industry_subcategory_id)")
dbExecute(fec, "CREATE INDEX transaction_type ON transactiontypes (transaction_type)")
