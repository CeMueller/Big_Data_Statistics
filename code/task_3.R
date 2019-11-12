##############################

"TODOs: 
- 3.1. 
- 
- 3.2.
>not yet tested with full data
>not most efficient way
>matrix() includes hard code '13' which is important to be right -->aufteilung der 5 top earner in die jeweiligen spalten.
- 3.3.
>again not sure that this is the most efficient way
- connection to database is not yet included, must be done first"

##############################  

# ======= SET UP ======= 

# load packages 
# install.packages("RSQLite")
library(RSQLite)
# install.packages("dplyr")
library(dplyr)
# install.packages("DBI")
library(DBI)
# install.packages("data.table")
library(data.table)
# install.packages("knitr")
library(knitr)
# install.packages("formattable")
library(formattable)
# install.packages("forcats")
#library(forcats)
# install.packages("kableExtra")
library(kableExtra)

#  Connect to DB
fec <- dbConnect(SQLite(), paste(getwd(),"data/fec.sqlite", sep="/"))


##############################  
##########Table 3.1###########
##############################  


# 1. Table which shows total amount of contributions from the Oil & Gas industry per year.
# Once in absolute and once in relative terms. Contributions only to political committees 
# from an individual, partnership or limited liability company.

# Define SQL query which selects the required variables, already excluding donations to Super PAC's or Hybrid PAC's
# (i.e. transaction_type NOT IN ('10', '15', '15E')), only including donations by the Oil & Gas industry 
# (i.e. contributor_category in ...), only donations to political committees (i.e. recipient_type ='C')
# as well as excluding negative donation amounts (i.e. AND amount>0)
contr.oil.gas.query <- "SELECT date, amount, contributor_category  
FROM donations
WHERE transaction_type NOT IN ('10', '15', '15E')
AND contributor_category IN(
SELECT industry_subcategory_id
FROM industrycodes
WHERE industry_category_ID IN ('E01', 'h'))
AND recipient_type ='C'
AND amount>0"


# Define SQL query which selects the required variables, already excluding donations to Super PAC's or Hybrid PAC's
# (i.e. transaction_type NOT IN ('10', '15', '15E')),  INCLUDING DONATIONS BY ALL INDUSTRIES,
# only donations to political committees (i.e. recipient_type ='C')
# as well as excluding negative donation amounts (i.e. AND amount>0)
total.contr.query <- "SELECT date, amount, contributor_category
FROM donations 
WHERE transaction_type NOT IN ('10', '15', '15E')
AND recipient_type ='C'
AND amount>0"

# Retrive Oil & Gas data from db via dbGetQuery and import efficiently into R workspace via as.data.table
contr.oil.gas.table <- as.data.table(dbGetQuery(fec, contr.oil.gas.query))
# Format date as date
contr.oil.gas.table$date <- as.Date(contr.oil.gas.table$date,"%Y-%m-%d")
# Extract year from date, which is used later to group donations by year
contr.oil.gas.table$year <- as.numeric(format(contr.oil.gas.table$date, "%Y"))
# Exclude all observations which have a date before 1990, or larger than 2014 (which exists in the data)
# Further Exclude 2014, as there are no donations by the Oil & Gas Sector.
contr.oil.gas.table <- contr.oil.gas.table[which(contr.oil.gas.table$year>=1990
                                                 & contr.oil.gas.table$year<=2013),]




# Same commands above for the total contributions table
total.contr.table <- as.data.table(dbGetQuery(fec, total.contr.query))
total.contr.table$date <- as.Date(total.contr.table$date,"%Y-%m-%d")
total.contr.table$year <- as.numeric(format(total.contr.table$date, "%Y"))
total.contr.table <- total.contr.table[which(total.contr.table$year>=1990
                                             & total.contr.table$year<=2013),]


# Calculating total contributions from Oil & Gas per year and later changing colnames
total.amount.og.year <- contr.oil.gas.table[,sum(amount), by = year]
colnames(total.amount.og.year) <- c("year", "tot_contribution_oil_gas")

# Calculating total contributions from all industries per year
total.amount.year <- total.contr.table[,sum(amount), by = year]

# Merge total amount to the the main table
total.amount.og.year <- merge(x=total.amount.og.year,y=total.amount.year,
                              by='year')

# Compute relative contribution amount by dividing Oil & Gas contribution by total contribution
total.amount.og.year$rel.amount.year <- total.amount.og.year$tot_contribution_oil_gas/
  total.amount.og.year$V1

# Delete V1 as it is of no use in the final table
total.amount.og.year$V1 <- NULL

# Change format of relative number to percent
total.amount.og.year$rel.amount.year <- percent(total.amount.og.year$rel.amount.year)

# Change format of absolute number
total.amount.og.year$tot_contribution_oil_gas <- accounting(total.amount.og.year$tot_contribution_oil_gas,
                                                            format = "d")


# Change colnames of table.3.1 and later order by Year and illustrate table through kable
colnames(total.amount.og.year) <- c("Year", "Total Contribution by Oil & Gas", "Relative Contribution by Oil & Gas")
table.3.1 <- total.amount.og.year[order(Year, decreasing = TRUE)]

# Create final table
kable(table.3.1,
      align =c("l","r","r"), row.names = FALSE) %>%
  kable_styling(bootstrap_options = c( "stripped", "hover", "condensed"), full_width = F,
                font_size =11)









##############################  
##########Table 3.2###########
############################## 


# 2.Table that shows top 5 politicians/candidates in terms of overall donations received as part of presidential 
# campaign, for each election cycle (only positive amounts)

# Define SQL query which selects the required variables, already including only contributions to the presidential
# elections with a positive amount. Furthermore, already sum up these amounts by cycle and recipient
top5.polit.query <- "SELECT distinct cycle, SUM(amount) as amount, recipient_name
FROM donations
WHERE seat = 'federal:president'
AND amount>0
GROUP BY cycle, recipient_name" 

# Retrieve from db via dbGetQuery and import efficiently into R workspace via as.data.table
top5.polit.table <- as.data.table(dbGetQuery(fec, top5.polit.query))

# Order the observations by cycle and descending total amount, furthermore only select the first 5 obs per cycle
top.5.table.help <- setorder(setDT(top5.polit.table), cycle, -amount)[, indx := seq_len(.N), by = cycle][indx <= 5]

# Define row and column names
rownames.3.2 <- as.numeric(seq(from = 1, to = 5))
colnames.3.2 <- as.numeric(seq(from = 1990, to = 2014, by = 2))

# Initialize final table.3.2
table.3.2 <- data.frame(matrix(ncol = 13, nrow = 5))
rownames(table.3.2) <- rownames.3.2  
colnames(table.3.2) <- colnames.3.2

# Fill up final table with respective values. If there is no value, the ifelse will default to 'NA'
for (i in rownames.3.2) {
  for (j in colnames.3.2) {
    k <- (j-1988)/2
    table.3.2[i,k] <-   ifelse(nrow(subset(top.5.table.help, indx == i & cycle == j, select=recipient_name))== 1,
                               subset(top.5.table.help, indx == i & cycle == j, select=recipient_name),
                               'NA')
  }
}

# Show table
kable(table.3.2,
      align =c("c","c","c","c","c","c","c","c","c","c","c")) %>% 
  kable_styling(bootstrap_options = c( "striped", "hover", "condensed"), full_width = F,
                font_size =11)  


##############################  
##########Table 3.3###########
############################## 


#3. Number of small donations (<1000) from individual contributors associated with following industries:
# Business Associations, Public Sector Unions, Industrial Unions, Non-Profit Institutions, Retired.

# Define SQL query which selects the required variables, and generate the newly required 'Industry'
# variable (i.e. CASE WHEN...). Only include donations from individuals (i.e. WHERE contributor_type 
# = 'I'),associated with the concerned industries (i.e. AND contributor_category IN(...)), and
# contribution amounts of between 0 and 1000 (i.e. AND amount<1000 and amount>0)

small.donations.query <- "SELECT date, amount,
CASE 
WHEN contributor_category IN ('G1000','G1100','G1200','G1300','G1310','G1400') then 'BUSINESS_ASSOCIATIONS'
WHEN contributor_category IN ('L1000','L1100','L1200','L1300','L1400','L1500') then 'PUBLIC_SECTOR_UNIONS'
WHEN contributor_category IN ('LC100','LC150','LE100','LE200','LM100','LM150') then 'INDUSTRIAL_UNIONS'
WHEN contributor_category IN ('X1200') then 'RETIRED'
ELSE 'NON_PROFIT_INSTITUTION'
END as 'Industry'
FROM donations
WHERE contributor_type = 'I'
AND contributor_category IN(
SELECT industry_subcategory_id
FROM industrycodes
WHERE industry_category_ID IN ('N00', 'P04', 'P02', 'W02', 'W06')) 
AND amount<1000 
AND amount>0"

# Retrieve from db via dbGetQuery and import efficiently into R workspace via as.data.table
small.donations.table.help <- as.data.table(dbGetQuery(fec, small.donations.query))

# Format date as date
small.donations.table.help$date <- as.Date(small.donations.table.help$date,"%Y-%m-%d")
# Extract year from date, which is used later to group donations by year
small.donations.table.help$year <- as.numeric(format(small.donations.table.help$date, "%Y"))


# count the number of donations per year and industry
small.donations.table <- count(small.donations.table.help, year, Industry)

# order the number of donations per year
small.donations.table <- small.donations.table[order(small.donations.table$year),]

# Exclude all observations which have a date before 1990, or larger than 2014 (which exists in the data)
# Exclude 2014 because there are barely any observations
small.donations.table <- small.donations.table[which(small.donations.table$year>=1990
                                                     & small.donations.table$year<=2013),]


# Define row and column names
rownames.3.3 <- as.numeric(seq(from = 1990, to = 2013))
colnames.3.3 <- c('BUSINESS_ASSOCIATIONS', 'PUBLIC_SECTOR_UNIONS', 'INDUSTRIAL_UNIONS',
                  'RETIRED', 'NON_PROFIT_INSTITUTION')
# Create reference df, which will be used in the loop later on
reference.3.3 <- seq(from = 1, to = 5)
ref.df.3.3 <- as.data.frame(cbind(colnames.3.3, reference.3.3))

# Initialize final table.3.3
table.3.3 <- data.frame(matrix(ncol = 5, nrow = 24))
rownames(table.3.3) <- rownames.3.3 
colnames(table.3.3) <- colnames.3.3



# Fill up final table with respective values. If there is no value, the ifelse will default to 0
for (i in rownames.3.3) {
  for (j in as.numeric(ref.df.3.3$reference.3.3)) {
    k <- (i-1989)
    table.3.3[k,j] <-   ifelse(nrow(subset(small.donations.table, year == i & Industry == as.character(ref.df.3.3[j,1]), select=n))== 1,
                               as.numeric(subset(small.donations.table, year == i & Industry == as.character(ref.df.3.3[j,1]), select=n)),
                               0)
  }
}

# Change colnames for final table
colnames(table.3.3) <- c('Business Associations', 'Public Sector Unions', 'Industrial Unions',
                         'Retired', 'Non-Profit Institutions')

# Order the table according to the years
table.3.3 <- table.3.3[ order(row.names(table.3.3), decreasing = TRUE), ]

# Add years as a vector, for illustratory purposes in the latter table
Year <- as.numeric(rownames(table.3.3))
table.3.3<- as.data.frame(cbind(Year, table.3.3))

# Format the numbers for the table and convert them into 'accounting' format
# Somehow it only works when applied seperately to each column vector instead of the whole df at once
for (i in 2:ncol(table.3.3)) {
  table.3.3[,i] <- accounting(table.3.3[,i], format = "d")
}

# Create output table
kable(table.3.3,
      align =c("l","r","r","r","r"), row.names = FALSE) %>%
  kable_styling(bootstrap_options = c( "stripped", "hover", "condensed"), full_width = F,
                font_size =11)

# Delete all files from workspace, except the ones still required
rm(list= ls()[!(ls() %in% c('fec','table.3.1', 'table.3.2', 'table.3.3'))])
