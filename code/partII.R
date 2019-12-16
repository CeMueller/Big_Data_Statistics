#############################
########DESC STATS##########
############################

setwd("C:/.../")

# install.packages("ff")
library(ff)
# install.packages("ffbase")
library(ffbase)
# install.packages("ETLUtils")
library(ETLUtils)
# install.packages("doBy")
library(doBy)
# install.packages("knitr")
library(knitr)
# install.packages("kableExtra")
library(kableExtra)
# install.packages("DescTools")
library(DescTools)
# install.packages("formattable")
library(formattable)

# Selects the needed variables and creates two new modified vars in sql. The var
# seat_status_mod is required as observations where the candidate is an Incumbent
# lack the seat_status variable. The others modified vars are for aestethics. Furthermore,
# it only selects donation to house elections (i.e. where seat = 'federal:house' ),
# positive donation amounts (i.e. and amount > 0 ), donations which went directly to 
# the candidate as donations going to organizations are not traceable 
# (i.e.and recipient_type='P) as well as observations where we can track if
# the recpient has won or lost (i.e. and (seat_status='I' or seat_result <>''))
data_extract_df <-  read.dbi.ffdf(query = ("select transaction_id, amount, recipient_name,                                           case 
                                          when recipient_party='R' then 'Republican'
                                          when recipient_party='D' then 'Democrat'      
                                           else 'Other'
                                           end as recipient_party_mod,
                                          case 
                                          when seat_status='I' then 'I'
                                          else 'O'
                                          end as seat_status_mod,
                                          case 
                                           when seat_status='I' then 'Win'
                                           when seat_result='W' then 'Win'
                                           else 'Loss'
                                           end as seat_result_mod
                                          from donations 
                                          where seat = 'federal:house' 
                                          and amount > 0 
                                          and recipient_type='P' 
                                          and (seat_status='I' or seat_result <>'')"), 
                dbConnect.args = list(drv = dbConnect(SQLite(), 
                paste(getwd(),"data/fec.sqlite", sep="/")), dbname = fec), next.rows = 500000)

# Define function which determines rounded values as well as the sum in the first
# split-apply-combine command
fun1 <- function(x){c(mean = round(mean(x)) , 
                     median= round(median(x)), 
                     std.dev = round(sd(x)),
                     donation.p.cand = sum(x))}

# split-apply-combine procedure on data file chunks
donations_categ1 <- as.data.frame(ffdfdply(data_extract_df,
                          split = data_extract_df$seat_result_mod,
                          BATCHBYTES = 100000000,
                          FUN = function(x) {
                            summaryBy(amount ~ seat_result_mod + recipient_party_mod,
                                      data = x, FUN = fun1) 
                            }))
# Define second function used for determining other summary statistics with a 'group by'
fun2 <- function(x){c(count.donors = length(x) , 
                        count.candidates=length(unique(x)), 
                        mean.donation.amount = round(length(x)/length(unique(x))))}
# Run second split-apply-combine approach
donations_categ2 <- as.data.frame(ffdfdply(data_extract_df,
                             split = data_extract_df$seat_result_mod,
                             BATCHBYTES = 100000000,
                             FUN = function(x) {
                               summaryBy(recipient_name ~ seat_result_mod + recipient_party_mod,
                                         data = x, FUN = fun2) 
                             }))

# Left join donations_categ2 onto donations_categ1
donations_categ <- merge(x=donations_categ1,y=donations_categ2 ,
                         by=c('seat_result_mod','recipient_party_mod'))
# Create donations per candidate here, as it requires values from fun1 as well as fun2
donations_categ[,6] <- round(donations_categ[,6]/donations_categ[,8])
# Change row names. First Vector has to be concatenated to recipient_party_mod as else
# there will be an error due to non-unique rownames
row.name <- c(2, 2, 2, 1, 1, 1)
rownames(donations_categ) <- paste (row.name,donations_categ$recipient_party_mod,
                                    sep = ". ", collapse = NULL)
# Delete obsolete columns and reorder rows/columns
donations_categ <- donations_categ[c(4,6,5,1,3,2),c(7,8,6,9,3,4,5)]
# Change colnames
colnames(donations_categ) <- c("No. of Donors","No. of Candidates", "Avg. Donation per Candidate",
                               "Avg. No. of Donors per Candidate", "Average Donation",
                               "Median Donation", "Std. Dev. Donation" )

# Format the numbers for the table and convert them into 'accounting' format
for (i in 1:ncol(donations_categ)) {
donations_categ[,i] <- accounting(donations_categ[,i], format = "d")
}

# Create table with the descriptive statistics for winners/losers as well as Democrats,
# Republican and other candidates
kable(donations_categ,
      align =c("l","r","r","r","r","r","r")) %>%
kable_styling(bootstrap_options = c( "hover", "condensed"), full_width = F,
              font_size =11)  %>%
row_spec(1:6, color="black") %>%
group_rows("Won Election", 1, 3) %>%
group_rows("Lost Election", 4, 6) %>%
footnote(general = "US Federal Election Commission (FEC) data (1990-2014). Data was filtered such that only positive donation amounts, going directly to the House candidates were considered.",
         general_title = "Data: " )

# Delete all files from workspace, except the ones still required
rm(list= ls()[!(ls() %in% c('fec','table.3.1', 'table.3.2', 'table.3.3', 'donations_categ'))])

#############################
#########GRAPH/REG##########
############################

# Select the required data from the sql table. Same where statements as above, however
# now the data processing is already executed to some extent in SQL. First the sum of
# total donation as well as the number of donators per candidate and cycle is calculated.
# Next the seat_result is modified to include all the Incumbents who won, which have an
# empty seat_result variable. Subsequently, the log10 of tot_amount as well as no_donors
# is created, as the normal variable most probably is heavily positively skewed. Taking
# the log gets rid or at least miinimizes this skewness.
sum_data_df <- as.data.frame.ffdf(read.dbi.ffdf(query =  "
                                   select *, 
                                   log10(tot_amount) as log_tot_amount, 
                                   log10(no_donors) as log_no_donors,
                                   case 
                                   when seat_result_dum = 1 then 'Win'
                                   else 'Lose'
                                   end as seat_result
                                   from(
                                   select 
                                   distinct cycle,  
                                   recipient_name,  
                                   case 
                                   when seat_status='I' then 1
                                   when seat_result='W' then 1
                                   else 0
                                   end as seat_result_dum, 
                                   sum(amount) as tot_amount, 
                                   count(transaction_id) as no_donors
                                   from donations 
                                   where seat = 'federal:house' 
                                   and amount > 0 
                                   and recipient_type='P' 
                                   and (seat_status='I' or seat_result <>'')
                                   group by cycle, recipient_name)", 
dbConnect.args = list(drv = dbConnect(SQLite(), 
                      paste(getwd(),"data/fec.sqlite", sep="/")), dbname = fec)))

# Winsorize the two x variables for the regression at the 1% and 99% percentile
# with the aim of reducing impact of the severest outliers
sum_data_df$win_log_tot_amount <- Winsorize(sum_data_df$log_tot_amount, 
                                            probs = c(0.01, 0.99))
sum_data_df$win_log_no_donors <- Winsorize(sum_data_df$log_no_donors, 
                                           probs = c(0.01, 0.99))

# Create logit regression, which is of better us in a dummy-dependent regression as 
# compared to a normal linear regression
logitMod <- glm(seat_result_dum ~ win_log_tot_amount + win_log_no_donors, 
                data = sum_data_df, family = "binomial")
summary(logitMod)
# Include the fitted probability values from the logit regression in the df
sum_data_df$prob.win<- as.numeric(logitMod$fitted.values)

# prob>0.5 indicates that this model would predict a win
sum_data_df$predicted.results <- ifelse(sum_data_df$prob.win > 0.5,1,0) 

# Check how many results would have been correctly predicted by this simple model
# This more or less corresponds to the "in-sample fit"
sum_data_df$match <- ifelse(sum_data_df$predicted.results == sum_data_df$seat_result_dum,1,0)
prob.correct.prediction <- sum(sum_data_df$match)/length(sum_data_df$match)
prob.correct.prediction

# Create df of logit regression results, which is later used in the graph
reg.results <- as.data.frame(round(summary(logitMod)$coefficients, digits=6))
rownames(reg.results) <- c("Intercept","log10(Total Donation Amount in USD)","log10(Number of Donors)")

#Define theme for reg.results table in graph
cust.thm <- ttheme_default(
  core = list(fg_params=list(cex = 0.65)),
  colhead = list(fg_params=list(cex = 0.65)),
  rowhead = list(fg_params=list(cex = 0.65)))


library(ggplot2)
# install.packages("extrafont")
library(extrafont)
library(gridExtra)

# Create graph where we plot Number of Donors) against Total Donation Amount in USD
# on a log10 scale. If the respective candidate has won is illustrated
# with the colors. Red is equal to a loss, whereas black means that the candidate has won.
# Furthermore, add a stat_ellipse, which is a 90% normal confidence interval ellipse, for 
# illustratory purposes. In the end, add the logit regression results from before to the 
# graph. The results show that the number of donors has a positive effect on the outcome
# whereas the total amount of donations has a negative effect. This in no way implies
# causation and should be seen as such.

quartz()
donorplot <- ggplot(sum_data_df, aes(y=tot_amount, x= no_donors, colour = seat_result))
donorplot +  
  labs( x = "Number of Donors", y = "Total Donation Amount in USD", 
        colour = "Election Results", title = "US House of Representatives Election Results",
        subtitle = "Relation between number of donors and amount of donations per candidate",
        caption = "Source: US Federal Election Commission (FEC) data (1990-2014).") +
  theme(plot.title = element_text(face="bold"), legend.position = c(0.9,0.4), 
        legend.title = element_text(size=9, face="bold"), legend.key = element_rect(fill = "white"),
        text=element_text(size=11,  family="Times"),
        plot.caption = element_text(color = "gray27", face = "italic"),
        panel.background = element_blank(), panel.grid.major = element_line(colour="grey", size=0.5),
        panel.border = element_rect(colour = "gray27", fill=NA, size=0.5)) +
  geom_point(alpha=0.1) +
  scale_color_manual(values = c("red", "black")) +
  stat_ellipse(level = 0.9)  +
  scale_y_continuous(trans ='log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  scale_x_continuous(trans ='log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
annotation_custom(tableGrob(reg.results, theme=cust.thm), xmin=-0.75, xmax=6, ymin=0, ymax=1)


# Delete all files from workspace, except the ones still required
rm(list= ls()[!(ls() %in% c('fec','table.3.1', 'table.3.2', 'table.3.3', 'donations_categ',
                            'sum_data_df', 'reg.results'))])


