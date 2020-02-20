#Bat Roost Analysis

#Stuff to do
# - how to generate the different plots wanted for the report
# make graphs look nicer
# sort StructureType graph so can see the x axis labels

library(ggplot2) #graphs
library(reshape2) #graphs
library(dplyr) #data wrangling (except percentiles)
library(lubridate) #dates and time formatting
library(magrittr) #piping
library(tidyverse)
library(plyr)
#library(PANDA)
################################################################################

### Create newmaster file from master and proforma, this will become master at
### end of analysis.

setwd("Y:/Mammal Society/Ecobat/Roost Sizes")
master <- read.csv("master.csv", header = TRUE) 

#Create separate columns for day, month and year
masteredit <- master
#tells it that date in a2 is formatted dmy not ymd
masteredit$SurveyDate <- lubridate::dmy(masteredit$SurveyDate)
masteredit2 <- masteredit %>%
  dplyr::mutate(day = lubridate::day(SurveyDate), 
                month = lubridate::month(SurveyDate), 
                year = lubridate::year(SurveyDate))
master <- masteredit2

#TASK 1: Read in proforma.csv and save it as a new row in master.csv. Will need 
#to create a new file called newmaster, will then do all analysis on this and
#at the end resave it as master to overwrite the old one so any new data is
#added to the file permanantely. 

proforma <- read.csv("proforma.csv", header = TRUE)

#Create separate columns for day, month, year
proformaedit <- proforma
proformaedit$SurveyDate <- lubridate::dmy(proformaedit$SurveyDate)
proformaedit2 <- proformaedit %>%
  dplyr::mutate(day = lubridate::day(SurveyDate), 
                month = lubridate::month(SurveyDate), 
                year = lubridate::year(SurveyDate))
proforma <- proformaedit2

#Bind the files together
newmaster <- rbind(master, proforma)

newmastercopy <- newmaster
newmastercopy$SurveyDate <- NULL
newmastercopy[newmastercopy == ""] <- NA
newmastercopy <- na.omit(newmastercopy)

#save as the new master (for the purpose of working this all out I will save it
# as master2 but once past working out stage change to save as master so will
#overwrite the original - will probably need something in the write. command
#that will say if a file with this name already exists I want to overwrite it)

# this will write as masterwrite.csv(newmaster, "./master2.csv", row.names = FALSE)

##########################################################################

#TASK 2: Replicate the graphs from the assessment example set over

#Graph 1: Natterer's all roost count by month (x-axis=month, y-axis=count)
#Subset master to have SurveyDate, Year, Month, Day and TotalCount
a <- subset(newmaster, select = c(TotalCount, StructureType, year, month, day))
#Change data with blank space to NA (does not include 0 data)
a[a == ""] <- NA
#omit rows with NAS
a1 <- na.omit(a)

plot(a1$TotalCount ~ a1$month)
boxplot(a1$TotalCount ~ a1$month)

#################################################

boxplot(a1$TotalCount)

#boxplot with ggplot 
ggplot(a1, aes(x="TotalCount", y=TotalCount))+geom_boxplot()+
  geom_point(
    data=proforma,
    aes(x="TotalCount", y=TotalCount), 
    color="red", size=5
  )

#This box plot shows outliers as black circles, median as black line, 75th
#quartile, 25th quartile, seem to be missing upper and lower max - try to
#change the quartiles it's showing and get the min and max value on it.


#try using the jitter thing to see where all the other points are? 
#boxplot showing total count, i think it's ommitting the zeros, also need to do
# a boxplot per month

####################################################################

#TASK 3: Percentiles

#What are the percentiles for the data?
quantile(a1$TotalCount)
median(a1$TotalCount)
summary(a1$TotalCount)
#French guy said they use percentiles: 25, 50, 75 and 90
#We use: 20, 40, 60, 80, therefore:
#low activity: 0-20th percentile
#low to moderate activity: 21-40th percentile
#moderate activity: 41-60th percentile
#moderate to high activity: 61-80th percentile
#high activity: 81-100th percentile
#Way taken from: 
#http://www.r-tutor.com/elementary-statistics/numerical-measures/percentile

quantile(a1$TotalCount, c(.20, .40, .60, .80,))
#20th = 7.4
#40th = 26
#60th = 42
#80th = 70

proforma$TotalCount # is 130

#Percentiles is column I think are correct percentiles because median is the
#same as them, Percentiles2 is an alternative just in case needed
#https://stats.stackexchange.com/questions/50080/estimate-quantile-of-value-in-a-vector
#https://stackoverflow.com/questions/53639037/percentile-rank-of-column-values-r
a2 <- a1 #a, a1, a2 are from newmaster which is master + proforma
a2$percentiles <- ecdf(a2$TotalCount)(a2$TotalCount)*100

Percentiles2 <- a2$TotalCount
Percentiles2.rank <- rank(Percentiles2, ties.method="min")
a2$percentiles2 <- round(Percentiles2.rank/length(Percentiles2.rank)*100)

#Pull out percentile rank of proforma row from newmaster
proforma$percentiles <- ecdf(a2$TotalCount)(proforma$TotalCount)*100

##############################################################################

### Boxplot of newmaster data with a point highlighting proforma percentile

boxplot(a2$percentiles)

#boxplot with ggplot 
ggplot(a2, aes(x="percentiles", y=percentiles))+geom_boxplot()+
  geom_point(
    data=proforma,
    aes(x="percentiles", y=percentiles), 
    color="red", size=5
  )

################################################################################
### reference range
# if make this a column then can get it to appear in text of rmarkdown report

#How to state what the reference range is e.g. output is number of rows in 
#master - for now, when select certain features it will be become the number of
#rows in the subset comparing with - as only comparing one roost at a time
#shouldn't get more complicated than this. 

structure$refrange <- nrow(structure) #works


#############################################################################
### Create boxplot graph that plots Count against Roost Type (structure)

b <- a2
b[b == ""] <- NA
b1 <- na.omit(b)

ggplot(b1, aes(x=StructureType, y=TotalCount)) + geom_boxplot()

############################################################################
#create code that will be beginning of subsetting data e.g. for a species, 
#time of year, type of roost etc. 
#Not much variation in master atm so will use StructureType but can then transfer
#to another variable when needed


#Create dataframe only with rows where StructureType is House
structure <- subset(newmastercopy, StructureType == "House")
#Could now plot proforma totalcount only against others of same StructureType
#Could then give this column new percentiles, depends when need percentiles

#can string together conditionals with & and | to create complex subsets
#create dataframe where StructureType = House, Region = South west and
#Months = 3, 4 or 5


structureregionmonth <- newmastercopy %>%
  filter(StructureType == "House") %>%
  filter(Region == "South West") %>%
  filter(month == 3 | month == 4 | month == 5) 



#structure <- subset(newmaster, select = c(TotalCount, StructureType, year, month, day))
#Change data with blank space to NA (does not include 0 data)

##########################################################################
# Create a column that lists a roost as low, low/moderate, moderate,
# moderate/high, high

# 1) find out what values 20, 40, 60, and 80 are and assign them to something

quantile(a1$TotalCount, c(.20, .40, .60, .80,))

quantile(a2$TotalCount, c(.100))

q0 <- quantile(a2$TotalCount, c(.0))
q1 <- quantile(a2$TotalCount, c(.20))
q2 <- quantile(a2$TotalCount, c(.40))
q3 <- quantile(a2$TotalCount, c(.60))
q4 <- quantile(a2$TotalCount, c(.80))


c(q1, q2, q3, q4, q5)

#So, e.g. if x is <= to q1 etc

#From Alex
#Tree.census3 <- Tree.census3 %>%
#  mutate(
   # class = case_when(
     # DBH >= 10 & DBH < 50 ~ "sapling",
     # DBH >= 50 & DBH < 100 ~ "small tree",
     # DBH >= 100 ~ "large tree",
      #TRUE ~ "unknown"
  #  )
 # )

#need to use percentiles 2 for this because if use percentiles, 0 counts are 
#classed as the 8th percentile and therefore no low cases, only low/medium


a2 <- a2%>%
  mutate(
    percentilerank = case_when(
      percentiles2 >= q0 & percentiles2 < q1 ~ "low",
      percentiles2 >= q1 & percentiles2 < q2 ~ "low/moderate",
      percentiles2 >= q2 & percentiles2 < q3 ~ "moderate",
      percentiles2 >= q3 & percentiles2 < q4 ~ "moderate/high",
      percentiles2 >= q4 ~ "high",
  
    )
  )

#Not sure if it ever gets to a point where there are enough 0s that it would
#work, perhaps because this is roost data and the assumption will be that 0
#data will not be present, can use percentiles instead of percentiles2 for the
#actual one

#check with Fiona, if she doesn't think 0s should be included, delete them first
#and then use percentiles and not percentiles2. 



count = a2$TotalCount 
quantile(count, c(.20, .40, .60, .80, na.rm = TRUE))
quantile(count, (.20))

#create a blank column
a2$rank <- ""

#stuck on this

if(a2$percentiles <= quantile(.20)) {a2$rank = "low"}
if(a2$percentiles <= quantile(.40)) {a2$rank = "low/moderate"}
if(a2$percentiles <= quantile(.60)) {a2$rank = "moderate"}
if(a2$percentiles <= quantile(.80)) {a2$rank = "moderare/high"}
if(a2$percentiles > quantile(.80)) {a2$rank = "high"} 
  }

#from Alex - does not work atm - puts all as high/5
a2$rank <- a2 %>%
  mutate(
    class = case_when(
      
      
      
      percentiles <= quantile(.20) & percentiles > 0 ~ "1",
      percentiles <= quantile(.40) & percentiles > quantile(.20) ~ "2",
      percentiles <= quantile(.60) & percentiles > quantile(.40)~ "3",
      percentiles <= quantile(.80) & percentiles > quantile(.60) ~ "4",
      percentiles <= 100 & percentiles > quantile(.80)~ "5",

      #TRUE ~ "unknown"
    )
  )

    
    if(percentiles <= quantile(count, (.40))
       a2$rank = "low/moderate")
      
      if(percentiles <= quantile(count, (.60)),
         a2$rank = "moderate")
        
        if(percentiles <= quantile(count, (.80))
           a2$rank = "moderate/highe")
          
          if(percentiles > quantile(count, (.80))
             a2$rank = "high")


}
  
#################################################

#Structure Type with percentiles, taking from Figure 4, Ecobat
## GRAPHICS ##
# get a colour-blind friendly colour palette for graphs
#colourCount <- length(unique(All_data$Spp))
getPalette <- colorRampPalette(brewer.pal(9, "RdBu"))
# set colour palette for activity levels in graphs
cbbPalette <- c("#D55E00", "#0072B2","#56B4E9","#009E73","#E69F00") 

graphbox <- ggplot(a2, aes(x=StructureType, y=Percentiles2)) + ##YOU ARE HERE ON EDITING
  geom_hline(aes(yintercept=0), linetype="dashed",
             color="#000000") +
  geom_hline(aes(yintercept=20), linetype="dashed",
             color="#0072B2") +
  geom_hline(aes(yintercept=40), linetype="dashed",
             color="#56B4E9") +
  geom_hline(aes(yintercept=60), linetype="dashed",
             color="#009E73") +
  geom_hline(aes(yintercept=80), linetype="dashed",
             color="#E69F00") +
  geom_boxplot(df, mapping = aes(fill=species))+
  xlab("\nSpecies")+
  ylab("Bat Activity Level (Percentile)\n")+
  scale_fill_grey(start = 0.35, end = 1)+
  scale_x_discrete(drop=TRUE) +
  scale_y_continuous(breaks=seq(0,100,20)) +
  expand_limits(y=c(0,100))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linetype="blank"),
        axis.line=element_line(size=0.5, colour="black",
                               linetype="solid"),
        axis.title.x=element_text(size=18),
        axis.text.x  = element_text(angle=90, face="italic",
                                    size=14, hjust=1, vjust=0),
        axis.text.y  = element_text(size=12),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.title.y=element_text(size=16),
        legend.position="none")
graphbox

  
  







###########################################
#Improving the graphs done so far

boxplot(a1$TotalCount ~ a1$month)







