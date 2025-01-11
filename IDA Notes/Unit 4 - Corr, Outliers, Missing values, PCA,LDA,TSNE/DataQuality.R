#R Code to create a Data Quality Report
#Author: Charles Nicholson
#Last Revision Date: January 17, 2024

#load libraries
library(tidyverse)  #to support numerous data wrangling and file ingestion functionality
library(knitr)      #to support the "kable" command to generate a nice looking table

#Let's get started!

#read in your data into an R object named dataSet
#e.g.: dataSet <- read_csv("C:/Users/nich8038/Teaching/Courses/Graduate Courses/DataSets/housingData.csv")
#e.g.: dataSet <- mtcars

dataSet <- read_csv("C:/Users/nich8038/OneDrive - University of Oklahoma/Documents/1 - Teaching/Courses/Graduate Courses/ISE-DSA 5103/Fall 2023/Assignments/Assignment 2/housingData.csv")

dataSet <- InsuranceFraud
   
#splitting the data into two distinct data sets, one is only numeric data; the other is only categorical/factor data
dataNumeric <- dataSet %>% select(where(is.numeric)) 
dataFactor <- dataSet %>% transmute(across(where(is.character),as.factor))

#take a quick look at the data sets
glimpse(dataNumeric)
glimpse(dataFactor)

#a user-defined function named Q1 that uses the quantile method, but only returns the Q1 value 
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}

#a user-defined function named Q3 that uses the quantile method, but only returns the Q3 value 
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

#a user-defined function named myNumericSummary that will compute all of the numeric statistics needed for the report
myNumericSummary <- function(x){
  c(length(x),  
    n_distinct(x), 
    sum(is.na(x)), 
    mean(x, na.rm=TRUE), 
    min(x,na.rm=TRUE),
    Q1(x,na.rm=TRUE),
    median(x,na.rm=TRUE),
    Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE),
    sd(x,na.rm=TRUE))
}


#create the numeric report
#First: call the myNumericSummary function and apply it to all of the columns in the numeric data set
#Second: add the titles of each statistic 

Y1 <- dataNumeric %>% summarize(across(everything(), myNumericSummary)) 
Y1 <- cbind(stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),Y1)

#check to make sure it is looking right...
glimpse(Y1)

#now, transpose the data frame, compute the missing pct and unique pct values, and re-arrange some columns
Y1 <- Y1 %>% 
    pivot_longer(!stat, names_to = "variable", values_to = "value") %>% 
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>%
    select(variable, n, missing, missing_pct, unique, unique_pct, everything())

#check it out again...
glimpse(Y1)

#set some options to clean it up a bit and kable() to create a table report structure
options(digits=3)
options(scipen=99)
Y1 %>% kable()

#note: you can also use kable to create HTML tables or LaTex tables, 
#kable(format='html') or kable(format='latex')


#Now for the categorical data... we need a couple more user-defined functions...

#a user-defined function named getmodes that extracts the modes of categorical data 
getmodes <- function(v,type=1) {
  
  tbl <- table(v)
  m1<-which.max(tbl)

  if (type==1) {
    return (names(m1))
  }
  else if (type==2) {
    if (length(tbl) > 1) {
        return (names(which.max(tbl[-m1])))
    }
    else {
      return (names(m1))
      }
  }
  else if (type==-1) {
    return (names(which.min(tbl)))
  }
  else {
    stop("Invalid type selected")
  }
  
}

#a user-defined function named getmodesCnt that extracts the counts associated with modes of categorical data 
getmodesCnt <- function(v,type=1) {
  
  tbl <- table(v)
  m1<-which.max(tbl)
  
  if (type==1) {
    return (max(tbl))
  }
  else if (type==2) {
    if (length(tbl) > 1) {
      return (max(tbl[-m1]))
    }
    else {
      return (max(tbl))
    }
  }
  else if (type==-1) {
    return (min(tbl))
  }
  else {
    stop("Invalid type selected")
  }
  
}

#a user-defined function named myCatSummary that will compute all of the categorical statistics needed for the report
myCatSummary <- function(x){
  
  c(length(x),  
    n_distinct(x), 
    sum(is.na(x)), 
    getmodes(x, type=1), 
    getmodesCnt(x, type=1),
    getmodes(x, type=2), 
    getmodesCnt(x, type=2),
    getmodes(x, type=-1), 
    getmodesCnt(x, type=-1))
}


#create the categorical quality report
#First: call the myCatSummary function and apply it to all of the columns in the categorical data set
#Second: add the titles of each statistic 
Y2<-dataFactor %>% summarize(across(everything(), myCatSummary))
Y2<-cbind(stat=c("n","unique","missing","1st mode","1st mode freq","2nd mode","2nd mode freq","least common","least common freq"),Y2)

#quick visual check
glimpse(Y2)

#now, transpose the data frame, compute the missing pct, unique pct values and freqRatio, and re-arrange some columns
Y2 <- Y2 %>% 
  pivot_longer(!stat, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(c("n","unique", "missing", "1st mode freq","2nd mode freq", "least common freq"),as.numeric)) %>%
  mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>%
  mutate(freqRatio = `1st mode freq`/`2nd mode freq`) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, freqRatio, everything())


#put into table form
sink("catSummary.txt")
Y2 %>% kable()
sink()


