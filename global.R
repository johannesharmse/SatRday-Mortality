#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking "Run App" above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(lattice)
library(ggplot2)
library(stats)
library(ipred)
library(plyr)
#library(dplyr)
library(e1071)
library(caret)

#library(shinyBS)
library(plotly)

#Sys.setlocale("LC_ALL", "en_US")

#library(rsconnect)

#require(shiny)

# e = local({load("Data/.RData"); environment()})
# tools:::makeLazyLoadDB(e, "New")
# 
# lazyLoad("New")
# ls()

 # load("./Data/.RData")
 # rm(list=setdiff(ls(), c("training.df", "finalModel", "dat", "dat1")))
 # 
 # saveRDS(dat, "./Data/dat.rds")
 # saveRDS(dat1, "./Data/dat1.rds")
 # saveRDS(training.df, "./Data/training.df.rds")
 # saveRDS(finalModel, "./Data/finalModel.rds")
 # 
 # write.csv(dat, file = "./Data/dat.csv", row.names = FALSE)
 # write.csv(dat1, file = "./Data/dat1.csv", row.names = FALSE)
 # write.csv(training.df, file = "./Data/training.df.csv", row.names = FALSE)

# toLocalEncoding <-
#   function(x, sep="\t", encoding="utf-8")
#   {
#     rawtsv <- tempfile()
#     write.table(x, file=rawtsv, sep=sep)
#     result <- read.table(file(rawtsv, encoding=encoding), sep=sep)
#     unlink(rawtsv)
#     result
#   }

#dat = toLocalEncoding(readRDS("Data/dat.rds"))
dat = read.table("www/dat.csv", header = TRUE, sep = ",")
#dat = toLocalEncoding(read.table("Data/dat.csv", header = TRUE, sep = ","))
dat[ , 1] = make.names(dat[ , 1])
# enc2native(dat)
# Encoding(dat) = "CP950"
# dat = iconv(dat, "CP950", "UTF-8")

#dat1 = readRDS("Data/dat1.rds")
dat1 = read.table("www/dat1.csv", header = TRUE, sep = ",")
#dat1 = toLocalEncoding(read.table("Data/dat1.csv", header = TRUE, sep = ","))
dat1$Col1 = as.character(dat1$Col1)
dat1[ , 1] = make.names(dat1[ , 1])
#dat1 = toLocalEncoding(dat1)
#dat1 = make.names(dat1)
# enc2native(dat1)
# Encoding(dat1) = "CP950"
# dat1 = iconv(dat1, "CP950", "UTF-8")

#training.df = toLocalEncoding(readRDS("Data/training.df.rds"))
training.df = read.table("www/training.df.csv", header = TRUE, sep = ",")
#training.df = toLocalEncoding(read.table("Data/training.df.csv", header = TRUE, sep = ","))


dat[nrow(dat) + 1, ] = NA
dat1[nrow(dat1) + 1,  ] = NA
training.df[nrow(training.df) + 1, ] = NA

#training.df[[25]] = ""

# enc2native(training.df)
# Encoding(training.df) = "CP950"
# dat = iconv(training.df, "CP950", "UTF-8")

finalModel = readRDS("www/finalModel.rds")
# enc2native(finalModel)
# Encoding(finalModel) = "CP950"
# dat = iconv(finalModel, "CP950", "UTF-8")
