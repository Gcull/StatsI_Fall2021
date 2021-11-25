#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
install.packages(car)
library(car)
data("Prestige")
help(Prestige)

lapply(c("car", "texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# 

# (a) 
library(tidyverse)
Prestige <- read_csv("Data/Prestige.csv")

Prestige$type.Dummy<-ifelse(Prestige$type=="prof",1, ifelse(Prestige$type == "bc" | Prestige$type == "wc", yes = 0, no = NA))
Prestige$type.Dummy <- ifelse(Prestige$type == "WC, BC", 0)
professional=c(0, 1)
Prestige$.Dummy <- ifelse(is.element(Prestige$professional, ),"", "")

# (b) 
regression_model_1 <- lm(prestige ~ income*type.Dummy, data=Prestige)
summary(regression_model_1)
# (c) 




#####################
# Problem 2
#####################

# (a)

# (b)

# (c)
#
