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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
# read in incumbents data subset from online .csv
incumbents <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv")
# run regression model with voteshare regressed on difflog
regression_model_problem1 <- lm(voteshare ~ difflog, data=incumbents)
# get summary of model with coefficient estimates 
summary(regression_model_problem1)

# 2.
#produce scatter plot between voteshare and difflog
pdf("Figure1)
plot(difflog, voteshare, 
     xlab = "Difference campaign spending between incumbent and challenger ( )", ylab = "Voteshare ()")


https://www.overleaf.com/learn/latex/Inserting_Images
\begin{figure}[width=0.9\textwidth]
\includegraphics[width=8cm]{Figure1.pdf}
\end{figure}

# Add the regression line to the scatterplot
abline(regression_model_problem1, col = "blue")
dev.off()
getwd()

#3. Save the residuals of the model in a seperate object.
resids1 <- regression_model_problem1$residuals


#4. Write the prediction equation. 
mean of outcome variable = 
#
dev.off()

#####################
# Problem 2
#####################

# run regression model with voteshare regressed on difflog
regression_model_problem2 <- lm(presvote ~ difflog, data=incumbents)
# get summary of model with coefficient estimates 
summary(regression_model_problem2)

# 2.
#produce scatter plot between voteshare and difflog
pdf("Figure2.pdf")
plot(incumbents$difflog, incumbents$presvote, 
     xlab = "Difference campaign spending between incumbent and challenger ( )", ylab = "Presvote ()")

# Add the regression line to the scatterplot
abline(regression_model_problem2, col = "blue")


#3. Save the residuals of the model in a seperate object.
resids2 <- regression_model_problem2$residuals


#4. Write the prediction equation. 

#
dev.off()



#####################
# Problem 3
#####################

# run regression model with voteshare regressed on difflog
regression_model_problem3 <- lm(voteshare ~ presvote, data=incumbents)
# get summary of model with coefficient estimates 
summary(regression_model_problem3)

# 2.
#produce scatter plot between voteshare and difflog
pdf(".pdf")
plot(incumbents$presvote, incumbents$voteshare, 
     xlab = "Difference campaign spending between incumbent and challenger ( )", ylab = "Voteshare ()")

# Add the regression line to the scatterplot
abline(regression_model_problem3, col = "blue")


#3. Save the residuals of the model in a seperate object.
resids3 <- regression_model_problem3$residuals


#4. Write the prediction equation. 

#
dev.off()


#####################
# Problem 4
#####################

# run regression model with voteshare regressed on difflog
regression_model_problem4 <- lm(resids1 <- regression_model_problem1$residuals
 ~ resids2 <- regression_model_problem2$residuals, data=incumbents)

regression_model_problem4 <- lm(resids1 ~ resids2 , data=incumbents)
# get summary of model with coefficient estimates 
summary(regression_model_problem4)

# 2.
#produce scatter plot between voteshare and difflog
pdf(".pdf")
plot(resids1, resids2,
     xlab = "Residuals Problem 1 ( )", ylab = "Residuals Problem 2 ()")

# Add the regression line to the scatterplot
abline(regression_model_problem4, col = "blue")


#3. Save the residuals of the model in a seperate object.
resids4 <- regression_model_problem4$residuals


#4. Write the prediction equation. 

#
dev.off()



#####################
# Problem 5
#####################

#1 Run a (thiS REGRESSION SEEMS TO BE A MULTIVARIATE REGRESSION WHILE ALL THE REST WERE BIVARIATE)regression where the outcome variable is the incumbent's voteshare and the explanatory variables are difflog and presvote.
# run a multivariate regression model with voteshare regressed on difflog and presvote
regression_model_problem5 <- lm( voteshare ~ difflog + presvote, data=incumbents)



# get summary of model with coefficient estimates 
summary(regression_model_problem5)


