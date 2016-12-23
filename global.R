library(cranlogs)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(rdrop2)
library(lubridate)
library(RCurl)

rcpp.dwn=cran_downloads(package="Rcpp",from="2015-01-01",to="2016-11-10")
ggplot2.dwn=cran_downloads(package="ggplot2",from="2015-01-01",to="2016-11-10")
dplyr.dwn=cran_downloads(package="dplyr",from="2015-01-01",to="2016-11-10")
stringr.dwn=cran_downloads(package="stringr",from="2015-01-01",to="2016-11-10")
dat.mat=cbind(rcpp.dwn,ggplot2.dwn[2],ggplot2.dwn[3],dplyr.dwn[2],dplyr.dwn[3],stringr.dwn[2],stringr.dwn[3])
data <- dat.mat
output.table <- data

Xrcpp=rcpp.dwn$count
Xggplot2=ggplot2.dwn$count
Xdplyr=dplyr.dwn$count
Xstringr=stringr.dwn$count

lm.trend=1:length(Xrcpp)
m1=arima(Xrcpp,c(2,1,2),
         seasonal=list(order=c(1,1,1),period=7),
         xreg=lm.trend)
lm.trend=1:length(Xggplot2)
m2=arima(Xggplot2,c(1,0,1),
         seasonal=list(order=c(1,1,1),period=7),
         xreg=lm.trend)
lm.trend=1:length(Xdplyr)
m3=arima(Xdplyr,c(1,1,3),
         seasonal=list(order=c(1,1,1),period=7),
         xreg=lm.trend)
lm.trend=1:length(Xstringr)
m4=arima(Xstringr,c(1,1,2),
         seasonal=list(order=c(1,1,1),period=7),
         xreg=lm.trend)
m=30
