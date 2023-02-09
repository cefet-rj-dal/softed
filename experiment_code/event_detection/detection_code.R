#loading ml_models
source("https://raw.githubusercontent.com/cefet-rj-dal/softed/main/experiment_code/detection_codes/myTimeseries.R")
#loading harbinger framework
source("https://raw.githubusercontent.com/cefet-rj-dal/softed/main/experiment_code/detection_codes/harbinger.r")
#loading harbinger ml methods extension
source("https://raw.githubusercontent.com/cefet-rj-dal/softed/main/experiment_code/detection_codes/anomalies.R")
library(readr)

library(tidyverse)
library(anomalize)
library(dplyr)
library(magrittr)
library(tibble)
library(otsad)

library(tensorflow)

#data.frame with two or more variables (time series) where the first variable refers to time.
#serie <- df_my_time_series

#====== tensor_cnn ======
#if version error appears, install tensorflow in python environment used for ml 
#tensorflow::install_tensorflow()
events_cnn <- evtdet.anomaly(serie, ts.anomalies.ml, ml_model=ts_tensor_cnn, sw_size=50, input_size = 5)

#====== SVM ======
events_svm <- evtdet.anomaly(serie, ts.anomalies.ml, ml_model=ts_svm, sw_size=50, input_size = 5)

#====== NNET ======
events_nnet <- evtdet.anomaly(serie, ts.anomalies.ml, ml_model=ts_nnet, sw_size=50, input_size = 10)


#---------------NA-----------------------
events_an <- evtdet.an_outliers(serie,w=50,na.action=na.omit)

#---------------GARCH---------------------
events_garch <- evtdet.garch_volatility_outlier(serie,spec=garch11,alpha=1.5,na.action=na.omit)

#---------------EWMA---------------------
events_ewma <- evtdet.otsad(serie,method="CpPewma", n.train = 50, alpha0 = 0.9, beta = 0.0, l = 3)

#---------------KNN-CAD------------------   
events_knn <- evtdet.otsad(serie,method="CpKnnCad", n.train = 50, threshold = 1, l = 19, k = 27, ncm.type = "ICAD", reducefp = TRUE)

#---------------SCP----------------------
events_scp <- evtdet.seminalChangePoint(serie, w=50,na.action=na.omit)

#---------------CF-----------------------
events_cf <- evtdet.changeFinder(serie,mdl=linreg,m=5,na.action=na.omit)

#====== K-means ======
events_means <- evtdet.anomaly(serie, ts.anomalies.kmeans, alpha=3)

#====== ELM ======
events_elm <- evtdet.anomaly(serie, ts.anomalies.ml, ml_model=ts_elm, sw_size=50, input_size = 5)

#---------------DE-----------------------
serie$timestamp <- as.POSIXct(serie$timestamp, origin = "1970-01-01")
serie <- as.tibble(serie)
events_a <- evtdet.anomalize(serie,max_anoms=0.2,na.action=na.omit)
