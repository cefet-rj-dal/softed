library(tibble)
library(EventDetectR)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R")


#============= Soft-Hard evaluating comparison function ================

soft_hard_comparison <- function(events, reference){
  cat("Hard:\n")
  print(hard_evaluate(events, reference, metric="confusion_matrix"))
  cat("\nSoft:\n")
  print(soft_evaluate(events, reference, metric="confusion_matrix"))
  
  Ps <- round(soft_evaluate(events, reference, metric="precision"),2)
  P <- round(hard_evaluate(events, reference, metric="precision"),2)
  
  Rs <- round(soft_evaluate(events, reference, metric="recall"),2)
  R <- round(hard_evaluate(events, reference, metric="recall"),2)
  
  F1s <- round(soft_evaluate(events, reference, metric="F1"),2)
  F1 <- round(hard_evaluate(events, reference, metric="F1"),2)
  
  metrics <- data.frame(hard=c(P,R,F1),soft=c(Ps,Rs,F1s))
  row.names(metrics) <- c("precision","recall","F1")
  
  cat("\n")
  print(metrics)
}

#====== Auxiliary Model definitions ======
ARIMA <- function(data) forecast::auto.arima(data)
AR <- function(data,p) forecast::Arima(data, order = c(p, 0, 0), seasonal = c(0, 0, 0))
garch <- function(data,spec,...) rugarch::ugarchfit(spec=spec,data=data,solver="hybrid", ...)@fit
ets <- function(data) forecast::ets(ts(data))
linreg <- function(data) {
  #browser()
  data <- as.data.frame(data)
  colnames(data) <- "x"
  data$t <- 1:nrow(data)
  
  #Adjusting a linear regression to the whole window
  lm(x~t, data)
}


#========= Data =========
# === WATER QUALITY ===
train <- geccoIC2018Train[16500:18000,]
reference <- subset(train, select=c(Time, EVENT))
names(reference) <- c("time","event")


#================= Scenarios ====================

#---------- Perfect performance ------------
#====== Outliers ======
test <- subset(train, select=c(Time, Redox))
#Detect
events <- evtdet.outliers(test, alpha=1.5)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


#---------- Almost perfect performance ------------
test <- subset(train, select=c(Time, pH))
#====== Adaptive Normalization Outliers ======
#Detect
events <- evtdet.an_outliers(test,w=100,alpha=1.5,na.action=na.omit)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


#---------- Worst performance ------------

# No time tolerance oportunities
test <- subset(train, select=c(Time, Leit))
#====== Seminal Change Point (1999) ======
#Detect
events <- optim.evtdet.seminalChangePoint(test)$events
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


# Time tolerance included
test <- subset(train, select=c(Time, pH))
#Detect
events <- reference
events$event[which(reference$event)[seq(1,length(which(reference$event)),18)]-4] <- TRUE
events$event[which(reference$event)[seq(18,length(which(reference$event)),18)]+4] <- TRUE
events$event[which(reference$event)] <- FALSE
events <- events[events$event,]
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


# Time tolerance included
test <- subset(train, select=c(Time, Redox))
#====== Seminal Change Point (1999) ======
#Detect
events <- reference
events$event[which(reference$event)+15] <- TRUE
events$event[which(reference$event)] <- FALSE
events <- events[events$event,]
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)

# Time tolerance included
test <- subset(train, select=c(Time, Redox))
#====== Seminal Change Point (1999) ======
#Detect
events <- reference
events$event[which(reference$event)+15] <- TRUE
events$event[which(reference$event)-15] <- TRUE
events$event[which(reference$event)] <- FALSE
events <- events[events$event,]
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)

# Time tolerance included
test <- subset(train, select=c(Time, Tp))
#====== Seminal Change Point (1999) ======
#Detect
events <- reference
events$event <- !events$event
events <- events[events$event,]
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


#---------- Almost all false positives ------------

# No time tolerance oportunities
test <- subset(train, select=c(Time, Leit))
#====== Seminal Change Point (1999) ======
#Detect
events <- evtdet.seminalChangePoint(test, w=50,na.action=na.omit)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)

# Time tolerance included
test <- subset(train, select=c(Time, Tp))
#====== Seminal Change Point (1999) ======
#Detect
events <- evtdet.seminalChangePoint(test, w=50,na.action=na.omit)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


#---------- Varied ------------

# No time tolerance oportunities
test <- subset(train, select=c(Time, Trueb))
#====== Seminal Change Point (1999) ======
#Detect
events <- evtdet.seminalChangePoint(test, w=50,na.action=na.omit)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


# Time tolerance included
test <- subset(train, select=c(Time, Leit))
#====== ChangeFinder (2005) ======
#Detect
events <- evtdet.changeFinder(test,mdl=linreg,m=5,na.action=na.omit)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


# Time tolerance included (big difference)
test <- subset(train, select=c(Time, Trueb))
#====== EWMA ======
#Detect
events <- evtdet.otsad(test,method="CpPewma", n.train = 50, alpha0 = 0.9, beta = 0.1, l = 3)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


# Time tolerance included (big difference)
test <- subset(train, select=c(Time, Trueb))
#====== Model Outliers - GARCH ======
#Garch specs
garch11 <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                               mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                               distribution.model = "norm")
#Detect
events <- evtdet.mdl_outlier(test,mdl=garch,na.action=na.omit,spec=garch11)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)


# Time tolerance included (NA to metric)
test <- subset(train, select=c(Time, Tp))
#====== OTSAD KNN ======
#Detect
events <- evtdet.otsad(test,method="CpKnnCad", n.train = 50, threshold = 1, l = 19, k = 27, ncm.type = "LDCD", reducefp = TRUE)
#Plot
print(evtplot(test,events, reference))
#Evaluate
soft_hard_comparison(events, reference)







#========= Data =========
# === NONSTATIONARITY ===
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/nonstationarity_sym.r")
nonstat_ts <- nonstationarity_sym(ts.len=200,ts.mean=0,ts.var=1)
#plot(ts(nonstat_ts),type="l",xlab="time",ylab="x")

test <- data.frame(time=1:length(nonstat_ts),x=nonstat_ts)
reference <- data.frame(time=1:length(nonstat_ts),event=0)
reference[c(200,400,500,600,700,800), "event"] <- 1


#====== Adaptive Normalization Outliers ======
#Detect
events_an <- evtdet.an_outliers(test,w=20,alpha=1.5,na.action=na.omit)
#Plot
print(evtplot(test,events_an,reference))
#Evaluate
soft_hard_comparison(events_an, reference)


#====== Change point V3 (2005) ======
#Detect
#events_cf <- evtdet.changeFinder(test,mdl=ARIMA,m=5,na.action=na.omit)
events_cf <- evtdet.changeFinder(test,mdl=ets,m=6,na.action=na.omit)
#Plot
print(evtplot(test,events_cf,reference, mark.cp=TRUE))
#Evaluate
soft_hard_comparison(events_cf, reference)


#====== Garch Volatility Outliers ======
#Garch specs
garch11 <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                               mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                               distribution.model = "norm")
#Detect
events_garch <- evtdet.garch_volatility_outlier(test,spec=garch11,alpha=1.5,na.action=na.omit)
#Plot
print(evtplot(test,events_garch,reference))
#Evaluate
soft_hard_comparison(events_garch, reference)


#====== Combining detected events ======
#events_cp_v3[events_cp_v3$type=="change point",]
#Combining detections
an_v3_garch <- rbind( events_an, events_cf,  events_garch)
#Plot
print(evtplot(test,an_v3_garch,reference, mark.cp=TRUE))
#Evaluate
soft_hard_comparison(an_v3_garch, reference)