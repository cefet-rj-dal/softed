source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R")
source("~/TMN_exp/myTimeseries.R")
source("~/TMN_exp/anomalies.R")


load("~/TMN_exp/prep_data/tmn.RData")


tmn_exp <- function(trend_data, method=c("AN","Kmeans","NNET","SVM","ELM","SCP","CF","EWMA","KNNCAD","GARCH"),
                    sw_size=12, input_size=6, alpha=1.5, na.action=na.omit){

  evt_det <- list()
  method <- match.arg(method)
  
  for (cnes in names(trend_data)){
    
    evt_det[[cnes]] <- list()
    
    ts_data <- trend_data[[cnes]]
    
    cat("Running method",method,"-",
        "Serie",cnes,"-",
        "Time:",toString(Sys.time()),"-",
        "WorkerId:",paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'),"\n",
        file="~/TMN_exp/log_file.txt", append=TRUE)
    
    switch(method,
           AN = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.anomaly(ts_data, ts.anomalies.an, k=sw_size, alpha=alpha),
             error=function(e) NULL)
           },
           Kmeans = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.anomaly(ts_data, ts.anomalies.kmeans, alpha=alpha),
             error=function(e) NULL)
           },
           NNET = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.anomaly(ts_data, ts.anomalies.ml, ml_model=ts_nnet, sw_size=sw_size, input_size = input_size),
             error=function(e) NULL)
           },
           SVM = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.anomaly(ts_data, ts.anomalies.ml, ml_model=ts_svm, sw_size=sw_size, input_size = input_size),
             error=function(e) NULL)
           },
           ELM = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.anomaly(ts_data, ts.anomalies.ml, ml_model=ts_elm, sw_size=sw_size, input_size = input_size),
             error=function(e) NULL)
           },
           SCP = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.seminalChangePoint(ts_data, w=sw_size, na.action=na.action),
             error=function(e) NULL)
           },
           CF = {evt_det[[cnes]][[method]] <- tryCatch({
             linreg <- function(data) {
               data <- as.data.frame(data)
               colnames(data) <- "x"
               data$t <- 1:nrow(data)
               lm(x~t, data)
             }
             evtdet.changeFinder(ts_data,mdl=linreg,m=input_size,na.action=na.action)},
             error=function(e) NULL)
           },
           EWMA = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.otsad(ts_data,method="CpPewma", n.train = sw_size, alpha0 = 0.9, beta = 0.1, l = 3),
             error=function(e) NULL)
           },
           KNNCAD = {evt_det[[cnes]][[method]] <- tryCatch(
             evtdet.otsad(ts_data,method="CpKnnCad", n.train = sw_size, threshold = 1, l = 19, k = 27, ncm.type = "LDCD", reducefp = TRUE),
             error=function(e) NULL)
           },
           GARCH = {evt_det[[cnes]][[method]] <- tryCatch({
             garch11 <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                                            distribution.model = "norm")
             evtdet.mdl_outlier(ts_data,mdl=garch,na.action=na.action,spec=garch11)},
             error=function(e) NULL)
           }
    )
    
  }

  saveRDS(evt_det, file = paste0("~/TMN_exp/results/tmn_evts_",method,".RDS"))
  
  evt_det
}


#===== Hyperparameter test ========
m <- "ELM"
test <- tmn_exp(trend, method=m, sw_size=48, input_size=6)

#Evaluate
evaluate(test[[1]][[m]], ref_evts[[1]], metric="confusion_matrix")
evaluate(test[[1]][[m]], ref_evts[[1]], metric="F1")
#Plot
print(evtplot(trend[[1]],test[[1]][[m]], ref_evts[[1]]))

#Evaluate across time series
summary_metrics <- function(det_data, ref_data, method, metric="F1", soft=TRUE){
  metrics <- sapply(names(det_data), function(s) tryCatch({
      if(soft) soft_evaluate(det_data[[s]][[method]], ref_data[[s]], metric=metric)
      else evaluate(det_data[[s]][[method]], ref_data[[s]], metric=metric)},
      error=function(e) NA)
    )
  summary(metrics)
}

summary_metrics(test, ref_evts, m, metric="F1")
summary_metrics(test, ref_evts, m, metric="precision")
summary_metrics(test, ref_evts, m, metric="recall")



#======= Running experiment ========
# Aldebaran
# cpu: 8 [0-7] detectCores()=8 cores
require(doParallel)
cluster <- makeCluster(detectCores()-1,outfile="")
registerDoParallel(cluster)


# Model tests ------------
start_time <- Sys.time()
cat(paste0("Start time: ",start_time))

#Returns a data.frame with the ranking of all experiment tests
exp_results <- tryCatch(
  foreach(method = c("AN","Kmeans","NNET","SVM","ELM","SCP","CF","EWMA","KNNCAD","GARCH"), .combine=c) %dopar% {
    
    source("~/TMN_exp/myTimeseries.R")
    source("~/TMN_exp/anomalies.R")
    
    tmn_exp(trend, method=method, sw_size=48, input_size=6)
    
  },
  error=function(e) cat("Error:",toString(e),
                        "Time of error:",toString(Sys.time()),"-",
                        "WorkerId:",paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'),"\n\n",
                        file="~/TMN_exp/log_file.txt", append=TRUE))

end_time <- Sys.time()
cat(paste0("End time: ",end_time))

cat(paste("Execution time elapsed: ",
          difftime(end_time, start_time, units="secs"), sep=""))


#--- Unregister the parallel backend
env <- foreach:::.foreachGlobals
rm(list=ls(name=env), pos=env)
rm("env")

stopCluster(cl = cluster)

# garbage collector
gc() 



#======= Combining experiment results ========
exp_results <- lapply(list.files("~/TMN_exp/results", pattern = ".RDS"), function(file) readRDS(paste0("~/TMN_exp/results/",file)))

exp_results <- do.call(mapply, args=c(exp_results, FUN=c, SIMPLIFY=FALSE))



methods <- c("AN","Kmeans","NNET","SVM","ELM","SCP","CF","EWMA","KNNCAD","GARCH")
exp_results <- lapply(exp_results,function(serie) sapply(methods, function(m) as.data.frame(ifelse(!is.null(serie[m]),serie[m],NULL)), simplify = TRUE, USE.NAMES = TRUE ))

ref_tmn <- list(TMN=ref_evts)
evts_tmn <- list(TMN=exp_results)
