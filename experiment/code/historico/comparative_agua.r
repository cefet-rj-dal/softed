library(tidyverse)

source("./soft_metric/soft_metric.r")

#================ Preparing the data =====================

path <- "./detection_data/agua/"
evt_files <- paste(path,list.files(path = path, pattern = ".Rds"),sep="")
evt_agua <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
names(evt_agua) <- stringr::str_remove(evt_files, path)
names(evt_agua) <- stringr::str_remove(names(evt_agua), ".Rds")

evt_exp <- list(agua=evt_agua)

list_evt_exp <- function(evt_exp){
  for(dataset in names(evt_exp)){
    
    methods <- unique(stringr::word(names(evt_exp[[dataset]]),1,sep = "\\_"))
    variables <- unique(stringr::str_remove_all(names(evt_exp[[dataset]]), paste(methods, collapse = "|")))
    variables <- stringr::str_remove(variables, "_")
    
    evts <- list()
    for(var in variables) {
      evts[[var]] <- sapply(methods,function(m) 
        evt_exp[[dataset]][which(
          stringr::str_detect(names(evt_exp[[dataset]]), m, negate = FALSE) &
            stringr::str_detect(names(evt_exp[[dataset]]), var, negate = FALSE)) ],
        simplify = TRUE, USE.NAMES = TRUE)
      names(evts[[var]]) <- methods
    }
    
    evt_exp[[dataset]] <- evts
  }
  
  return(evt_exp)
}



evts <- list_evt_exp(evt_exp)


#================ Preparing the table of metrics =====================

table_evt_exp <- function(evt_exp,reference){
  metric_var <- NULL
  
  for(var in names(evt_exp)) {
    
    metric_method <- NULL
    for(method in names(evt_exp[[var]])) {
      #browser()
      F1s <- tryCatch(soft_evaluate(evt_exp[[var]][[method]], reference, metric="F1"),
                      error = function(e) NA)
      F1 <- tryCatch(hard_evaluate(evt_exp[[var]][[method]], reference, metric="F1"),
                     error = function(e) NA)
      
      metrics <- cbind(round(F1,2),round(F1s,2))
      colnames(metrics) <- paste(method,c("hard","soft"),sep="_")
      
      metric_method <- cbind(metric_method,metrics)
    }
    
    metric_var <- rbind(metric_var, cbind(var=var,metric_method))
  }
  
  return(metric_var)
}

#========= Data =========
# === WATER QUALITY ===
train <- geccoIC2018Train[16500:18000,]
reference <- subset(train, select=c(Time, EVENT))
names(reference) <- c("time","event")

tbl_agua <- table_evt_exp(evts[["agua"]],reference)

write.csv(tbl_agua,"metrics_agua.csv")