library(tidyverse)

#================ Preparing the data =====================

path <- "./detection_data/A1_Yahoo/"
evt_files <- paste(path,list.files(path = path, pattern = ".Rds"),sep="")
evt_A1_Yahoo <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
names(evt_A1_Yahoo) <- stringr::str_remove(evt_files, path)
names(evt_A1_Yahoo) <- stringr::str_remove(names(evt_A1_Yahoo), ".Rds")
names(evt_A1_Yahoo) <- sapply(names(evt_A1_Yahoo), function(n) paste(substr(n,1,nchar(n)-2),substring(n, nchar(n)),sep=""))

path <- "./detection_data/A3_Yahoo/"
evt_files <- paste(path,list.files(path = path, pattern = ".Rds"),sep="")
evt_A3_Yahoo <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
names(evt_A3_Yahoo) <- stringr::str_remove(evt_files, path)
names(evt_A3_Yahoo) <- stringr::str_remove(names(evt_A3_Yahoo), ".Rds")
names(evt_A3_Yahoo) <- sapply(names(evt_A3_Yahoo), function(n) paste(substr(n,1,nchar(n)-2),substring(n, nchar(n)),sep=""))

evt_exp <- list(A1=evt_A1_Yahoo,A3=evt_A3_Yahoo)

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
      F1s <- tryCatch(soft_evaluate(evt_exp[[var]][[method]], reference[[var]], metric="F1"),
                      error = function(e) NA)
      F1 <- tryCatch(hard_evaluate(evt_exp[[var]][[method]], reference[[var]], metric="F1"),
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
# === A1_Yahoo ===
path <- "./detection_data/events_yahoo/A1/"
evt_files <- paste(path,list.files(path = path, pattern = ".Rds"),sep="")
reference_A1 <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
names(reference_A1) <- stringr::str_remove(evt_files, path)
names(reference_A1) <- stringr::str_remove(names(reference_A1), ".Rds")
names(reference_A1) <- sapply(names(reference_A1), function(n) paste(substr(n,1,nchar(n)-2),substring(n, nchar(n)),sep=""))

reference_A1 <- lapply(reference_A1, function(var) cbind(time=1:nrow(var),event=var[1]))

tbl_A1 <- table_evt_exp(evts[["A1"]],reference_A1)

write.csv(tbl_A1,"metrics_A1.csv")

# === A3_Yahoo ===
path <- "./detection_data/events_yahoo/A3/"
evt_files <- paste(path,list.files(path = path, pattern = ".Rds"),sep="")
reference_A3 <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
names(reference_A3) <- stringr::str_remove(evt_files, path)
names(reference_A3) <- stringr::str_remove(names(reference_A3), ".Rds")
names(reference_A3) <- sapply(names(reference_A3), function(n) paste(substr(n,1,nchar(n)-2),substring(n, nchar(n)),sep=""))

reference_A3 <- lapply(reference_A3, function(var) cbind(time=1:nrow(var),event=var[1]))

tbl_A3 <- table_evt_exp(evts[["A3"]],reference_A3)

write.csv(tbl_A3,"metrics_A3.csv")
