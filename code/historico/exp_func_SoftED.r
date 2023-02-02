library(tidyverse)

source("./soft_metric/soft_metric.r")

#================ Preparing the data =====================
get_dataset <- function(path_folder,dataset_names){
  data <- list()
  for(dataset in dataset_names){
    #browser()
    path <- paste(path_folder,dataset,"/",sep="",collapse="")
    evt_files <- paste(path,list.files(path = path, pattern = ".Rds", recursive = TRUE),sep="")
    evt_dataset <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
    names(evt_dataset) <- stringr::str_remove(evt_files, path)
    names(evt_dataset) <- stringr::str_remove(names(evt_dataset), ".Rds")
    
    data[[dataset]] <- evt_dataset
  }
  return(data)
}

list_evt_exp <- function(evt_exp){
  for(dataset in names(evt_exp)){
    #browser()
    methods <- unique(stringr::word(names(evt_exp[[dataset]]),1,sep = "\\/"))
    variables <- unique(stringr::str_remove_all(names(evt_exp[[dataset]]), paste(methods, collapse = "|")))
    variables <- stringr::str_remove(variables, "/")
    
    evts <- list()
    for(var in variables) {
      evts[[var]] <- lapply(methods,function(m)
        tryCatch(evt_exp[[dataset]][[which(
          stringr::str_detect(names(evt_exp[[dataset]]), m, negate = FALSE) &
            stringr::str_detect(names(evt_exp[[dataset]]), paste(var,"\\b",sep=""), negate = FALSE)) ]],
          error = function(e) data.frame())
      )
      names(evts[[var]]) <- methods
    }
    
    evt_exp[[dataset]] <- evts
  }
  
  return(evt_exp)
}

list_evt_exp_3w <- function(evt_exp){
  for(dataset in names(evt_exp)){
    #browser()
    wells <- unique(stringr::word(names(evt_exp[[dataset]]),1,sep = "\\/"))
    methods <- unique(stringr::word(names(evt_exp[[dataset]]),2,sep = "\\/"))
    variables <- unique(stringr::word(names(evt_exp[[dataset]]),3,sep = "\\/"))
    
    evts <- list()
    for(well in wells){
      evts[[well]] <- list()
      for(var in variables) {
        evts[[well]][[var]] <- lapply(methods,function(m)
          tryCatch(evt_exp[[dataset]][[which(
            stringr::str_detect(names(evt_exp[[dataset]]), paste(well,"\\b",sep=""), negate = FALSE) &
              stringr::str_detect(names(evt_exp[[dataset]]), paste(m,"\\b",sep=""), negate = FALSE) &
              stringr::str_detect(names(evt_exp[[dataset]]), paste(var,"\\b",sep=""), negate = FALSE)) ]],
            error = function(e) data.frame())
        )
        names(evts[[well]][[var]]) <- methods
      }
    }
    
    evt_exp[[dataset]] <- evts
  }
  
  return(evt_exp)
}



#================ Preparing the reference data =====================
get_reference <- function(path_folder,dataset_names,evts){
  data <- list()
  for(dataset in dataset_names){
    #browser()
    path <- paste(path_folder,"gabarito_",dataset,"/",sep="",collapse="")
    evt_files <- paste(path,list.files(path = path, pattern = ".Rds", recursive = TRUE),sep="")
    evt_dataset <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
    names(evt_dataset) <- stringr::str_remove(evt_files, path)
    names(evt_dataset) <- stringr::str_remove(names(evt_dataset), ".Rds")
    names(evt_dataset) <- names(evts[[dataset]])
    
    data[[dataset]] <- evt_dataset
  }
  return(data)
}

get_reference_3w <- function(path_folder,dataset_names){
  data <- list()
  for(dataset in dataset_names){
    #browser()
    path <- paste(path_folder,"gabarito_",dataset,"/",sep="",collapse="")
    evt_files <- paste(path,list.files(path = path, pattern = ".Rds", recursive = TRUE),sep="")
    evt_dataset <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
    names(evt_dataset) <- stringr::str_remove(evt_files, path)
    names(evt_dataset) <- stringr::str_remove(names(evt_dataset), ".Rds")
    names(evt_dataset) <- stringr::str_remove(names(evt_dataset), "gabarito_")
    
    data[[dataset]] <- evt_dataset
  }
  return(data)
}


#================ Preparing the table of metrics =====================
table_evt_exp <- function(evt_exp,reference, k=15, metric="F1"){
  metric_var_df <- NULL
  metric_var <- list()
  
  for(var in names(evt_exp)) {
    
    hard_metric_method <- NULL
    for(method in names(evt_exp[[var]])) {
      #browser()
      F1 <- tryCatch(hard_evaluate(evt_exp[[var]][[method]], reference[[var]], metric=metric),
                     error = function(e) NA)
      
      metrics <- data.frame(round(F1,2))
      colnames(metrics) <- method
      
      if(is.null(hard_metric_method)) hard_metric_method <- metrics
      else hard_metric_method <- cbind(hard_metric_method,metrics)
    }
    
    soft_metric_method <- NULL
    for(method in names(evt_exp[[var]])) {
      #browser()
      F1s <- tryCatch(soft_evaluate(evt_exp[[var]][[method]], reference[[var]], k=k, metric=metric),
                      error = function(e) NA)
      
      metrics <- data.frame(round(F1s,2))
      colnames(metrics) <- method
      
      if(is.null(soft_metric_method)) soft_metric_method <- metrics
      else soft_metric_method <- cbind(soft_metric_method,metrics)
    }
    
    metric_var[[var]] <- list()
    metric_var[[var]][["hard"]] <- cbind(var=var,hard_metric_method)
    metric_var[[var]][["soft"]] <- cbind(var=var,soft_metric_method)
    
    colnames(hard_metric_method) <- paste(colnames(hard_metric_method),"hard",sep="_")
    colnames(soft_metric_method) <- paste(colnames(soft_metric_method),"soft",sep="_")
    
    metric_var_df <- rbind(metric_var_df, cbind(var=var,hard_metric_method,soft_metric_method))
  }
  
  return(list(metric_var=metric_var,metric_var_df=metric_var_df))
}

table_evt_exp_3w <- function(evt_exp,reference, k=15, metric="F1"){
  metrics_df <- NULL
  metrics_lst <- list()
  
  for(well in names(evt_exp)){
    
    metric_well_df <- NULL
    metric_well <- list()
    for(var in names(evt_exp[[well]])) {
      
      hard_metric_method <- NULL
      for(method in names(evt_exp[[well]][[var]])) {
        #browser()
        F1 <- tryCatch(hard_evaluate(evt_exp[[well]][[var]][[method]], reference[[well]], metric=metric),
                       error = function(e) NA)
        
        metrics <- data.frame(round(F1,2))
        colnames(metrics) <- method
        
        if(is.null(hard_metric_method)) hard_metric_method <- metrics
        else hard_metric_method <- cbind(hard_metric_method,metrics)
      }
      
      soft_metric_method <- NULL
      for(method in names(evt_exp[[well]][[var]])) {
        #browser()
        F1s <- tryCatch(soft_evaluate(evt_exp[[well]][[var]][[method]], reference[[well]], k=k, metric=metric),
                        error = function(e) NA)
        
        metrics <- data.frame(round(F1s,2))
        colnames(metrics) <- method
        
        if(is.null(soft_metric_method)) soft_metric_method <- metrics
        else soft_metric_method <- cbind(soft_metric_method,metrics)
      }
      
      metric_well[[var]] <- list()
      metric_well[[var]][["hard"]] <- cbind(var=var,hard_metric_method)
      metric_well[[var]][["soft"]] <- cbind(var=var,soft_metric_method)
      
      colnames(hard_metric_method) <- paste(colnames(hard_metric_method),"hard",sep="_")
      colnames(soft_metric_method) <- paste(colnames(soft_metric_method),"soft",sep="_")
      
      metric_well_df <- rbind(metric_well_df, cbind(var=var,hard_metric_method,soft_metric_method))
    }
    
    metrics_lst[[well]] <- metric_well
    
    metrics_df <- rbind(metrics_df, cbind(well=well, metric_well_df))
  }
  
  return(list(metric_well=metrics_lst,metric_well_df=metrics_df))
}


#========= Data =========
data_results <- function(evts,ref, k=15, metric="F1"){
  data_results <- list()
  for(dataset in names(evts))
    data_results[[dataset]] <- table_evt_exp(evts[[dataset]],ref[[dataset]], k=k, metric=metric)
  
  return(data_results)
}

data_results_3w <- function(evts,ref, k=15, metric="F1"){
  data_results <- list()
  for(dataset in names(evts))
    data_results[[dataset]] <- table_evt_exp_3w(evts[[dataset]],ref[[dataset]], k=k, metric=metric)
  
  return(data_results)
}

k_results <- function(evts,ref, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall")){
  k_results <- list()
  for(k in k_values){
    metric_results <- list()
    for(m in metrics){
      metric_results[[m]] <- data_results(evts,ref, k=k, metric=m)
    }
    k_results[[as.character(k)]] <- metric_results
  }
  return(k_results)
}

k_results_3w <- function(evts,ref, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall")){
  k_results <- list()
  for(k in k_values){
    metric_results <- list()
    for(m in metrics){
      metric_results[[m]] <- data_results_3w(evts,ref, k=k, metric=m)
    }
    k_results[[as.character(k)]] <- metric_results
  }
  return(k_results)
}

#========= Result Analysis =========
#NA: method could not run
#NaN: no true positives

evt_exp_analysis <- function(data_results){
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft F1s; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & is.nan(unlist(serie$hard[-1]))] )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1][is.nan(unlist(serie$soft[-1])) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))])) 
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change>0]
    } )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      !(names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5])
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change==0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change<0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  
  #Boxplot of the differences between Soft F1 and Hard F1
  diff_plot <- reshape2::melt(diff_data)
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  diffNaN_plot <- reshape2::melt(diffNaN)
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  diffTop_plot <- reshape2::melt(diffTop)
  
  percH0 <- cbind(group="percH0",reshape2::melt(percH0_diff))
  percE0 <- cbind(group="percE0",reshape2::melt(percE0_diff))
  percNaN2 <- cbind(group="percNaN2",reshape2::melt(percNaN2_diff))
  percNaNH <- cbind(group="percNaNH",reshape2::melt(percNaNH_diff))
  perc_diff <- rbind(percH0,percE0,percNaNH,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percE0","percNaNH","percNaN2"))
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top))
}

evt_exp_analysis_pr <- function(data_results){
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft precision/recall; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & serie$hard[-1]==0] )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1][is.nan(unlist(serie$soft[-1])) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))])) 
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$soft[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change>0]
    } )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      !(names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5])
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change==0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change<0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  
  #Boxplot of the differences between Soft F1 and Hard F1
  diff_plot <- reshape2::melt(diff_data)
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  diffNaN_plot <- reshape2::melt(diffNaN)
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  diffTop_plot <- reshape2::melt(diffTop)
  
  percH0 <- cbind(group="percH0",reshape2::melt(percH0_diff))
  percE0 <- cbind(group="percE0",reshape2::melt(percE0_diff))
  percNaN2 <- cbind(group="percNaN2",reshape2::melt(percNaN2_diff))
  percNaNH <- cbind(group="percNaNH",reshape2::melt(percNaNH_diff))
  perc_diff <- rbind(percH0,percE0,percNaNH,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percE0","percNaNH","percNaN2"))
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top))
}


evt_exp_analysis_3w <- function(data_results){
  #Number NAs: method could not run
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft F1s; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & is.nan(unlist(serie$hard[-1]))] ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1][is.nan(unlist(serie$soft[-1])) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change>0]
    } ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      !(names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5])
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change==0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change<0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  
  #Boxplot of the differences between Soft F1 and Hard F1
  diff_plot <- reshape2::melt(diff_data)
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  diffNaN_plot <- reshape2::melt(diffNaN)
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  diffTop_plot <- reshape2::melt(diffTop)
  
  
  percH0 <- cbind(group="percH0",reshape2::melt(percH0_diff))
  percE0 <- cbind(group="percE0",reshape2::melt(percE0_diff))
  percNaN2 <- cbind(group="percNaN2",reshape2::melt(percNaN2_diff))
  percNaNH <- cbind(group="percNaNH",reshape2::melt(percNaNH_diff))
  perc_diff <- rbind(percH0,percE0,percNaNH,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percE0","percNaNH","percNaN2"))
  
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top))
}

evt_exp_analysis_pr_3w <- function(data_results){
  #Number NAs: method could not run
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft precision/recall; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & serie$hard[-1]==0] ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1][is.nan(unlist(serie$soft[-1])) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change>0]
    } ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      !(names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5])
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      names(rank_soft)[1:5][top5_change>0] %in% names(rank_hard)[1:5]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change==0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_soft <- serie$soft[-1][order(t(serie$soft[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top5_change <- sapply(names(rank_soft), function(method) match(method,names(rank_hard))-match(method,names(rank_soft)) )[1:5]
      top5_change[top5_change<0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$soft[-1]-serie$hard[-1])[1:5] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  
  #Boxplot of the differences between Soft F1 and Hard F1
  diff_plot <- reshape2::melt(diff_data)
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  diffNaN_plot <- reshape2::melt(diffNaN)
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  diffTop_plot <- reshape2::melt(diffTop)
  
  
  percH0 <- cbind(group="percH0",reshape2::melt(percH0_diff))
  percE0 <- cbind(group="percE0",reshape2::melt(percE0_diff))
  percNaN2 <- cbind(group="percNaN2",reshape2::melt(percNaN2_diff))
  percNaNH <- cbind(group="percNaNH",reshape2::melt(percNaNH_diff))
  perc_diff <- rbind(percH0,percE0,percNaNH,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percE0","percNaNH","percNaN2"))
  
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top))
}


all_evt_exp_analysis <- function(results){
  
  k_results <- list()
  for(k in names(results)){
    k_results[[k]] <- list(F1=evt_exp_analysis(results[[k]]$F1),
                           precision=evt_exp_analysis_pr(results[[k]]$precision),
                           recall=evt_exp_analysis_pr(results[[k]]$recall))
  }
  return(k_results)
}

all_evt_exp_analysis_3w <- function(results){
  k_results <- list()
  for(k in names(results)){
    k_results[[k]] <- list(F1=evt_exp_analysis_3w(results[[k]]$F1),
                           precision=evt_exp_analysis_pr_3w(results[[k]]$precision),
                           recall=evt_exp_analysis_pr_3w(results[[k]]$recall))
  }
  return(k_results)
}


#================ Plots =====================

plot_evt_exp_analysis <- function(evt_exp_analysis,dataset_name){
  
  exp_analysis <- evt_exp_analysis$F1
  exp_analysis_p <- evt_exp_analysis$precision
  exp_analysis_r <- evt_exp_analysis$recall
  
  require(ggplot2)
  require(RColorBrewer)
  
  dataset_lab <- paste(dataset_name,"dataset",sep=" ")
  
  #Boxplot of the differences between Soft F1 and Hard F1
  p <- ggplot(exp_analysis$diff_data, aes(x=L1, y=value)) + 
    geom_boxplot(fill=brewer.pal(9,"Set1")[1])+
    theme_bw()+
    labs(x=dataset_lab, y = "F1 difference (Soft-Hard)")
  print(p)
  
  
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  p <- ggplot(exp_analysis$diffNaN, aes(x=L1, y=value)) + 
    geom_boxplot(fill=brewer.pal(9,"Set1")[3])+
    theme_bw()+
    labs(x=dataset_lab, y = "Soft F1 when Hard F1 is N/A")
  print(p)
  
  exp_analysis_pr <- rbind(cbind(Metric="precision",exp_analysis_p$diffNaN),cbind(Metric="recall",exp_analysis_r$diffNaN))
  p <- ggplot(exp_analysis_pr, aes(x=L1, y=value, fill=Metric)) + 
    geom_boxplot()+
    theme_bw()+
    labs(x=dataset_lab, y = "Soft metrics when Hard metrics are 0")+
    scale_fill_manual(values=c(brewer.pal(9,"Greens")[7],brewer.pal(9,"Greens")[4]),
                      name="Metric")
  print(p)
  
  blank_theme <- theme_bw()+
    theme(
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      axis.text.x=element_blank()
    )
  col_facets <- length(unique(exp_analysis$perc_diff$L1))/2
  
  p <- ggplot(exp_analysis$perc_diff, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y", start=0)+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x=dataset_lab, y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = 1.7), position = position_stack(vjust = 0.5))+
    scale_fill_brewer(palette="Set1",
                      name="F1 (Soft vs. Hard)",
                      labels=c("Soft > Hard","Soft = Hard","Soft >= 0, Hard is N/A","Soft and Hard are N/A"))
  print(p)
  
  
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  p <- ggplot(exp_analysis$diffTop, aes(x=value)) + 
    geom_histogram(binwidth = 1, fill="gray", color="black", alpha=0.9) +
    facet_wrap(~L1, ncol=col_facets) +
    theme_bw()+
    labs(x="Rank climb of top 5 methods based on Soft F1", y = dataset_lab)+
    scale_x_continuous(breaks = seq(0, max(exp_analysis$diffTop$value), by = 1))
  print(p)
  
  
  p <- ggplot(exp_analysis$perc_top, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y", start=0)+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x=dataset_lab, y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = 1.7), position = position_stack(vjust = 0.5))+
    scale_fill_brewer(palette="Set2",
                      name="Top 5 ranking based on Soft F1",
                      labels=c("Method climbed to the top","Method climbed within the top","Method maintened position","Method pushed down the ranking"))
  print(p)
}

plot_evt_exp_analysis_by_k <- function(evt_exp_analysis,dataset_name){
  
  exp_analysis <- list()
  for(data in names(evt_exp_analysis[[1]]$F1)){
    exp_analysis[[data]] <- NULL
    for(k in names(evt_exp_analysis)){
      exp_analysis[[data]] <- rbind(exp_analysis[[data]],cbind(k=k,evt_exp_analysis[[k]]$F1[[data]]))
    }
  }
  exp_analysis_p <- list()
  for(data in names(evt_exp_analysis[[1]]$precision)){
    exp_analysis_p[[data]] <- NULL
    for(k in names(evt_exp_analysis)){
      exp_analysis_p[[data]] <- rbind(exp_analysis_p[[data]],cbind(k=k,evt_exp_analysis[[k]]$precision[[data]]))
    }
  }
  exp_analysis_r <- list()
  for(data in names(evt_exp_analysis[[1]]$recall)){
    exp_analysis_r[[data]] <- NULL
    for(k in names(evt_exp_analysis)){
      exp_analysis_r[[data]] <- rbind(exp_analysis_r[[data]],cbind(k=k,evt_exp_analysis[[k]]$recall[[data]]))
    }
  }
  
  require(ggplot2)
  
  dataset_lab <- paste(dataset_name,"dataset",sep=" ")
  
  #Boxplot of the differences between Soft F1 and Hard F1
  p <- ggplot(exp_analysis$diff_data, aes(x=L1, y=value, fill=reorder(k, sort(as.numeric(k))))) + 
    geom_boxplot()+
    theme_bw()+
    labs(x=dataset_lab, y = "F1 difference (Soft-Hard)")+
    scale_fill_brewer(palette="Paired",name="k")
  print(p)
  
  
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  p <- ggplot(exp_analysis$diffNaN, aes(x=L1, y=value, fill=reorder(k, sort(as.numeric(k))))) + 
    geom_boxplot()+
    theme_bw()+
    labs(x=dataset_lab, y = "Soft F1 when Hard F1 is N/A")+
    scale_fill_brewer(palette="Paired",name="k")
  print(p)
  
  exp_analysis_pr <- rbind(cbind(Metric="precision",exp_analysis_p$diffNaN),cbind(Metric="recall",exp_analysis_r$diffNaN))
  p <- ggplot(exp_analysis_pr, aes(x=L1, y=value, fill=reorder(k, sort(as.numeric(k))))) + 
    geom_boxplot()+
    facet_wrap(~ Metric)+
    theme_bw()+
    labs(x=dataset_lab, y = "Soft metrics when Hard metrics are 0")+
    scale_fill_brewer(palette="Paired",name="k")
  print(p)
  
  blank_theme <- theme_bw()+
    theme(
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank()
    )
  col_facets <- length(unique(exp_analysis$perc_diff$L1))/2
  
  p <- ggplot(exp_analysis$perc_diff, aes(x=reorder(k, sort(as.numeric(k))), y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x="k", y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = k), position = position_stack(vjust = 0.5))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette="Set1",
                      name="F1 (Soft vs. Hard)",
                      labels=c("Soft > Hard","Soft = Hard","Soft >= 0, Hard is N/A","Soft and Hard are N/A"))
  print(p)
  
  
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  p <- ggplot(exp_analysis$diffTop, aes(x=value)) + 
    geom_histogram(binwidth = 1, fill="gray", color="black", alpha=0.9) +
    facet_grid(reorder(k, sort(as.numeric(k))) ~ L1)+
    theme_bw()+
    labs(x="Rank climb of top 5 methods based on Soft F1", y = dataset_lab)+
    scale_x_continuous(breaks = seq(0, max(exp_analysis$diffTop$value), by = 1))
  print(p)
  
  
  p <- ggplot(exp_analysis$perc_top, aes(x=reorder(k, sort(as.numeric(k))), y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x="k", y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = k), position = position_stack(vjust = 0.5))+
    scale_fill_brewer(palette="Set2",
                      name="Top 5 ranking based on Soft F1",
                      labels=c("Method climbed to the top","Method climbed within the top","Method maintened position","Method pushed down the ranking"))
  print(p)
}