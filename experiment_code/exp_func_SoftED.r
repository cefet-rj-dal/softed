library(tidyverse)

source("./softed_metrics.r")
source("./hard_metrics.r")
source("./nab_metrics.r")

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


#================ Calculating the metrics =====================
evaluate_results <- function(evts, ref,dataset_name, k_values=seq(15, 60, by = 15)){
  metric_results <- list()
  #browser()
  for(dataset in names(evts)){
    evt_exp <- list()

    cat("\n\n=== calculating metrics for dataset ",dataset," ===\n")
    
    for(var in names(evts[[dataset]])) {
      metric_var <- list()
      
      for(method in names(evts[[dataset]][[var]])) {
        cat("\ncalculating hard metrics for method:",method,"and var:",var,"\n")
        time_hard <- system.time({
          hard_metrics <- tryCatch(hard_evaluate(evts[[dataset]][[var]][[method]], ref[[dataset]], metric=NULL),
                                   error = function(e) NA)
        })
        cat(time_hard,"\n")
        cat("calculating soft metrics for method:",method,"and var:",var,"\n")
        k_results <- list()
        for(k in k_values){
          time_soft <- system.time({
            soft_metrics <- tryCatch(soft_evaluate(evts[[dataset]][[var]][[method]], ref[[dataset]], k=k, metric=NULL),
                                     error = function(e) NA)
          })
          cat("k:",k,"\n",time_hard,"\n")
          k_results[[as.character(k)]] <- list(value=soft_metrics, time=time_soft)
        }
        cat("calculating NAB metrics for method:",method,"and var:",var,"\n")
        time_nab <- system.time({
          nab_metrics <- tryCatch(nab_evaluate(evts[[dataset]][[var]][[method]], ref[[dataset]], rep(NA,nrow(ref[[dataset]])), metric=NULL),
                                  error = function(e) NA)
        })
        cat(time_nab,"\n")
        
        metric_var[[method]][["hard"]] <- list(value=hard_metrics, time=time_hard)
        metric_var[[method]][["soft"]] <- k_results
        metric_var[[method]][["nab"]] <- list(value=nab_metrics, time=time_nab)
      }
      evt_exp[[var]] <- metric_var
    }
    
    metric_results[[dataset]] <- evt_exp
    saveRDS(metric_results,paste0("SoftED_exp/results/",dataset_name,"/",dataset,".rds"))
  }
  
  return(metric_results)
}


evaluate_results_3w <- function(evts, ref, k_values=seq(15, 60, by = 15)){
  metric_results <- list()
  #browser()
  for(dataset in names(evts)){
    evt_exp_3w <- list()
    
    for(well in names(evts[[dataset]])){
      metric_well <- list()
      cat("\n\n=== calculating metrics for dataset ",dataset," and well ",well," ===\n")
      
      for(var in names(evts[[dataset]][[well]])) {
        metric_var <- list()
        
        for(method in names(evts[[dataset]][[well]][[var]])) {
          cat("\ncalculating hard metrics for method:",method,"and var:",var,"\n")
          time_hard <- system.time({
            hard_metrics <- tryCatch(hard_evaluate(evts[[dataset]][[well]][[var]][[method]], ref[[dataset]][[well]], metric=NULL),
                                     error = function(e) NA)
          })
          cat(time_hard,"\n")
          cat("calculating soft metrics for method:",method,"and var:",var,"\n")
          k_results <- list()
          for(k in k_values){
            time_soft <- system.time({
              soft_metrics <- tryCatch(soft_evaluate(evts[[dataset]][[well]][[var]][[method]], ref[[dataset]][[well]], k=k, metric=NULL),
                                       error = function(e) NA)
            })
            cat("k:",k,"\n",time_hard,"\n")
            k_results[[as.character(k)]] <- list(value=soft_metrics, time=time_soft)
          }
          cat("calculating NAB metrics for method:",method,"and var:",var,"\n")
          time_nab <- system.time({
            nab_metrics <- tryCatch(nab_evaluate(evts[[dataset]][[well]][[var]][[method]], ref[[dataset]][[well]], rep(NA,nrow(ref[[dataset]][[well]])), metric=NULL),
                                    error = function(e) NA)
          })
          cat(time_nab,"\n")
          
          metric_var[[method]][["hard"]] <- list(value=hard_metrics, time=time_hard)
          metric_var[[method]][["soft"]] <- k_results
          metric_var[[method]][["nab"]] <- list(value=nab_metrics, time=time_nab)
        }
        metric_well[[var]] <- metric_var
      }
      
      evt_exp_3w[[well]] <- metric_well
      
      saveRDS(metric_well,paste0("SoftED_exp/results/3W/",paste(dataset,well,sep = "_"),".rds"))
    }
    
    metric_results[[dataset]] <- evt_exp_3w
  }
  
  return(metric_results)
}

#================ Calculating NAB window lengths =====================
NAB_wlen <- function(ref, window.length.perc = 0.1){
  w_results <- data.frame()
  #browser()
  for(dataset in names(ref)){
    
    for(var in names(ref[[dataset]])){
      
      data.length <- nrow(ref[[dataset]][[var]])
      num.real.anomaly <- tryCatch(sum(ref[[dataset]][[var]][,2]>0,  na.rm = TRUE),
                                   error = function(e) NA)
      if(is.na(num.real.anomaly)) next
      
      if (num.real.anomaly == 0) {
        windowLength <- 0
      } else {
        windowLength <- floor((data.length * window.length.perc) / num.real.anomaly)
        if (windowLength %% 2 != 0) windowLength <- windowLength - 1
      }
      
      w_results <- rbind(w_results, cbind(dataset=dataset, serie=var, data.length=data.length, num.real.anomaly=num.real.anomaly, window.length.perc=window.length.perc, windowLength=windowLength))
    }
  }

  return(w_results)
}

NAB_wlen_3w <- function(ref, window.length.perc = 0.1){
  w_results <- data.frame()
  #browser()
  for(dataset in names(ref)){
    
    for(well in names(ref[[dataset]])){
      
      data.length <- nrow(ref[[dataset]][[well]])
      num.real.anomaly <- tryCatch(sum(ref[[dataset]][[well]][,2]>0,  na.rm = TRUE),
                                   error = function(e) NA)
      if(is.na(num.real.anomaly)) next
      
      if (num.real.anomaly == 0) {
        windowLength <- 0
      } else {
        windowLength <- floor((data.length * window.length.perc) / num.real.anomaly)
        if (windowLength %% 2 != 0) windowLength <- windowLength - 1
      }
      
      w_results <- rbind(w_results, cbind(dataset=dataset, serie=well, data.length=data.length, num.real.anomaly=num.real.anomaly, window.length.perc=window.length.perc, windowLength=windowLength))
    }
  }
  
  return(w_results)
}


#================ Preparing the table of metrics =====================
table_evt_exp <- function(eval_metrics, dataset=NULL, k=15, metric="F1", rival=c("soft","NAB")){
  metric_var_df <- NULL
  metric_var <- list()
  
  rival_metric <- match.arg(rival)
  
  for(var in names(eval_metrics)) {
    
    hard_metric_method <- NULL
    hard_metric_method_time <- NULL
    for(method in names(eval_metrics[[var]])) {
      #browser()
      metric_value <- tryCatch(eval_metrics[[var]][[method]]$hard$value[[metric]],
                     error = function(e) NA)
      
      metrics <- data.frame(round(metric_value,2))
      colnames(metrics) <- method
      
      if(is.null(hard_metric_method)) hard_metric_method <- metrics
      else hard_metric_method <- cbind(hard_metric_method,metrics)
    }
    
    rival_metric_method <- NULL
    for(method in names(eval_metrics[[var]])) {
      #browser()
      
      metric_value <- tryCatch(switch(rival_metric,
                                      "soft" = eval_metrics[[var]][[method]]$soft[[as.character(k)]]$value[[metric]],
                                      "NAB" = eval_metrics[[var]][[method]]$nab$value[[metric]]),
                      error = function(e) NA)
      
      metrics <- data.frame(round(metric_value,2))
      colnames(metrics) <- method
      
      if(is.null(rival_metric_method)) rival_metric_method <- metrics
      else rival_metric_method <- cbind(rival_metric_method,metrics)
    }
    
    metric_var[[var]] <- list()
    metric_var[[var]][["hard"]] <- cbind(var=var,hard_metric_method)
    metric_var[[var]][["rival"]] <- cbind(var=var,rival_metric_method)
    
    colnames(hard_metric_method) <- paste(colnames(hard_metric_method),"hard",sep="_")
    colnames(rival_metric_method) <- paste(colnames(rival_metric_method),"rival",sep="_")
    
    metric_var_df <- rbind(metric_var_df, cbind(var=var,hard_metric_method,rival_metric_method))
  }
  
  return_lst <- list(metric_var=metric_var,metric_var_df=metric_var_df)
  
  return(return_lst)
}

table_evt_exp_3w <- function(eval_metrics, dataset=NULL, k=15, metric="F1", rival=c("soft","NAB")){
  metrics_df <- NULL
  metrics_lst <- list()
  #browser()
  rival_metric <- match.arg(rival)
  
  for(well in names(eval_metrics)){

    metric_well_df <- NULL
    metric_well <- list()
    for(var in names(eval_metrics[[well]])) {

      hard_metric_method <- NULL
      for(method in names(eval_metrics[[well]][[var]])) {
        #browser()
        metric_value <- tryCatch(eval_metrics[[well]][[var]][[method]]$hard$value[[metric]],
                       error = function(e) NA)
        
        metrics <- data.frame(round(metric_value,2))
        colnames(metrics) <- method
        
        if(is.null(hard_metric_method)) hard_metric_method <- metrics
        else hard_metric_method <- cbind(hard_metric_method,metrics)
      }
      
      rival_metric_method <- NULL
      for(method in names(eval_metrics[[well]][[var]])) {
        
        metric_value <- tryCatch(switch(rival_metric,
                                        "soft" = eval_metrics[[well]][[var]][[method]]$soft[[as.character(k)]]$value[[metric]],
                                        "NAB" = eval_metrics[[well]][[var]][[method]]$nab$value[[metric]]),
                        error = function(e) NA)
        
        metrics <- data.frame(round(metric_value,2))
        colnames(metrics) <- method
        
        if(is.null(rival_metric_method)) rival_metric_method <- metrics
        else rival_metric_method <- cbind(rival_metric_method,metrics)
      }
      
      metric_well[[var]] <- list()
      metric_well[[var]][["hard"]] <- cbind(var=var,hard_metric_method)
      metric_well[[var]][["rival"]] <- cbind(var=var,rival_metric_method)
      
      colnames(hard_metric_method) <- paste(colnames(hard_metric_method),"hard",sep="_")
      colnames(rival_metric_method) <- paste(colnames(rival_metric_method),"rival",sep="_")
      
      metric_well_df <- rbind(metric_well_df, cbind(var=var,hard_metric_method,rival_metric_method))
    }
    
    metrics_lst[[well]] <- metric_well
    
    metrics_df <- rbind(metrics_df, cbind(well=well, metric_well_df))
  }
  
  return_lst <- list(metric_well=metrics_lst,metric_well_df=metrics_df)
  
  return(return_lst)
}


#========= Data =========
data_results <- function(evaluate_metrics, k=NULL, metrics=c("F1","precision","recall"), rival=c("soft","NAB")){
  metric_results <- list()
  for(m in metrics){
    data_results <- list()
    for(dataset in names(evaluate_metrics)){
      data_results[[dataset]] <- table_evt_exp(evaluate_metrics[[dataset]], dataset=dataset, k=k, metric=m, rival=rival)
    }
    metric_results[[m]] <- data_results
  }
  return(metric_results)
}

data_results_3w <- function(evaluate_metrics, k=NULL, metrics=c("F1","precision","recall"), rival=c("soft","NAB")){
  
  metric_results <- list()
  for(m in metrics){
    data_results <- list()
    for(dataset in names(evaluate_metrics)){
      data_results[[dataset]] <- table_evt_exp_3w(evaluate_metrics[[dataset]], dataset=dataset, k=k, metric=m, rival=rival)
    }
    metric_results[[m]] <- data_results
  }
  return(metric_results)
}

k_results <- function(evaluate_metrics, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft"){
  k_results <- list()
  for(k in k_values){
    k_results[[as.character(k)]] <- data_results(evaluate_metrics, k=k, metric=metrics, rival=rival)
  }
  return(k_results)
}

k_results_3w <- function(evaluate_metrics, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft"){
  k_results <- list()
  for(k in k_values){
    k_results[[as.character(k)]] <- data_results_3w(evaluate_metrics, k=k, metric=metrics, rival=rival)
  }
  return(k_results)
}

#========= Result Analysis =========
#NA: method could not run
#NaN: no true positives

evt_exp_analysis <- function(data_results, top=3){
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  #browser()
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft F1s; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & is.nan(unlist(serie$hard[-1]))] )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1][is.nan(unlist(serie$rival[-1])) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))])) 
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change>0]
    } )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      !(names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top])
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change==0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change<0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
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
  perc_diff <- rbind(percH0,percNaNH,percE0,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percNaNH","percE0","percNaN2"))
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  lengths_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  len_diff <- reshape2::melt(lengths_diff)
  lengths_top <- lapply(data_results, function(dataset) {
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    length(as.matrix( diff_dataset_total)) 
  }
  )
  len_top <- reshape2::melt(lengths_top)
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top, 
              len_diff=len_diff, len_top=len_top))
}

evt_exp_analysis_pr <- function(data_results, top=3){
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft precision/recall; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & serie$hard[-1]==0] )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1][is.nan(unlist(serie$rival[-1])) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))])) 
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & is.nan(unlist(serie$hard[-1]))] )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change>0]
    } )
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      !(names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top])
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change==0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change<0]
    } )
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
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
  perc_diff <- rbind(percH0,percNaNH,percE0,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percNaNH","percE0","percNaN2"))
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  lengths_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_var, function(serie) serie$rival[-1]-serie$hard[-1] )
    length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  len_diff <- reshape2::melt(lengths_diff)
  lengths_top <- lapply(data_results, function(dataset) {
    diff_dataset_total <- sapply(dataset$metric_var, function(serie) (serie$rival[-1]-serie$hard[-1])[top] )
    length(as.matrix( diff_dataset_total)) 
  }
  )
  len_top <- reshape2::melt(lengths_top)
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top, 
              len_diff=len_diff, len_top=len_top))
}


evt_exp_analysis_3w <- function(data_results, top=3){
  #Number NAs: method could not run
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft F1s; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & is.nan(unlist(serie$hard[-1]))] ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1][is.nan(unlist(serie$rival[-1])) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change>0]
    } ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      !(names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top])
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change==0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change<0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
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
  perc_diff <- rbind(percH0,percNaNH,percE0,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percNaNH","percE0","percNaN2"))
  
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  lengths_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  len_diff <- reshape2::melt(lengths_diff)
  lengths_top <- lapply(data_results, function(dataset) {
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    length(as.matrix( diff_dataset_total)) 
  }
  )
  len_top <- reshape2::melt(lengths_top)
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top, 
              len_diff=len_diff, len_top=len_top))
}

evt_exp_analysis_pr_3w <- function(data_results, top=3){
  #Number NAs: method could not run
  #NA,NA;NA,vs: vs-vh = NA (% of NA results)
  NA_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[is.na(diff_dataset) & !is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    as.numeric(as.matrix( diff_dataset[!is.na(diff_dataset)] )) 
  }
  )
  percH0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset>0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  percE0_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) & diff_dataset==0] ))/ length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  
  #NaN,NaN;NaN,vs: vs (soft precision/recall; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & serie$hard[-1]==0] ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNaN2_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1][is.nan(unlist(serie$rival[-1])) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  percNaNH_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1][!is.na(serie$rival[-1]) & is.nan(unlist(serie$hard[-1]))] ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total[!is.na(diff_dataset_total) | is.nan(unlist(diff_dataset_total))]))  
  }
  )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change>0]
    } ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  percNewTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      !(names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top])
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      names(rank_rival)[top][top_change>0] %in% names(rank_hard)[top]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    sum(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percSameTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change==0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    length(as.numeric(unlist( diff_dataset )))/length(as.matrix( diff_dataset_total)) 
  }
  )
  percLowerTop_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) {
      rank_rival <- serie$rival[-1][order(t(serie$rival[-1]),decreasing = TRUE)]
      rank_hard <- serie$hard[-1][order(t(serie$hard[-1]),decreasing = TRUE)]
      top_change <- sapply(names(rank_rival), function(method) match(method,names(rank_hard))-match(method,names(rank_rival)) )[top]
      top_change[top_change<0]
    } ))
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
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
  perc_diff <- rbind(percH0,percNaNH,percE0,percNaN2)
  
  perc_diff$group <- factor(perc_diff$group, levels = c("percH0","percNaNH","percE0","percNaN2"))
  
  
  percNewTop <- cbind(group="percNewTop",reshape2::melt(percNewTop_diff))
  percTop <- cbind(group="percTop",reshape2::melt(percTop_diff))
  percSameTop <- cbind(group="percSameTop",reshape2::melt(percSameTop_diff))
  percLowerTop <- cbind(group="percLowerTop",reshape2::melt(percLowerTop_diff))
  perc_top <- rbind(percNewTop,percTop,percSameTop,percLowerTop)
  
  perc_top$group <- factor(perc_top$group, levels = c("percNewTop","percTop","percSameTop","percLowerTop"))
  
  lengths_diff <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$rival[-1]-serie$hard[-1] ))
    length(as.matrix( diff_dataset[!is.na(diff_dataset) | is.nan(unlist(diff_dataset))])) 
  }
  )
  len_diff <- reshape2::melt(lengths_diff)
  lengths_top <- lapply(data_results, function(dataset) {
    diff_dataset_total <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) (serie$rival[-1]-serie$hard[-1])[top] ))
    length(as.matrix( diff_dataset_total)) 
  }
  )
  len_top <- reshape2::melt(lengths_top)
  
  return(list(diff_data=diff_plot, diffNaN=diffNaN_plot, diffTop=diffTop_plot, perc_diff=perc_diff, perc_top=perc_top, 
              len_diff=len_diff, len_top=len_top))
}


k_evt_exp_analysis <- function(results,top=3){
  
  k_results <- list()
  for(k in names(results)){
    k_results[[k]] <- list(F1=evt_exp_analysis(results[[k]]$F1,top),
                           precision=evt_exp_analysis_pr(results[[k]]$precision,top),
                           recall=evt_exp_analysis_pr(results[[k]]$recall,top))
  }
  return(k_results)
}

k_evt_exp_analysis_3w <- function(results,top=3){
  k_results <- list()
  for(k in names(results)){
    k_results[[k]] <- list(F1=evt_exp_analysis_3w(results[[k]]$F1,top),
                           precision=evt_exp_analysis_pr_3w(results[[k]]$precision,top),
                           recall=evt_exp_analysis_pr_3w(results[[k]]$recall,top))
  }
  return(k_results)
}

all_evt_exp_analysis <- function(results,top=3){
  
  return(list(F1=evt_exp_analysis(results$F1,top),
              precision=evt_exp_analysis_pr(results$precision,top),
              recall=evt_exp_analysis_pr(results$recall,top)))
}

all_evt_exp_analysis_3w <- function(results,top=3){
  
  return(list(F1=evt_exp_analysis_3w(results$F1,top),
              precision=evt_exp_analysis_pr_3w(results$precision,top),
              recall=evt_exp_analysis_pr_3w(results$recall,top)))
}



#================ Plots =====================

plot_evt_exp_analysis <- function(evt_exp_analysis,dataset_name, rival=c("SoftED","NAB")){
  
  exp_analysis <- evt_exp_analysis$F1
  exp_analysis_p <- evt_exp_analysis$precision
  exp_analysis_r <- evt_exp_analysis$recall
  
  rival_metric <- match.arg(rival)
  
  require(ggplot2)
  require(RColorBrewer)
  require(Cairo)
  
  dataset_lab <- paste(dataset_name,"dataset",sep=" ")
  
  #Boxplot of the differences between Soft F1 and Hard F1
  exp_analysis$diff_data$L1 <- gsub('Tipo', 'Type ', exp_analysis$diff_data$L1)
  p <- ggplot(exp_analysis$diff_data, aes(x=L1, y=value)) + 
    geom_boxplot(fill=brewer.pal(9,"Set1")[1])+
    theme_bw()+
    labs(x=dataset_lab, y = paste0("F1 difference (",rival_metric,"-Hard)"))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/",rival_metric,"vsHard/",dataset_name,"/F1_diff_",rival_metric,".pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  exp_analysis_pr <- rbind(cbind(Metric="Precision",exp_analysis_p$diff_data),cbind(Metric="Recall",exp_analysis_r$diff_data))
  exp_analysis_pr$L1 <- gsub('Tipo', 'Type ', exp_analysis_pr$L1)
  p <- ggplot(exp_analysis_pr, aes(x=L1, y=value, fill=Metric)) + 
    geom_boxplot()+
    theme_bw()+
    labs(x=dataset_lab, y = paste0("Metrics difference (",rival_metric,"-Hard)"))+
    scale_fill_manual(values=c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Reds")[4]),
                      name="Metric")+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/",rival_metric,"vsHard/",dataset_name,"/PR_diff_",rival_metric,".pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  exp_analysis$diffNaN$L1 <- gsub('Tipo', 'Type ', exp_analysis$diffNaN$L1)
  p <- ggplot(exp_analysis$diffNaN, aes(x=L1, y=value)) + 
    geom_boxplot(fill=brewer.pal(9,"Set1")[3])+
    theme_bw()+
    labs(x=dataset_lab, y = paste0(rival_metric," F1 when Hard F1 is N/A"))
  
  file_name <- paste0("plots/",rival_metric,"vsHard/",dataset_name,"/F1_HNaN_",rival_metric,".pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  exp_analysis_pr <- rbind(cbind(Metric="Precision",exp_analysis_p$diffNaN),cbind(Metric="Recall",exp_analysis_r$diffNaN))
  exp_analysis_pr$L1 <- gsub('Tipo', 'Type ', exp_analysis_pr$L1)
  p <- ggplot(exp_analysis_pr, aes(x=L1, y=value, fill=Metric)) + 
    geom_boxplot()+
    theme_bw()+
    labs(x=dataset_lab, y = paste0(rival_metric," metrics when Hard metrics are 0"))+
    scale_fill_manual(values=c(brewer.pal(9,"Greens")[7],brewer.pal(9,"Greens")[4]),
                      name="Metric")+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/",rival_metric,"vsHard/",dataset_name,"/PR_H0_",rival_metric,".pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  blank_theme <- theme_bw()+
    theme(
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      axis.text.x=element_blank()
    )
  col_facets <- length(unique(exp_analysis$perc_diff$L1))/2
  exp_analysis$perc_diff$L1 <- gsub('Tipo', 'Type ', exp_analysis$perc_diff$L1)
  #browser()
  p <- ggplot(exp_analysis$perc_diff, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x=dataset_lab, y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = 1), position = position_stack(vjust = 0.5))+
    scale_fill_brewer(palette="Set1",
                      name=paste0("F1"),
                      labels=c(paste0(rival_metric," > Hard"),
                               paste0(rival_metric," >= 0, Hard is N/A"),
                               paste0(rival_metric," = Hard"),
                               paste0(rival_metric," and Hard are N/A")))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/",rival_metric,"vsHard/",dataset_name,"/analysis_",rival_metric,".pdf")
  CairoPDF(file_name,width=8,height=6)
  print(p)
  dev.off()
  
  
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  exp_analysis$diffTop$L1 <- gsub('Tipo', 'Type ', exp_analysis$diffTop$L1)
  p <- ggplot(exp_analysis$diffTop, aes(x=value)) + 
    geom_histogram(binwidth = 1, fill="gray", color="black", alpha=0.9) +
    facet_wrap(~L1, ncol=col_facets) +
    theme_bw()+
    labs(x=paste0("Rank climb of top 5 methods based on ",rival_metric," F1"), y = dataset_lab)+
    scale_x_continuous(breaks = seq(0, max(exp_analysis$diffTop$value), by = 1))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/",rival_metric,"vsHard/",dataset_name,"/climb_top_",rival_metric,".pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  exp_analysis$perc_top$L1 <- gsub('Tipo', 'Type ', exp_analysis$perc_top$L1)
  p <- ggplot(exp_analysis$perc_top, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x=dataset_lab, y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = 1), position = position_stack(vjust = 0.5))+
    scale_fill_brewer(palette="Set2",
                      name=paste0("Top 5 methods (by ",rival_metric," F1)"),
                      labels=c("Climbed to the top",
                               "Climbed within the top",
                               "Maintened position",
                               "Pushed down the ranking"))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/",rival_metric,"vsHard/",dataset_name,"/analysis_top_",rival_metric,".pdf")
  CairoPDF(file_name,width=10,height=6)
  print(p)
  dev.off()
}


plot_evt_exp_comparative <- function(evt_exp_analysis_soft,evt_exp_analysis_nab,dataset_name){
  
  exp_analysis_soft <- evt_exp_analysis_soft$F1
  exp_analysis_p_soft <- evt_exp_analysis_soft$precision
  exp_analysis_r_soft <- evt_exp_analysis_soft$recall
  
  exp_analysis_nab <- evt_exp_analysis_nab$F1
  exp_analysis_p_nab <- evt_exp_analysis_nab$precision
  exp_analysis_r_nab <- evt_exp_analysis_nab$recall
  
  require(ggplot2)
  require(RColorBrewer)
  require(Cairo)
  #browser()
  dataset_lab <- paste(dataset_name,"dataset",sep=" ")
  
  col_facets <- length(unique(exp_analysis_soft$perc_diff$L1))/2
  
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  exp_analysis_pr_soft <- rbind(cbind(Metric="Precision",exp_analysis_p_soft$diffNaN),cbind(Metric="Recall",exp_analysis_r_soft$diffNaN))
  exp_analysis_pr_nab <- rbind(cbind(Metric="Precision",exp_analysis_p_nab$diffNaN),cbind(Metric="Recall",exp_analysis_r_nab$diffNaN))
  exp_analysis_pr <- rbind(cbind(rival="SoftED",exp_analysis_pr_soft),cbind(rival="NAB",exp_analysis_pr_nab))
  exp_analysis_pr$rival <- factor(exp_analysis_pr$rival, levels = c("SoftED","NAB"))
  agg_mean <- aggregate(exp_analysis_pr$value, list(L1 = exp_analysis_pr$L1, rival = exp_analysis_pr$rival, Metric=exp_analysis_pr$Metric), mean)
  agg_sd <- aggregate(exp_analysis_pr$value, list(L1 = exp_analysis_pr$L1, rival = exp_analysis_pr$rival, Metric=exp_analysis_pr$Metric), sd)
  colnames(agg_mean) <- c("L1","rival","Metric","value")
  exp_analysis_pr <- agg_mean
  exp_analysis_pr$sd <- agg_sd$x
  exp_analysis_pr$L1 <- gsub('Tipo', 'Type ', exp_analysis_pr$L1)
  
  
  p <- ggplot(exp_analysis_pr, aes(x=rival, y=value, fill=interaction(Metric,rival))) + 
    geom_bar(position="dodge", stat = "identity")+
    #geom_errorbar( aes(x=rival, ymin=value-sd, ymax=value+sd), position = position_dodge(.9), width=0.4, colour="black", alpha=0.9, size=0.5)+
    facet_wrap(~ L1, ncol=col_facets)+
    theme_bw()+
    labs(x=dataset_lab, y = paste0("SoftED and NAB metrics when Hard metrics are 0"))+
    scale_fill_manual(values=c(brewer.pal(9,"Purples")[7],brewer.pal(9,"Purples")[4],
                               brewer.pal(9,"Blues")[7],brewer.pal(9,"Blues")[4]),
                      name="Metric",
                      labels=c("Precision (SoftED)","Recall (SoftED)","Precision (NAB)","Recall (NAB)"))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/comparative/",dataset_name,"/PR_H0.pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  
  exp_analysis <- rbind(cbind(rival="SoftED",exp_analysis_soft$perc_diff),cbind(rival="NAB",exp_analysis_nab$perc_diff))
  exp_analysis$rival <- factor(exp_analysis$rival, levels = c("SoftED","NAB"))
  exp_analysis$L1 <- gsub('Tipo', 'Type ', exp_analysis$L1)
  
  blank_theme <- theme_bw()+
    theme(
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank()
    )
  #browser()
  p <- ggplot(exp_analysis, aes(x=rival, y=value, fill=group))+
    geom_bar(position="stack",width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    theme_bw()+
    labs(x=dataset_lab, y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = rival), position = position_stack(vjust = 0.5),size=3)+
    scale_fill_brewer(palette="Set1",
                      name=paste0("F1"),
                      labels=c(paste0("SoftED/NAB > Hard"),
                               paste0("SoftED/NAB >= 0, Hard is N/A"),
                               paste0("Same value"),
                               paste0("Both are N/A")))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/comparative/",dataset_name,"/analysis.pdf")
  CairoPDF(file_name,width=8,height=6)
  print(p)
  dev.off()
  
  exp_analysis <- rbind(cbind(rival="SoftED",exp_analysis_soft$perc_top),cbind(rival="NAB",exp_analysis_nab$perc_top))
  exp_analysis$rival <- factor(exp_analysis$rival, levels = c("SoftED","NAB"))
  exp_analysis$L1 <- gsub('Tipo', 'Type ', exp_analysis$L1)
  
  p <- ggplot(exp_analysis, aes(x=rival, y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x=dataset_lab, y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = rival), position = position_stack(vjust = 0.5),size=3)+
    scale_fill_brewer(palette="Set2",
                      name=paste0("Top 5 methods (by SoftED/NAB F1)"),
                      labels=c("Climbed to the top",
                               "Climbed within the top",
                               "Maintened position",
                               "Pushed down the ranking"))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/comparative/",dataset_name,"/analysis_top.pdf")
  CairoPDF(file_name,width=10,height=6)
  print(p)
  dev.off()
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
  require(Cairo)
  
  dataset_lab <- paste(dataset_name,"dataset",sep=" ")
  
  #Boxplot of the differences between Soft F1 and Hard F1
  exp_analysis$diff_data$L1 <- gsub('Tipo', 'Type ', exp_analysis$diff_data$L1)
  p <- ggplot(exp_analysis$diff_data, aes(x=L1, y=value, fill=reorder(k, sort(as.numeric(k))))) + 
    geom_boxplot()+
    theme_bw()+
    labs(x=dataset_lab, y = paste0("F1 difference (SoftED-Hard)"))+
    scale_fill_brewer(palette="Paired",name="k")+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/SoftEDbyK/",dataset_name,"/F1_diff_SoftED.pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  
  #Boxplot of Soft F1 when Hard F1 is NaN (no true positives)
  exp_analysis$diffNaN$L1 <- gsub('Tipo', 'Type ', exp_analysis$diffNaN$L1)
  p <- ggplot(exp_analysis$diffNaN, aes(x=L1, y=value, fill=reorder(k, sort(as.numeric(k))))) + 
    geom_boxplot()+
    theme_bw()+
    labs(x=dataset_lab, y = paste0("SoftED F1 when Hard F1 is N/A"))+
    scale_fill_brewer(palette="Paired",name="k")+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/SoftEDbyK/",dataset_name,"/F1_HNaN_SoftED.pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()

  
  exp_analysis_pr <- rbind(cbind(Metric="Precision",exp_analysis_p$diffNaN),cbind(Metric="Recall",exp_analysis_r$diffNaN))
  agg_mean <- aggregate(exp_analysis_pr$value, list(L1 = exp_analysis_pr$L1, k=exp_analysis_pr$k , Metric=exp_analysis_pr$Metric), mean)
  agg_sd <- aggregate(exp_analysis_pr$value, list(L1 = exp_analysis_pr$L1, k=exp_analysis_pr$k, Metric=exp_analysis_pr$Metric), sd)
  colnames(agg_mean) <- c("L1","k","Metric","value")
  exp_analysis_pr <- agg_mean
  exp_analysis_pr$sd <- agg_sd$x
  exp_analysis_pr$L1 <- gsub('Tipo', 'Type ', exp_analysis_pr$L1)
  p <- ggplot(exp_analysis_pr, aes(x=L1, y=value, fill=reorder(k, sort(as.numeric(k))))) + 
    #geom_boxplot()+
    geom_bar(position="dodge", stat = "identity")+
    facet_wrap(~ Metric)+
    theme_bw()+
    labs(x=dataset_lab, y = paste0("SoftED metrics when Hard metrics are 0"))+
    scale_fill_brewer(palette="Paired",name="k")+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/SoftEDbyK/",dataset_name,"/PR_H0_SoftED.pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  blank_theme <- theme_bw()+
    theme(
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank()
    )
  col_facets <- length(unique(exp_analysis$perc_diff$L1))/2
  
  exp_analysis$perc_diff$L1 <- gsub('Tipo', 'Type ', exp_analysis$perc_diff$L1)
  p <- ggplot(exp_analysis$perc_diff, aes(x=reorder(k, sort(as.numeric(k))), y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x="k", y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = k), position = position_stack(vjust = 0.5),size=3)+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette="Set1",
                      name=paste0("F1"),
                      labels=c(paste0("SoftED > Hard"),
                               paste0("SoftED = Hard"),
                               paste0("SoftED >= 0, Hard is N/A"),
                               paste0("SoftED and Hard are N/A")))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/SoftEDbyK/",dataset_name,"/analysis.pdf")
  CairoPDF(file_name,width=8,height=6)
  print(p)
  dev.off()
  
  
  #Boxplot of rank climb of top 5 methods based on the Soft F1
  exp_analysis$diffTop$L1 <- gsub('Tipo', 'Type ', exp_analysis$diffTop$L1)
  p <- ggplot(exp_analysis$diffTop, aes(x=value)) + 
    geom_histogram(binwidth = 1, fill="gray", color="black", alpha=0.9) +
    facet_grid(reorder(k, sort(as.numeric(k))) ~ L1)+
    theme_bw()+
    labs(x=paste0("Rank climb of top 5 methods based on SoftED F1"), y = dataset_lab)+
    scale_x_continuous(breaks = seq(0, max(exp_analysis$diffTop$value), by = 1))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/SoftEDbyK/",dataset_name,"/climb_top_SoftED.pdf")
  CairoPDF(file_name,width=7,height=6)
  print(p)
  dev.off()
  
  exp_analysis$perc_top$L1 <- gsub('Tipo', 'Type ', exp_analysis$perc_top$L1)
  p <- ggplot(exp_analysis$perc_top, aes(x=reorder(k, sort(as.numeric(k))), y=value, fill=group))+
    geom_bar(width = 1, stat = "identity", color="white")+
    facet_wrap(~ L1, ncol=col_facets)+
    blank_theme+
    labs(x="k", y = "")+
    geom_text(aes(label = scales::percent(value,accuracy =1), x = k), position = position_stack(vjust = 0.5),size=3)+
    scale_fill_brewer(palette="Set2",
                      name=paste0("Top 5 methods (by SoftED F1)"),
                      labels=c("Climbed to the top",
                               "Climbed within the top",
                               "Maintened position",
                               "Pushed down the ranking"))+
    theme(legend.position="bottom")
  
  file_name <- paste0("plots/SoftEDbyK/",dataset_name,"/analysis_top.pdf")
  CairoPDF(file_name,width=10,height=6)
  print(p)
  dev.off()
}