#================ Function definitions: Soft-Hard differences ================
evt_exp_diffs <- function(data_results){
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  df_soft_hard <- lapply(data_results, function(dataset) {
    
    df_res <- dataset$metric_var_df
    
    col_hard <- grep("_hard", colnames(df_res))
    col_soft <- grep("_soft", colnames(df_res))
    methods <- unique(sub("_.*", "", colnames(df_res[col_hard])))
    
    df_soft <- df_res[c(1,col_soft)]
    names(df_soft) <- c("var",methods)
    df_soft <- reshape2::melt(df_soft, id=c("var"), value.name = "soft")
    
    df_hard <- df_res[c(1,col_hard)]
    names(df_hard) <- c("var",methods)
    df_hard <- reshape2::melt(df_hard, id=c("var"), value.name = "hard")
    
    df_soft_hard <- merge(df_soft,df_hard)
    names(df_soft_hard) <- c("var","method","soft","hard")
    
    
    cbind(df_soft_hard, soft_hard_diff= df_soft_hard$soft - df_soft_hard$hard)
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(df_soft_hard, function(dataset) dataset[!is.na(dataset$soft_hard_diff),] )
  
  #NaN,NaN;NaN,vs: vs (soft F1s; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(df_soft_hard, function(dataset) dataset[ !is.na(dataset$soft) & is.nan(dataset$hard), c("var","method","hard","soft")] )
  
  
  # #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(df_soft_hard, function(dataset) {
    require(dplyr)
    df_Top <- dataset %>% group_by(var,.drop = FALSE) %>%
      filter(!is.na(hard) & hard>0) %>%
      summarise(method=method,rank_hard=order(hard),rank_soft=order(soft))

    top5_change <- cbind(df_Top, rank_change= df_Top$rank_hard - df_Top$rank_soft)

    top5_change[top5_change$rank_soft<=5,]
  }
  )
  
  return(list(diff_data=diff_data, diffNaN=diffNaN)) #, diffTop=diffTop
}

evt_exp_diffs_pr <- function(data_results){
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  df_soft_hard <- lapply(data_results, function(dataset) {
    
    df_res <- dataset$metric_var_df
    
    col_hard <- grep("_hard", colnames(df_res))
    col_soft <- grep("_soft", colnames(df_res))
    methods <- unique(sub("_.*", "", colnames(df_res[col_hard])))
    
    df_soft <- df_res[c(1,col_soft)]
    names(df_soft) <- c("var",methods)
    df_soft <- reshape2::melt(df_soft, id=c("var"), value.name = "soft")
    
    df_hard <- df_res[c(1,col_hard)]
    names(df_hard) <- c("var",methods)
    df_hard <- reshape2::melt(df_hard, id=c("var"), value.name = "hard")
    
    df_soft_hard <- merge(df_soft,df_hard)
    names(df_soft_hard) <- c("var","method","soft","hard")
    
    
    cbind(df_soft_hard, soft_hard_diff= df_soft_hard$soft - df_soft_hard$hard)
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(df_soft_hard, function(dataset) dataset[!is.na(dataset$soft_hard_diff),] )
  
  #NaN,NaN;NaN,vs: vs (soft precision/recall; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(df_soft_hard, function(dataset) dataset[ !is.na(dataset$soft) & dataset$hard==0, c("var","method","hard","soft")] )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(df_soft_hard, function(dataset) {
    require(dplyr)
    df_Top <- dataset %>% group_by(var,.drop = FALSE) %>%
      filter(!is.na(hard) & hard>0) %>%
      summarise(method=method,rank_hard=order(hard),rank_soft=order(soft))

    top5_change <- cbind(df_Top, rank_change= df_Top$rank_hard - df_Top$rank_soft)

    top5_change[top5_change$rank_soft<=5,]
  }
  )
  
  return(list(diff_data=diff_data, diffNaN=diffNaN)) #, diffTop=diffTop
}

all_evt_exp_diffs <- function(results){
  k_results <- list()
  for(k in names(results)){
    k_results[[k]] <- list(F1=evt_exp_diffs(results[[k]]$F1),
                           precision=evt_exp_diffs_pr(results[[k]]$precision),
                           recall=evt_exp_diffs_pr(results[[k]]$recall))
  }
  return(k_results)
}


#================ Soft-Hard differences ================
data_diffs_tmn <- all_evt_exp_diffs(tmn_results)


#================ Top 3 Soft-Hard differences ================
top_data_diffs_tmn <- lapply(data_diffs_tmn,function(k) lapply(k,function(metric) lapply(metric,function(diff) {
  require(dplyr)
  top <- bind_rows(diff, .id = "tipo")
  top <- head(top[order(top[,ncol(top)],decreasing = TRUE),],5)
  as.data.frame(top[top[,ncol(top)]!=0,])
})))



#========= Example series =========
rank <- 1
#K of 6 months
example_series <- top_data_diffs_tmn[["6"]][["F1"]][["diffNaN"]][,1:5]

evts <- evts_tmn[[1]][[example_series$var[rank]]][[example_series$method[rank]]]
ref <- ref_tmn[[1]][[example_series$var[rank]]]
serie <- trend[[example_series$var[rank]]]
#serie <- train[[example_series$var[1]]]
evaluate(evts, ref, metric="confusion_matrix")
soft_evaluate(evts, ref, metric="confusion_matrix")

#Plot
print(evtplot(serie,evts, ref))

#==== NAB score ====
score_data <- ref
colnames(score_data) <- c("timestamp","is.real.anomaly")
score_data$is.anomaly <- score_data$timestamp %in% evts$time
score_data$value <- serie[,2]
otsad::GetDetectorScore(score_data, print = TRUE, title = "")


#K of 12 months
example_series <- top_data_diffs[["12"]][["F1"]][["diffNaN"]][,1:5]

evts <- evts_tmn[[1]][[example_series$var[rank]]][[example_series$method[rank]]]
ref <- ref_tmn[[1]][[example_series$var[rank]]]
serie <- trend[[example_series$var[rank]]]
#serie <- train[[example_series$var[1]]]
evaluate(evts, ref, metric="confusion_matrix")
soft_evaluate(evts, ref, metric="confusion_matrix")

#Plot
print(evtplot(serie,evts, ref))



#K of 24 months
example_series <- top_data_diffs[["24"]][["F1"]][["diffNaN"]][,1:5]

evts <- evts_tmn[[1]][[example_series$var[rank]]][[example_series$method[rank]]]
ref <- ref_tmn[[1]][[example_series$var[rank]]]
serie <- trend[[example_series$var[rank]]]
#serie <- train[[example_series$var[1]]]
evaluate(evts, ref, metric="confusion_matrix")
soft_evaluate(evts, ref, metric="confusion_matrix")

#Plot
print(evtplot(serie,evts, ref))