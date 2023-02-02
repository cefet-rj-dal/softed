#================ Function definitions: Soft-Hard differences ================
evt_exp_diffs_3w <- function(data_results){

  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  df_soft_hard <- lapply(data_results, function(dataset) {
    
    df_res <- dataset$metric_well_df
    
    col_hard <- grep("_hard", colnames(df_res))
    col_soft <- grep("_soft", colnames(df_res))
    methods <- unique(sub("_.*", "", colnames(df_res[col_hard])))
    
    df_soft <- df_res[c(1:2,col_soft)]
    names(df_soft) <- c("well","var",methods)
    df_soft <- reshape2::melt(df_soft, id=c("well","var"), value.name = "soft")
    
    df_hard <- df_res[c(1:2,col_hard)]
    names(df_hard) <- c("well","var",methods)
    df_hard <- reshape2::melt(df_hard, id=c("well","var"), value.name = "hard")
    
    df_soft_hard <- merge(df_soft,df_hard)
    names(df_soft_hard) <- c("well","var","method","soft","hard")

    
    cbind(df_soft_hard, soft_hard_diff= df_soft_hard$soft - df_soft_hard$hard)
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(df_soft_hard, function(dataset) dataset[!is.na(dataset$soft_hard_diff),] )
  
  #NaN,NaN;NaN,vs: vs (soft F1s; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(df_soft_hard, function(dataset) dataset[ !is.na(dataset$soft) & is.nan(dataset$hard), c("well","var","method","hard","soft")] )
  
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(df_soft_hard, function(dataset) {
    require(dplyr)
    df_Top <- dataset %>% group_by(well,var,.drop = FALSE) %>% 
      filter(!is.na(hard) & hard>0) %>% 
      summarise(method=method,rank_hard=order(hard),rank_soft=order(soft))

    top5_change <- cbind(df_Top, rank_change= df_Top$rank_hard - df_Top$rank_soft)
    
    top5_change[top5_change$rank_soft<=5,]
  }
  )
  
  return(list(diff_data=diff_data, diffNaN=diffNaN, diffTop=diffTop))
}

evt_exp_diffs_pr_3w <- function(data_results){

  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  df_soft_hard <- lapply(data_results, function(dataset) {
    
    df_res <- dataset$metric_well_df
    
    col_hard <- grep("_hard", colnames(df_res))
    col_soft <- grep("_soft", colnames(df_res))
    methods <- unique(sub("_.*", "", colnames(df_res[col_hard])))
    
    df_soft <- df_res[c(1:2,col_soft)]
    names(df_soft) <- c("well","var",methods)
    df_soft <- reshape2::melt(df_soft, id=c("well","var"), value.name = "soft")
    
    df_hard <- df_res[c(1:2,col_hard)]
    names(df_hard) <- c("well","var",methods)
    df_hard <- reshape2::melt(df_hard, id=c("well","var"), value.name = "hard")
    
    df_soft_hard <- merge(df_soft,df_hard)
    names(df_soft_hard) <- c("well","var","method","soft","hard")
    
    
    cbind(df_soft_hard, soft_hard_diff= df_soft_hard$soft - df_soft_hard$hard)
  }
  )
  
  #vh,vs: vs-vh (diffs; % of diffs > 0; % of diffs == 0)
  diff_data <- lapply(df_soft_hard, function(dataset) dataset[!is.na(dataset$soft_hard_diff),] )
  
  #NaN,NaN;NaN,vs: vs (soft precision/recall; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(data_results, function(dataset) {
    diff_dataset <- sapply(dataset$metric_well, function(well) sapply(well, function(serie) serie$soft[-1][!is.na(serie$soft[-1]) & serie$hard[-1]==0] ))
    as.numeric(unlist( diff_dataset ))
  }
  )
  #NaN,NaN;NaN,vs: vs (soft precision/recall; % of Soft F1>=0 and hard F1 N/A)
  diffNaN <- lapply(df_soft_hard, function(dataset) dataset[ !is.na(dataset$soft) & dataset$hard==0, c("well","var","method","hard","soft")] )
  
  #change in top 5 ranking (boxplot, % of results)
  diffTop <- lapply(df_soft_hard, function(dataset) {
    require(dplyr)
    df_Top <- dataset %>% group_by(well,var,.drop = FALSE) %>% 
      filter(!is.na(hard) & hard>0) %>% 
      summarise(method=method,rank_hard=order(hard),rank_soft=order(soft))
    
    top5_change <- cbind(df_Top, rank_change= df_Top$rank_hard - df_Top$rank_soft)
    
    top5_change[top5_change$rank_soft<=5,]
  }
  )
  
  return(list(diff_data=diff_data, diffNaN=diffNaN, diffTop=diffTop))
}

all_evt_exp_diffs_3w <- function(results){
  k_results <- list()
  for(k in names(results)){
    k_results[[k]] <- list(F1=evt_exp_diffs_3w(results[[k]]$F1),
                           precision=evt_exp_diffs_pr_3w(results[[k]]$precision),
                           recall=evt_exp_diffs_pr_3w(results[[k]]$recall))
  }
  return(k_results)
}


#================ Soft-Hard differences ================
data3w_diffs <- all_evt_exp_diffs_3w(data3w_results)


#================ Top 3 Soft-Hard differences ================
top_data3w_diffs <- lapply(data3w_diffs,function(k) lapply(k,function(metric) lapply(metric,function(diff) {
  require(dplyr)
  top <- bind_rows(diff, .id = "tipo")
  top <- head(top[order(top[,ncol(top)],decreasing = TRUE),],3)
  as.data.frame(top[top[,ncol(top)]!=0,])
})))

example_series_3w <- top_data3w_diffs[["60"]][["F1"]][["diffNaN"]][,1:4]
#example_series_3w <- top_data3w_diffs[["60"]][["recall"]][["diffTop"]][,1:4]


#Hard recall are the same and equal 0.5
#Soft recal of SCP is 0.84 and of NA is 0.5 
#Tipo2,WELL-00003_20141122214325,P-TPT,NA
#Tipo2,WELL-00003_20141122214325,P-TPT,SCP
#
#Hard recall are the same and equal 0.5
#Soft recal of KNN-CAD is 0.99 and all others are 0.5 
#Tipo2,WELL-00002_20131104014101,T-TPT,GARCH
#Tipo2,WELL-00002_20131104014101,T-TPT,KNN-CAD
#Tipo2,WELL-00002_20131104014101,T-TPT,NA
#Tipo2,WELL-00002_20131104014101,T-TPT,SCP
example_series_3w_2 <- rbind(cbind(tipo="Tipo2",well="WELL-00003_20141122214325",var="P-TPT",method=c("NA","SCP")),
                           cbind(tipo="Tipo2",well="WELL-00002_20131104014101",var="T-TPT",method=c("GARCH","KNN-CAD","NA","SCP")))


examples_data3w_diffs <- lapply(data3w_diffs[["60"]], function(metric) lapply(metric,function(diff) {
  require(dplyr)
  ex <- example_series_3w
  series_data <- bind_rows(diff, .id = "tipo") %>% 
    filter((tipo %in% ex$tipo[1] & well %in% ex$well[1] & var %in% ex$var[1])|
             (tipo %in% ex$tipo[2] & well %in% ex$well[2] & var %in% ex$var[2])|
             (tipo %in% ex$tipo[3] & well %in% ex$well[3] & var %in% ex$var[3]))
  series_data
}))

#================ Selecting example series ================

source( "https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R" )

construct_examples <- function(example_series){
  
  df_examples <- NULL
  plots <- list()
  
  for(example in 1:nrow(example_series)){
    ex <- example_series[example,1:4]
    ex <- as.data.frame(lapply(ex, as.character))
    
    evts <- evts_3w[[ex$tipo]][[ex$well]][[ex$var]][[ex$method]]
    ref <- ref_3w[[ex$tipo]][[ex$well]]
    
    serie <- read.csv(paste0("../detection_data/3W_examples/",ex$well,".csv"))
    #browser()
    serie <- serie[c("timestamp",gsub("-",".",ex$var))]
    serie$timestamp <- as.POSIXct(serie$timestamp,tz = "GMT")

    plots[[paste0(ex$well,".",ex$method)]] <- evtplot(serie,evts,ref)
    
    df_metrics <- NULL
    for(metric in c("accuracy","sensitivity","specificity","pos_pred_value","neg_pred_value","precision",
                "recall","F1","prevalence","detection_rate","detection_prevalence","balanced_accuracy", "rank")){
    
      if(metric != "rank"){
        hm <- round(hard_evaluate(evts, ref, metric=metric),2)
        sm <- round(soft_evaluate(evts, ref, k=60, metric=metric),2)
        diff <- sm-hm
      }
      else{
        require(dplyr)
        #browser()
        ranking <- bind_rows(data3w_diffs[["60"]][["F1"]][["diffTop"]], .id = "tipo") %>% 
          filter(tipo==ex$tipo & well==ex$well & var==ex$var & method==ex$method)
        
        hm <- ranking$rank_hard
        sm <- ranking$rank_soft
        diff <- hm-sm
      }
      
      tryCatch(df_metrics <- rbind(df_metrics,cbind(ex,metric=metric,hard=hm,soft=sm, diff=diff)),
               error = function(e) NA)
      
    }
    df_examples <- rbind(df_examples,df_metrics)
  }
  
  return(list(df_examples=df_examples,plots=plots))
}

examples_3w <- construct_examples( example_series_3w )

examples_3w_2 <- construct_examples( example_series_3w_2 )
