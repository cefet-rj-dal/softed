source("exp_func_SoftED.r")
load("datasets_evts_ref.RData")

#================ Preparing the data =====================

dataset_names <- c("Tipo1","Tipo2","Tipo5","Tipo6","Tipo7","Tipo8")

evt_exp <- get_dataset("../detection_data/3WDataset/",dataset_names)

evts_3w <- list_evt_exp_3w(evt_exp)


#================ Preparing the reference data =====================

evt_reference <- get_reference_3w("../detection_data/3WDataset/",dataset_names)

ref_3w <- lapply(evt_reference, function(dataset) lapply(dataset, function(var) {
  var <- as.data.frame(var)
  classes <- unique(var[["class"]])[-c(1:2)]
  cp <- match(classes, var[["class"]])
  var[-cp,"class"] <- 0
  var[cp,"class"] <- 1
  return(var)
} ) )



#================ Evaluating all metrics - Hard, SoftED and NAB =====================

evaluate_metrics_3w <- evaluate_results_3w(evts_3w,ref_3w, k_values=seq(15, 60, by = 15))
saveRDS(evaluate_metrics_3w,"evaluate_metrics_3w.rds")



#================ Metric comparison results - SoftED vs Hard =====================

data3w_results_soft <- k_results_3w(evaluate_metrics_3w, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft")
data3w_analysis_soft <- k_evt_exp_analysis_3w(data3w_results_soft, top=3)

#========= Plots =========

plot_evt_exp_analysis(data3w_analysis_soft$`15`,"3W", rival="SoftED")

plot_evt_exp_analysis_by_k(data3w_analysis_soft,"3W")



#================ Metric comparison results - NAB vs Hard =====================

data3w_results_nab <- data_results_3w(evaluate_metrics_3w, metrics=c("F1","precision","recall"), rival="NAB")
data3w_analysis_nab <- all_evt_exp_analysis_3w(data3w_results_nab, top=3)

#========= Plots =========

plot_evt_exp_analysis(data3w_analysis_nab,"3W", rival="NAB")


#========= Comparative Plots SoftED vs NAB =========

plot_evt_exp_comparative(data3w_analysis_soft$`15`,data3w_analysis_nab,"3W")



#================ Calculating NAB window lengths =====================

data3w_wlen_nab <- NAB_wlen_3w(ref_3w, window.length.perc = 0.1)
summary(as.numeric(data3w_wlen_nab$windowLength))
