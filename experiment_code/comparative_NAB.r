source("exp_func_SoftED.r")
load("datasets_evts_ref.RData")

#================ Preparing the data =====================

dataset_names <- c("artificialWithAnomaly","realAdExchange","realAWSCloudwatch",
                   "realKnownCause","realTraffic","realTweets")

evt_exp <- get_dataset("../detection_data/NAB/",dataset_names)

evts_NAB <- list_evt_exp(evt_exp)
evts_NAB <- lapply(evts_NAB, function(dataset) lapply(dataset, function(var) lapply(var, function(method) {
  method$time <- tryCatch(as.POSIXct(lubridate::ymd_hms(method$time)),error = function(e) NA)
  method} )))


#================ Preparing the reference data =====================

evt_reference <- get_reference("../detection_data/NAB/",dataset_names,evts_NAB)

ref_NAB <- lapply(evt_reference, function(dataset) lapply(dataset, function(var) {
  var$timestamp <- tryCatch(as.POSIXct(lubridate::ymd_hms(var$timestamp)),error = function(e) NA)
  var} ))



#================ Evaluating all metrics - Hard, SoftED and NAB =====================

evaluate_metrics_NAB <- evaluate_results(evts_NAB,ref_NAB,dataset_name="NAB",k_values=seq(15, 60, by = 15))
saveRDS(evaluate_metrics_NAB,"evaluate_metrics_NAB.rds")



#================ Metric comparison results - SoftED vs Hard =====================

NAB_results_soft <- k_results(evaluate_metrics_NAB, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft")
NAB_analysis_soft <- k_evt_exp_analysis(NAB_results_soft, top=3)

#========= Plots =========

plot_evt_exp_analysis(NAB_analysis_soft$`15`,"NAB", rival="SoftED")

plot_evt_exp_analysis_by_k(NAB_analysis_soft,"NAB")



#================ Metric comparison results - NAB vs Hard =====================

NAB_results_nab <- data_results(evaluate_metrics_NAB, metrics=c("F1","precision","recall"), rival="NAB")
NAB_analysis_nab <- all_evt_exp_analysis(NAB_results_nab, top=3)

#========= Plots =========

plot_evt_exp_analysis(NAB_analysis_nab,"NAB", rival="NAB")



#========= Comparative Plots SoftED vs NAB =========

plot_evt_exp_comparative(NAB_analysis_soft$`15`,NAB_analysis_nab,"NAB")



#================ Calculating NAB window lengths =====================

NAB_wlen_nab <- NAB_wlen(ref_NAB, window.length.perc = 0.1)
summary(as.numeric(NAB_wlen_nab$windowLength))


