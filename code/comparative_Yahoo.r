source("exp_func_SoftED.r")
load("datasets_evts_ref.RData")

#================ Preparing the data =====================

dataset_names <- c("A1","A2","A3","A4")

evt_exp <- get_dataset("../detection_data/Yahoo/",dataset_names)
evts_yahoo <- list_evt_exp(evt_exp)


#================ Preparing the reference data =====================

evt_reference <- get_reference("../detection_data/Yahoo/",dataset_names,evts_yahoo)

ref_yahoo <- lapply(evt_reference, function(dataset) lapply(dataset, function(var) cbind(time=1:length(var[[1]]),event=var[1])) )



#================ Evaluating all metrics - Hard, SoftED and NAB =====================

evaluate_metrics_yahoo <- evaluate_results(evts_yahoo,ref_yahoo,dataset_name="Yahoo",k_values=seq(15, 60, by = 15))
saveRDS(evaluate_metrics_yahoo,"evaluate_metrics_yahoo.rds")



#================ Metric comparison results - SoftED vs Hard =====================

yahoo_results_soft <- k_results(evaluate_metrics_yahoo, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft")
yahoo_analysis_soft <- k_evt_exp_analysis(yahoo_results_soft, top=3)

#========= Plots =========

plot_evt_exp_analysis(yahoo_analysis_soft$`15`,"Yahoo", rival="SoftED")

plot_evt_exp_analysis_by_k(yahoo_analysis_soft,"Yahoo")



#================ Metric comparison results - NAB vs Hard =====================

yahoo_results_nab <- data_results(evaluate_metrics_yahoo, metrics=c("F1","precision","recall"), rival="NAB")
yahoo_analysis_nab <- all_evt_exp_analysis(yahoo_results_nab, top=3)

#========= Plots =========

plot_evt_exp_analysis(yahoo_analysis_nab,"Yahoo", rival="NAB")



#========= Comparative Plots SoftED vs NAB =========

plot_evt_exp_comparative(yahoo_analysis_soft$`15`,yahoo_analysis_nab,"Yahoo")



#================ Calculating NAB window lengths =====================

yahoo_wlen_nab <- NAB_wlen(ref_yahoo, window.length.perc = 0.1)
summary(as.numeric(yahoo_wlen_nab$windowLength))
