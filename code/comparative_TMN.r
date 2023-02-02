source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R")

load("../detection_data/TMN/tmn_evts.RData")

source("exp_func_SoftED.r")
load("datasets_evts_ref.RData")


#================ Evaluating all metrics - Hard, SoftED and NAB =====================

evaluate_metrics_tmn <- evaluate_results(evts_tmn,ref_tmn,dataset_name="TMN",k_values=c(6,12,15,24))
saveRDS(evaluate_metrics_tmn,"evaluate_metrics_tmn.rds")



#================ Metric comparison results - SoftED vs Hard =====================

tmn_results_soft <- k_results(evaluate_metrics_tmn, k_values=c(6,12,15,24,seq(15, 60, by = 15)), metrics=c("F1","precision","recall"), rival="soft")
tmn_analysis_soft <- k_evt_exp_analysis(tmn_results_soft, top=3)
#========= Plots =========

plot_evt_exp_analysis(tmn_analysis_soft$`15`,"TMN", rival="SoftED")

plot_evt_exp_analysis_by_k(tmn_analysis_soft,"TMN")



#================ Metric comparison results - NAB vs Hard =====================

tmn_results_nab <- data_results(evaluate_metrics_tmn, metrics=c("F1","precision","recall"), rival="NAB")
tmn_analysis_nab <- all_evt_exp_analysis(tmn_results_nab, top=3)

#========= Plots =========

plot_evt_exp_analysis(tmn_analysis_nab,"TMN", rival="NAB")



#========= Comparative Plots SoftED vs NAB =========

plot_evt_exp_comparative(tmn_analysis_soft$`15`,tmn_analysis_nab,"TMN")


#================ Calculating NAB window lengths =====================

tmn_wlen_nab <- NAB_wlen(ref_tmn, window.length.perc = 0.1)
summary(as.numeric(tmn_wlen_nab$windowLength))
