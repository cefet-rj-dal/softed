source("exp_func_SoftED.r")
load("datasets_evts_ref.RData")


#===========3W ==================
# Evaluating all metrics - Hard, SoftED and NAB
evaluate_metrics_3w <- evaluate_results_3w(evts_3w,ref_3w, k_values=seq(15, 60, by = 15))
saveRDS(evaluate_metrics_3w,"evaluate_metrics_3w.rds")

# Calculating NAB window lengths
data3w_wlen_nab <- NAB_wlen_3w(ref_3w, window.length.perc = 0.1)
summary(as.numeric(data3w_wlen_nab$windowLength))


#=========== NAB ==================
# Evaluating all metrics - Hard, SoftED and NAB
evaluate_metrics_NAB <- evaluate_results(evts_NAB,ref_NAB,dataset_name="NAB",k_values=seq(15, 60, by = 15))
saveRDS(evaluate_metrics_NAB,"evaluate_metrics_NAB.rds")

# Calculating NAB window lengths
NAB_wlen_nab <- NAB_wlen(ref_NAB, window.length.perc = 0.1)
summary(as.numeric(NAB_wlen_nab$windowLength))


#=========== NMR ==================
# Evaluating all metrics - Hard, SoftED and NAB
evaluate_metrics_tmn <- evaluate_results(evts_tmn,ref_tmn,dataset_name="TMN",k_values=c(6,12,15,24))
saveRDS(evaluate_metrics_tmn,"evaluate_metrics_tmn.rds")

# Calculating NAB window lengths
tmn_wlen_nab <- NAB_wlen(ref_tmn, window.length.perc = 0.1)
summary(as.numeric(tmn_wlen_nab$windowLength))


#=========== Yahoo ==================
# Evaluating all metrics - Hard, SoftED and NAB
evaluate_metrics_yahoo <- evaluate_results(evts_yahoo,ref_yahoo,dataset_name="Yahoo",k_values=seq(15, 60, by = 15))
saveRDS(evaluate_metrics_yahoo,"evaluate_metrics_yahoo.rds")

# Calculating NAB window lengths
yahoo_wlen_nab <- NAB_wlen(ref_yahoo, window.length.perc = 0.1)
summary(as.numeric(yahoo_wlen_nab$windowLength))