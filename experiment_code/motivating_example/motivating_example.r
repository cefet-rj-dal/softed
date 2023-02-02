library(tibble)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R")

#============= NAB Score evaluating function ================
nab_evaluate <- function(test,events,reference,beta=1){
  
  score_data <- reference
  colnames(score_data) <- c("timestamp","is.real.anomaly")
  if(is.null(events$time)) score_data$is.anomaly <- FALSE
  else score_data$is.anomaly <- score_data$timestamp %in% events$time
  score_data$value <- test[,2]
  
  NAB <- otsad::GetDetectorScore(score_data, print = TRUE, title = "")
  TP <- NAB$tp
  FP <- NAB$fp
  FN <- NAB$fn
  TN <- NAB$tn
  NAB_score <- round(NAB$standard,2)
  
  cat("NAB:\n")
  print(as.table(matrix(c(as.character(TRUE),as.character(FALSE),
                          round(TP,2),round(FP,2),
                          round(FN,2),round(TN,2)), nrow = 3, ncol = 2, byrow = TRUE,
                        dimnames = list(c("Detected", "TRUE","FALSE"),
                                        c("Events", "")))))
  P_nab <- round(TP/(TP+FP),2)
  R_nab <- round(TP/(TP+FN),2)
  F1_nab <- round((1+beta^2)*P_nab*R_nab/((beta^2 * P_nab)+R_nab),2)
  
  
  metrics <- data.frame(NAB=c(P_nab,R_nab,F1_nab,NAB_score))
  row.names(metrics) <- c("precision","recall","F1","NAB Score")
  
  cat("\n")
  print(metrics)
}
#============= Soft-Hard-NAB evaluating comparison function ================
soft_hard_nab_comparison <- function(test,events,reference, k=15, beta=1){
  cat("Hard:\n")
  print(hard_evaluate(events, reference, metric="confusion_matrix"))
  
  cat("\nSoft:\n")
  print(soft_evaluate(events, reference, metric="confusion_matrix"))
  
  cat("\nNAB:\n")
  score_data <- reference
  colnames(score_data) <- c("timestamp","is.real.anomaly")
  if(is.null(events$time)) score_data$is.anomaly <- FALSE
  else score_data$is.anomaly <- score_data$timestamp %in% events$time
  score_data$value <- test[,2]
  
  NAB <- otsad::GetDetectorScore(score_data, print = TRUE, title = "")
  TP_nab <- NAB$tp
  FP_nab <- NAB$fp
  FN_nab <- NAB$fn
  TN_nab <- NAB$tn
  NAB_score <- round(NAB$standard,2)
  
  print(as.table(matrix(c(as.character(TRUE),as.character(FALSE),
                          round(TP_nab,2),round(FP_nab,2),
                          round(FN_nab,2),round(TN_nab,2)), nrow = 3, ncol = 2, byrow = TRUE,
                        dimnames = list(c("Detected", "TRUE","FALSE"),
                                        c("Events", "")))))
  
  Ps <- round(soft_evaluate(events, reference, k=k, metric="precision"),2)
  P <- round(hard_evaluate(events, reference, metric="precision"),2)
  P_nab <- round(TP_nab/(TP_nab+FP_nab),2)
  
  Rs <- round(soft_evaluate(events, reference, k=k, metric="recall"),2)
  R <- round(hard_evaluate(events, reference, metric="recall"),2)
  R_nab <- round(TP_nab/(TP_nab+FN_nab),2)
  
  
  F1s <- round(soft_evaluate(events, reference, k=k, metric="F1"),2)
  F1 <- round(hard_evaluate(events, reference, metric="F1"),2)
  F1_nab <- round((1+beta^2)*P_nab*R_nab/((beta^2 * P_nab)+R_nab),2)
  
  metrics <- data.frame(hard=c(P,R,F1,NA),soft=c(Ps,Rs,F1s,NA),NAB=c(P_nab,R_nab,F1_nab,NAB_score))
  row.names(metrics) <- c("precision","recall","F1","NAB Score")
  
  cat("\n")
  print(metrics)
}



#---------- Example 3 ------------
# === NMR dataset ===
rank <- 1
#K of 6 months
example_series <- top_data_diffs_tmn[["6"]][["F1"]][["diffNaN"]][,1:5]
reference <- ref_tmn[[1]][[example_series$var[rank]]]
names(reference) <- c("time","event")
reference$event <- as.logical(reference$event)
serie <- trend[[example_series$var[rank]]]


data <- cbind(serie,reference$event)
names(data) <- c("date","serie","event")

ggplot(data_t, aes(x=date,y=serie))+
  geom_line()+
  geom_point(aes(x=date,y=ifelse(event,serie,NA)))+
  theme_bw()+
  labs(x="Time", y = "")+
  scale_y_continuous(expand = expansion(mult = c(0, .2)))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())