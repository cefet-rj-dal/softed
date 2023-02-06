metrics <- function(TP,FP,FN,TN, beta=1){
  confMatrix <- as.table(matrix(c(as.character(TRUE),as.character(FALSE),
                                  round(TP,2),round(FP,2),
                                  round(FN,2),round(TN,2)), nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("Detected", "TRUE","FALSE"),
                                                c("Events", ""))))
  
  accuracy <- (TP+TN)/(TP+FP+FN+TN)
  sensitivity <- TP/(TP+FN)
  specificity <- TN/(FP+TN)
  prevalence <- (TP+FN)/(TP+FP+FN+TN)
  PPV <- (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
  NPV <- (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
  detection_rate <- TP/(TP+FP+FN+TN)
  detection_prevalence <- (TP+FP)/(TP+FP+FN+TN)
  balanced_accuracy <- (sensitivity+specificity)/2
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  
  F1 <- (1+beta^2)*precision*recall/((beta^2 * precision)+recall)
  
  s_metrics <- list(TP=TP,FP=FP,FN=FN,TN=TN,confMatrix=confMatrix,accuracy=accuracy,
                    sensitivity=sensitivity, specificity=specificity,
                    prevalence=prevalence, PPV=PPV, NPV=NPV,
                    detection_rate=detection_rate, detection_prevalence=detection_prevalence,
                    balanced_accuracy=balanced_accuracy, precision=precision,
                    recall=recall, F1=F1)
  
  return(s_metrics)
}


nab_metrics <- function(score_data, beta=1,...){
  
  require("otsad")
  NAB <- otsad::GetDetectorScore(score_data, print = FALSE, title = "",...)
  TP_nab <- NAB$tp
  FP_nab <- NAB$fp
  FN_nab <- NAB$fn
  TN_nab <- NAB$tn
  NAB_score <- round(NAB$standard,2)
  
  return(c(metrics(TP_nab,FP_nab,FN_nab,TN_nab, beta=beta),list(NAB_score=NAB_score)))
}



#==== soft_evaluate: Function for soft evaluating quality of event detection ====
# input:
#   events: A data.frame with at least one variables: time (events time/indexes)
#   reference: data.frame of the same length as the time series with two variables: time, event (boolean indicating true events)
#
# output:
#   calculated metrics values.
nab_evaluate <- function(events, reference, serie,
                          metric=c("confusion_matrix","accuracy","sensitivity","specificity","pos_pred_value","neg_pred_value","precision",
                                   "recall","F1","prevalence","detection_rate","detection_prevalence","balanced_accuracy","NAB_score"), beta=1,...){
  #browser()
  if(is.null(events) | is.null(events$time)) stop("No detected events were provided for evaluation",call. = FALSE)
  
  score_data <- reference
  colnames(score_data) <- c("timestamp","is.real.anomaly")
  if(is.null(events$time)) score_data$is.anomaly <- FALSE
  else score_data$is.anomaly <- score_data$timestamp %in% events$time
  score_data$value <- serie
  
  
  nabMetrics <- nab_metrics(score_data,beta=beta,...)
  
  if(is.null(metric)) return(nabMetrics)
  else metric <- match.arg(metric)
  
  metric_value <- switch(metric,
                         "confusion_matrix" = nabMetrics$confMatrix,
                         "accuracy" = nabMetrics$accuracy,
                         "sensitivity" = nabMetrics$sensitivity,
                         "specificity" = nabMetrics$specificity,
                         "pos_pred_value" = nabMetrics$PPV,
                         "neg_pred_value" = nabMetrics$NPV,
                         "precision" = nabMetrics$precision,
                         "recall" = nabMetrics$recall,
                         "F1" = nabMetrics$F1,
                         "prevalence" = nabMetrics$prevalence,
                         "detection_rate" = nabMetrics$detection_rate,
                         "detection_prevalence" = nabMetrics$detection_prevalence,
                         "balanced_accuracy" = nabMetrics$balanced_accuracy,
                         "NAB_score" = nabMetrics$NAB_score)
  
  return(metric_value)
}