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


hard_metrics <- function(detection,events, beta=1){
  TP <- sum(detection & events)
  FP <- sum(detection & !events)
  FN <- sum(!detection & events)
  TN <- sum(!detection & !events)
  
  return(metrics(TP,FP,FN,TN, beta=beta))
}

#==== evaluate: Function for evaluating quality of event detection ====
# input:
#   events: A data.frame with at least one variables: time (events time/indexes)
#   reference: data.frame of the same length as the time series with two variables: time, event (boolean indicating true events)
#
# output:
#   calculated metrics values.
hard_evaluate <- function(events, reference, 
                     metric=c("confusion_matrix","accuracy","sensitivity","specificity","pos_pred_value","neg_pred_value","precision",
                              "recall","F1","prevalence","detection_rate","detection_prevalence","balanced_accuracy"), beta=1){
  #browser()
  if(is.null(events) | is.null(events$time)) stop("No detected events were provided for evaluation",call. = FALSE)
  
  names(reference) <- c("time","event")
  detected <- cbind.data.frame(time=reference$time,event=0)
  detected[detected$time %in% events$time, "event"] <- 1
  reference_vec <- as.logical(reference$event)
  detected_vec <- as.logical(detected$event)
  
  hardMetrics <- hard_metrics(detected_vec, reference_vec, beta=beta)
  
  if(is.null(metric)) return(hardMetrics)
  else metric <- match.arg(metric)
  
  metric_value <- switch(metric,
                         "confusion_matrix" = hardMetrics$confMatrix,
                         "accuracy" = hardMetrics$accuracy,
                         "sensitivity" = hardMetrics$sensitivity,
                         "specificity" = hardMetrics$specificity,
                         "pos_pred_value" = hardMetrics$PPV,
                         "neg_pred_value" = hardMetrics$NPV,
                         "precision" = hardMetrics$precision,
                         "recall" = hardMetrics$recall,
                         "F1" = hardMetrics$F1,
                         "prevalence" = hardMetrics$prevalence,
                         "detection_rate" = hardMetrics$detection_rate,
                         "detection_prevalence" = hardMetrics$detection_prevalence,
                         "balanced_accuracy" = hardMetrics$balanced_accuracy)
  
  return(metric_value)
}
