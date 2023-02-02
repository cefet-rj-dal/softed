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


soft_scores <- function(detection, events, k=15){
  
  E <- which(events)
  m <- length(E)
  
  D <- which(detection)
  n <- length(D)
  
  mu <- function(j,i,E,D,k) max(min( (D[i]-(E[j]-k))/k, ((E[j]+k)-D[i])/k ), 0)
  
  Mu <- matrix(NA,nrow = n, ncol = m)
  for(j in 1:m) for(i in 1:n) Mu[i,j] <- mu(j,i,E,D,k)
  
  E_d <- list()
  for(i in 1:n) E_d[[i]] <- which(Mu[i,] == max(Mu[i,]))
  
  D_e <- list()
  for(j in 1:m) D_e[[j]] <- which(sapply(1:n, function(i) j %in% E_d[[i]] & Mu[i,j] > 0)) 
  
  d_e <- c()
  for(j in 1:m) {
    if(length(D_e[[j]])==0) d_e[j] <- NA
    else d_e[j] <- D_e[[j]][which.max(sapply(D_e[[j]], function(i) Mu[i,j]))]
  }
  
  S_e <- c()
  for(j in 1:m) {
    if(length(D_e[[j]])==0) S_e[j] <- NA
    #else S_e[j] <- sum(sapply(D_e[[j]], function(i) Mu[i,j])) / length(D_e[[j]]) #mean
    else S_e[j] <- max(sapply(D_e[[j]], function(i) Mu[i,j]))  #max
  }
  
  S_d <- c()
  for(i in 1:n) S_d[i] <- max(S_e[which(d_e == i)], 0)
  
  return(S_d)
}


soft_metrics <- function(detection,events,k=15,beta=1){
  
  softScores <- soft_scores(detection, events, k=k)
  
  m <- length(which(events))
  n <- length(which(detection))
  t <- length(events)
  
  TPs <- sum(softScores)
  FPs <- sum(1-softScores)
  FNs <- m-TPs
  TNs <- (t-m)-FPs
  
  return(metrics(TPs,FPs,FNs,TNs, beta=beta))
}



#==== soft_evaluate: Function for soft evaluating quality of event detection ====
# input:
#   events: output from 'evtdet' function regarding a particular times series. 
#           A data.frame with three variables: time (events time/indexes), serie (corresponding time serie), type (event type)
#   reference: data.frame of the same length as the time series with two variables: time, event (boolean indicating true events)
#
# output:
#   calculated metric value.
soft_evaluate <- function(events, reference, k=15,
                     metric=c("confusion_matrix","accuracy","sensitivity","specificity","pos_pred_value","neg_pred_value","precision",
                              "recall","F1","prevalence","detection_rate","detection_prevalence","balanced_accuracy"), beta=1){
  #browser()
  if(is.null(events) | is.null(events$time)) stop("No detected events were provided for evaluation",call. = FALSE)
  
  names(reference) <- c("time","event")
  detected <- cbind.data.frame(time=reference$time,event=0)
  detected[detected$time %in% events$time, "event"] <- 1
  reference_vec <- as.logical(reference$event)
  detected_vec <- as.logical(detected$event)
  
  softMetrics <- soft_metrics(detected_vec,reference_vec,k=k,beta=beta)
  
  if(is.null(metric)) return(softMetrics)
  else metric <- match.arg(metric)
  
  metric_value <- switch(metric,
           "confusion_matrix" = softMetrics$confMatrix,
           "accuracy" = softMetrics$accuracy,
           "sensitivity" = softMetrics$sensitivity,
           "specificity" = softMetrics$specificity,
           "pos_pred_value" = softMetrics$PPV,
           "neg_pred_value" = softMetrics$NPV,
           "precision" = softMetrics$precision,
           "recall" = softMetrics$recall,
           "F1" = softMetrics$F1,
           "prevalence" = softMetrics$prevalence,
           "detection_rate" = softMetrics$detection_rate,
           "detection_prevalence" = softMetrics$detection_prevalence,
           "balanced_accuracy" = softMetrics$balanced_accuracy)
  
  return(metric_value)
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
#   events: output from 'evtdet' function regarding a particular times series.
#   reference: data.frame of the same length as the time series with two variables: time, event (boolean indicating true events)
#
# output:
#   calculated metric value.
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
#   events: output from 'evtdet' function regarding a particular times series. 
#           A data.frame with three variables: time (events time/indexes), serie (corresponding time serie), type (event type)
#   reference: data.frame of the same length as the time series with two variables: time, event (boolean indicating true events)
#
# output:
#   calculated metric value.
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