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