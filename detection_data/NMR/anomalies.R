source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTSRegression.R")

#loadlibrary("cluster")


ts.anomalies.boxplot <- function(data, alpha=1.5)
{
  data <- data.frame(data)
  org <- nrow(data)
  cond <- rep(FALSE, org)
  if (org >= 30) {
    #data <- na.omit(data)
    i <- ncol(data)
    q <- quantile(data[,i], na.rm=TRUE)
    IQR <- q[4] - q[2]
    lq1 <- q[2] - alpha*IQR
    hq3 <- q[4] + alpha*IQR
    cond <- data[,i] < lq1 | data[,i] > hq3
  }
  return (cond)
}

ts.anomalies.an <- function(x, k, alpha=1.5) {
  sx <- ts_sw(x,k)
  ma <- apply(sx, 1, mean)
  sxd <- sx - ma
  iF <- ts.anomalies.boxplot(sxd,alpha)
  iF <- c(rep(NA, k-1), iF)
  
  sx <- ts_sw(rev(x),k)
  ma <- apply(sx, 1, mean)
  sxd <- sx - ma
  iB <- ts.anomalies.boxplot(sxd,alpha)
  iB <- c(rep(NA, k-1), iB)
  iB <- rev(iB)
  
  
  i <- iF & iB
  i[1:k] <- iB[1:k]
  i[(length(x)-k+1):length(x)] <- iF[(length(x)-k+1):length(x)]
  return(i)
}

ts.anomalies.kmeans <- function(x, alpha=1.5) {
  sx <- ts_sw(x, 3)
  pos <- rep(FALSE, nrow(sx))
  sx <- na.omit(data.frame(sx))
  clu <- kmeans(x = sx, centers = 1)
  clud  <- rowSums(sx - clu$centers[clu$cluster,])^2
  bp <- ts.anomalies.boxplot(clud,alpha)
  pos[as.integer(rownames(sx))[bp]] <- TRUE
  return(pos)
}


ts.anomalies.dbscan <- function(x)
{
  sx <- ts_sw(x, 3)
  pos <- rep(FALSE, nrow(sx))
  sx <- na.omit(data.frame(sx))
  eps <- mean(abs(sx$t0 - sx$t1))
  MinPts <- 3 * 2
  
  require("fpc")
  clu <- fpc::dbscan(sx, eps = eps, MinPts = MinPts)
  bp <- clu$cluster == 0
  pos[as.integer(rownames(sx))[bp]] <- TRUE
  return(pos)
}

ts.anomalies.ml <- function(x, ml_model, sw_size, input_size)
{
  sw <- ts_sw(x, sw_size)
  preprocess <- ts_gminmax()
  model <- ml_model(preprocess, input_size=input_size)
  model <- ts_train(model, sw)
  y <- (model$train_value-model$train_pred)^2
  y <- data.frame(y)
  bp <- c(rep(FALSE,sw_size-1), ts.anomalies.boxplot(y))
  return(bp)
}


evtdet.anomaly <- function(data, func, ...){
  
  serie_name <- names(data)[-1]
  names(data) <- c("time","serie")  
  serie <- data$serie
  
  #Returns index of outlier observations
  events <- evtdet(serie, func, ...)
  
  #Returns index of outlier observations
  index.out <- which(events)
  
  anomalies <- cbind.data.frame(time=data[index.out,"time"],
                                serie=serie_name,
                                type="anomaly")
  names(anomalies) <- c("time","serie","type")
  
  return(anomalies)
}