loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("TSPred")
loadlibrary("nnet")
loadlibrary("e1071")
loadlibrary("nnfor")
loadlibrary("randomForest")
loadlibrary("elmNNRcpp")
loadlibrary("RSNNS")
loadlibrary("fpc")
loadlibrary("dplyr")
loadlibrary("tfdatasets")
loadlibrary("tensorflow")
loadlibrary("keras")


# basic functions

ts_sw <- function(x, sw_size) {
  ts_lag <- function(x, k) 
  {
    c(rep(NA, k), x)[1 : length(x)] 
  }
  n <- length(x)-sw_size+1
  sw <- NULL
  for(c in (sw_size-1):0){
    t  <- ts_lag(x,c)
    t <- t[sw_size:length(t)]
    sw <- cbind(sw,t,deparse.level = 0)
  }
  col <- paste("t",c((sw_size-1):0), sep="")
  #rownames(sw) <- NULL
  colnames(sw) <- col
  return(sw)  
}

ts_as_matrix <- function(sw, size) {
  sw <- data.frame(sw)
  sw <- sw[, (ncol(sw)-size+1):ncol(sw)]
  sw <- as.matrix(sw)
  return(sw)
}

outliers.boxplot <- function(x, alpha = 1.5)
{
  ismatrix <- is.matrix(x)
  if(ismatrix || is.data.frame(x)) {
    x <- data.frame(x)
    x <- na.omit(x)
    org <- nrow(x)
    if (org >= 30) {
      q <- as.data.frame(lapply(x, quantile, na.rm=TRUE))
      n <- ncol(x)
      for (i in 1:n)
      {
        IQR <- q[4,i] - q[2,i]
        lq1 <- q[2,i] - alpha*IQR
        hq3 <- q[4,i] + alpha*IQR
        cond <- x[,i] >= lq1 & x[,i] <= hq3
        x <- x[cond,]
      }
    }
    if (ismatrix)
      x <- as.matrix(x)
  }
  else if (length(x) >= 30) {
    q <- quantile(x)
    IQR <- q[4] - q[2]
    lq1 <- q[2] - alpha*IQR
    hq3 <- q[4] + alpha*IQR
    cond <- x >= lq1 & x <= hq3
    x <- x[cond]
  }
  return (x)
}

ts_train_test <- function(x, test_size, sw_size = 0, offset=0) {
  if (offset == 0) {
    offset <- length(x)-test_size
  }
  if (sw_size == 0) {
    train <- x[1:offset]
    test <- x[(offset+1):(offset+test_size)]
  }
  else {
    train <- x[1:offset]
    test <- x[(offset-(sw_size-1)+1):(offset+test_size)]
    train <- ts_sw(train, sw_size)
    test <- ts_sw(test, sw_size)
  }
  return(list(train=train, test=test))
}

ts_sw_project <- function(sw) 
{
  if (is.vector(sw)) {
    input <- sw
    output <- sw
  }
  else {
    input <- sw[,1:ncol(sw)-1]
    output <- sw[,ncol(sw)]
  }
  return (list(input=input, output=output))
} 

# time series preprocessing

# classe ts_preprocess

ts_preprocess <- function() {
  value <- list(sw_size = NA, scale = 1, offset = 0, rescale = FALSE)
  attr(value, "class") <- "ts_preprocess"
  return(value)
}


ts_setup <- function(obj, x) {
  #x contains both input and output
  UseMethod("ts_setup")
}

ts_setup.default <- function(obj, x) {
  if (is.vector(x)) 
    obj$sw_size <- 0
  else
    obj$sw_size <- ncol(x)
  return(obj)
}

ts_normalize <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  UseMethod("ts_normalize")
}

ts_normalize.default <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return(list(x=x,arguments=NULL))
}

ts_denormalize <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  UseMethod("ts_denormalize")
}

ts_denormalize.default <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return(list(x=x,arguments=NULL))
}

# classe ts_gminmax

ts_gminmax <- function() {
  value <- ts_preprocess()
  value$gmin <- NaN
  value$gmax <- NaN
  value$rescale <- TRUE
  class(value) <- append("ts_gminmax", class(value))  
  return(value)
}

ts_setup.ts_gminmax <- function(obj, x) {
  obj <- ts_setup.default(obj, x)
  
  x <- outliers.boxplot(x)
  
  io <- ts_sw_project(x)
  
  obj$gmin <- min(x)
  obj$gmax <- max(x)
  
  if (obj$rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(x, 1, min)
    swio_max <- apply(x, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- outliers.boxplot(ratio)
    ratio <- mean(ratio)
    
    w <- (obj$gmax - obj$gmin)/(2*ratio)
    c <- (obj$gmax + obj$gmin)/2
    obj$gmax <- c + w
    obj$gmin <- c - w
  }
  
  return(obj)
}

ts_normalize.ts_gminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return (list(x = obj$scale*(x-obj$gmin)/(obj$gmax-obj$gmin) + obj$offset, arguments = NULL))
}

ts_denormalize.ts_gminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  return (list(x=((x - obj$offset) * (obj$gmax-obj$gmin) + obj$gmin), arguments=NULL))
}

# classe ts_diff

ts_gminmax_diff <- function() {
  value <- ts_preprocess()
  value$gmin <- NaN
  value$gmax <- NaN
  value$rescale <- TRUE
  class(value) <- append("ts_gminmax_diff", class(value))  
  return(value)
}

ts_diff <- function(x) {
  isvector <- !(is.matrix(x) || is.data.frame(x))
  if(isvector) {
    x <- ts_sw(x,2)
  }
  x <- x[,2:ncol(x)] - x[,1:(ncol(x)-1)]
  return(x)
}

ts_setup.ts_gminmax_diff <- function(obj, x) {
  obj <- ts_setup.default(obj, x)
  
  x <- ts_diff(x)
  
  x <- outliers.boxplot(x)
  
  io <- ts_sw_project(x)
  
  obj$gmin <- min(x)
  obj$gmax <- max(x)
  
  if (obj$rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(x, 1, min)
    swio_max <- apply(x, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- outliers.boxplot(ratio)
    ratio <- mean(ratio)
    
    obj$offset <- obj$offset + (1 - ratio) * obj$scale / 2
    obj$scale <- obj$scale * ratio
  }
  
  return(obj)
}

ts_normalize.ts_gminmax_diff <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  if (is.vector(x)) {
    ref <- arguments$ref
    x <- x - ref
  }
  else {
    ref <- x[,ncol(x)]
    x <- ts_diff(x)
  }
  x <- obj$scale*(x-obj$gmin)/(obj$gmax-obj$gmin) + obj$offset
  return (list(x=x, arguments=list(ref=ref)))
}

ts_denormalize.ts_gminmax_diff <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$offset) * (obj$gmax-obj$gmin) / obj$scale + obj$gmin
  if (is.vector(x)) {
    ref <- arguments$ref
    x <- x + ref
  }
  else {
    ref <- arguments$ref
    x <- cbind(x, arguments$ref)
    for (i in (ncol(x)-1):1) {
      x[,i] <- x[,i+1]-x[,i]
    }
  }
  return (list(x=x, arguments=list(ref=ref)))
}

# classe ts_swminmax

ts_swminmax <- function() {
  value <- ts_preprocess()
  value$rescale <- TRUE
  class(value) <- append("ts_swminmax", class(value))  
  return(value)
}

ts_setup.ts_swminmax <- function(obj, x) {
  valid_range <- function(x, alpha=1.5) {
    q <- quantile(x)
    IQR <- q[4] - q[2]
    return(q[2] - alpha*IQR)
  }
  
  obj <- ts_setup.default(obj, x)
  
  x <- outliers.boxplot(x)
  
  io <- ts_sw_project(x)
  
  if (obj$rescale) {
    swi_min <- apply(io$input, 1, min)
    swi_max <- apply(io$input, 1, max)
    
    swio_min <- apply(x, 1, min)
    swio_max <- apply(x, 1, max)
    
    ratio <- (swi_max-swi_min)/(swio_max-swio_min)
    ratio <- outliers.boxplot(ratio)
    ratio <- valid_range(ratio)
    obj$offset <- obj$offset + (1 - ratio) * obj$scale / 2
    obj$scale <- obj$scale * ratio
  }
  
  return(obj)
}

ts_normalize.ts_swminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  if (is.vector(x)) {
    i_min <- arguments$i_min 
    i_max <- arguments$i_max
  }
  else {
    i_min <- apply(x, 1, min)
    i_max <- apply(x, 1, max)
  }
  return (list(x=obj$scale*(x-i_min)/(i_max-i_min) + obj$offset, arguments=list(i_min=i_min, i_max=i_max)))
}

ts_denormalize.ts_swminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$offset) * (arguments$i_max - arguments$i_min) / obj$scale + arguments$i_min
  return (list(x=x, arguments=arguments))
}

# classe ts_anminmax

ts_anminmax <- function() {
  value <- ts_preprocess()
  value$gmin <- NA
  value$gmax <- NA
  value$rescale <- TRUE
  class(value) <- append("ts_anminmax", class(value))  
  return(value)  
}

ts_inertia <- function(obj, x) {
  UseMethod("ts_inertia")
}

ts_inertia.default <- function(obj, x) {
  an <- apply(x, 1, mean)
  return(an)
}

ts_remove_inertia <- function(obj, x, an) {
  UseMethod("ts_remove_inertia")
}

ts_remove_inertia.default <- function(obj, x, an) {
  return(x / an)
}

ts_add_inertia <- function(obj, x, an) {
  UseMethod("ts_add_inertia")
}

ts_add_inertia.default <- function(obj, x, an) {
  return(x * an)
}

ts_setup.ts_anminmax <- function(obj, x) {
  obj <- ts_setup.default(obj, x)
  
  an <- ts_inertia(obj, ts_sw_project(x)$input)
  x <- ts_remove_inertia(obj, x, an)
  x <- cbind(x, an)
  x <- outliers.boxplot(x)
  x <- x[,1:(ncol(x)-1)]
  
  io <- ts_sw_project(x)
  
  obj$gmin <- min(io$input)
  obj$gmax <- max(io$input)
  
  if (obj$rescale) {
    ratio <- (obj$gmax-obj$gmin)/(max(x)-min(x))
    
    obj$offset <- obj$offset + (1 - ratio) * obj$scale / 2
    obj$scale <- obj$scale * ratio
  }
  return(obj)
}

ts_normalize.ts_anminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  if (is.vector(x)) {
    an <- arguments$an
    x <- ts_remove_inertia(obj, x, an)
  }
  else {
    an <- ts_inertia(obj, x)
    x <- ts_remove_inertia(obj, x, an)
  }
  return (list(x=obj$scale*(x - obj$gmin)/(obj$gmax - obj$gmin) + obj$offset, arguments=list(an=an)))
}

ts_denormalize.ts_anminmax <- function(obj, x, arguments=NULL) {
  # x can be either input matrix or output vector
  x <- (x - obj$offset) * (obj$gmax - obj$gmin) / (obj$scale) + obj$gmin
  x <- ts_add_inertia(obj, x, arguments$an)
  return (list(x=x, arguments=arguments))
}

# classe ts_animinmax

ts_animinmax <- function() {
  value <- ts_anminmax()
  value$gmin <- NA
  value$gmax <- NA
  value$rescale <- TRUE
  class(value) <- append("ts_animinmax", class(value))  
  return(value)  
}

ts_remove_inertia.ts_animinmax <- function(obj, x, an) {
  return(x - an)
}

ts_add_inertia.ts_animinmax <- function(obj, x, an) {
  return(x + an)
}

ts_aneminmax <- function() {
  value <- ts_anminmax()
  value$gmin <- NA
  value$gmax <- NA
  value$rescale <- TRUE
  class(value) <- append("ts_aneminmax", class(value))  
  return(value)  
}

ts_inertia.ts_aneminmax <- function(obj, x) {
  exp_mean <- function(x) {
    n <- length(x)
    y <- rep(0,n)
    alfa <- 1 - 2.0 / (n + 1);
    for (i in 0:(n-1)) {
      y[n-i] <- alfa^i
    }
    m <- sum(y * x)/sum(y)
    return(m)
  }
  
  an <- apply(x, 1, exp_mean)
  return(an)
}


# time series models

ts_model <- function() {
  obj <- list(mdl=NULL)
  attr(obj, "class") <- "ts_model"
  return(obj)
}

ts_invoke_train <- function(obj, X, Y = NULL, arguments = NULL) {
  UseMethod("ts_invoke_train")
}

ts_invoke_train.default <- function(obj, X, Y = NULL, arguments = NULL) {
  obj$mdl <- NULL
  return(obj)
}

ts_train <- function(obj, x, arguments = NULL) {
  UseMethod("ts_train")
}

ts_train.default <- function(obj, x) {
  return(obj)  
}

ts_invoke_predict <- function(obj, X) {
  UseMethod("ts_invoke_predict")
}

ts_invoke_predict.default <- function(obj, X) {
  prediction <- predict(obj$mdl, X)
  return(prediction)
}

ts_predict <- function(obj, X, steps_ahead=1) {
  UseMethod("ts_predict")
}

ts_predict.default <- function(obj, X, steps_ahead=1) {
  return(NULL)
}

ts_test <- function(obj, test, steps_ahead=1) {
  UseMethod("ts_test")
}

ts_test.default <- function(obj, test, steps_ahead=1) {
  return(obj)
}

#class ts_dir

ts_dir <- function() {
  obj <- ts_model()
  class(obj) <- append("ts_dir", class(obj))  
  return(obj)
}

ts_train.ts_dir <- function(obj, x) {
  set.seed(1)
  
  obj <- ts_invoke_train(obj, x)
  
  obj$train_smape <- TSPred::sMAPE(obj$train_value, obj$train_pred)  
  obj$train_mse <- TSPred::MSE(obj$train_value, obj$train_pred)  
  return(obj)
}


ts_predict.ts_dir <- function(obj, X=NULL, steps_ahead=1) {
  if (is.vector(X))
    steps_ahead <- length(X)
  pred <- forecast(obj$mdl, h = steps_ahead)
  return(pred$mean)
}

ts_test.ts_dir <- function(obj, test, steps_ahead=1) {
  prediction <- ts_predict(obj, steps_ahead=steps_ahead)
  
  obj$test_pred <- prediction
  obj$test_value <- test
  
  obj$test_smape <- TSPred::sMAPE(obj$test_value, obj$test_pred)  
  obj$test_mse <- TSPred::MSE(obj$test_value, obj$test_pred)  
  
  return(obj)
}


#class ts_arima

ts_arima <- function() {
  obj <- ts_dir()
  class(obj) <- append("ts_arima", class(obj))  
  return(obj)
}

ts_invoke_train.ts_arima <- function(obj, X, Y = NULL, arguments = NULL) {
  obj$mdl <- auto.arima(X, allowdrift = TRUE, allowmean = TRUE) 
  obj$train_pred <- obj$mdl$residuals + X
  obj$train_value <- X
  return(obj)
}

#class ts_emlp_dir

ts_emlp_dir <- function(input_size) {
  obj <- ts_dir()
  obj$input_size <- input_size
  obj$difforder <- NA
  class(obj) <- append("ts_emlp_dir", class(obj))  
  return(obj)
}

ts_invoke_train.ts_emlp_dir <- function(obj, X, Y = NULL, arguments = NULL) {
  obj$mdl <- NULL
  X <- ts(X)
  max_smape <- -1
  for (j in 0:1) {
    for (i in 2:30) {
      mlmodel <- nnfor::mlp(X, hd = i, lags=1:obj$input_size, sel.lag=rep(FALSE, obj$input_size), difforder=j)
      smape <- mlmodel$MSE   
      if ((smape < max_smape) || is.null(obj$mdl)) {
        obj$mdl <- mlmodel
        obj$difforder <- j
        obj$hd <- i
        max_smape <- smape
      }
    }
  }
  start <- length(X)-length(obj$mdl$fitted)+1
  obj$train_pred <- obj$mdl$fitted
  obj$train_value <- X[start:length(X)]
  
  return(obj)
}

#class ts_eelm_dir

ts_eelm_dir <- function(input_size) {
  obj <- ts_dir()
  obj$input_size <- input_size
  obj$difforder <- NA
  class(obj) <- append("ts_eelm_dir", class(obj))  
  return(obj)
}

ts_invoke_train.ts_eelm_dir <- function(obj, X, Y = NULL, arguments = NULL) {
  obj$mdl <- NULL
  X <- ts(X)
  max_smape <- -1
  for (j in 0:1) {
    for (i in 2:30) {
      mlmodel <- nnfor::elm(X, hd = i, lags=1:obj$input_size, sel.lag=rep(FALSE, obj$input_size), difforder=j)
      smape <- mlmodel$MSE   
      if ((smape < max_smape) || is.null(obj$mdl)) {
        obj$mdl <- mlmodel
        obj$difforder <- j
        obj$hd <- i
        max_smape <- smape
      }
    }
  }
  start <- length(X)-length(obj$mdl$fitted)+1
  obj$train_pred <- obj$mdl$fitted
  obj$train_value <- X[start:length(X)]
  
  return(obj)
}

#class ts_swmodel

ts_swmodel <- function(preprocess, input_size) {
  obj <- ts_model()
  obj$preprocess <- preprocess
  obj$input_size <- input_size
  class(obj) <- append("ts_swmodel", class(obj))  
  return(obj)
}

ts_train.ts_swmodel <- function(obj, x) {
  set.seed(1)
  
  obj$preprocess <- ts_setup(obj$preprocess, x)
  
  io <- ts_sw_project(x)
  
  input <- ts_normalize(obj$preprocess, io$input)
  
  ouput <- ts_normalize(obj$preprocess, io$output, input$arguments)
  
  obj <- ts_invoke_train(obj, ts_as_matrix(input$x, obj$input_size), as.matrix(ouput$x), arguments)
  
  prediction <- ts_invoke_predict(obj, ts_as_matrix(input$x, obj$input_size))
  prediction <- ts_denormalize(obj$preprocess, prediction, input$arguments)
  
  obj$train_pred <- prediction$x
  obj$train_value <- io$output
  
  obj$train_smape <- TSPred::sMAPE(obj$train_value, obj$train_pred)  
  obj$train_mse <- TSPred::MSE(obj$train_value, obj$train_pred)  
  return(obj)
}

ts_predict.ts_swmodel <- function(obj, X, steps_ahead=1) {
  prediction <- NULL
  if (steps_ahead == 1) {
    input <- ts_normalize(obj$preprocess, X)
    pred <- ts_invoke_predict(obj, ts_as_matrix(input$x, obj$input_size))
    pred <- as.vector(pred)
    pred <- ts_denormalize(obj$preprocess, pred, input$arguments)
    prediction <- pred$x
  }
  else {
    X <- data.frame(X)[1,]
    for (i in 1:steps_ahead) {
      input <- ts_normalize(obj$preprocess, X)
      pred <- ts_invoke_predict(obj, ts_as_matrix(input$x, obj$input_size))
      pred <- as.vector(pred)
      pred <- ts_denormalize(obj$preprocess, pred, input$arguments)
      X[1,] <- c(X[1,2:ncol(X)], pred$x)
      prediction <- c(prediction, pred$x)
    }
  }
  return(prediction)
}

ts_test.ts_swmodel <- function(obj, test, steps_ahead=1) {
  io <- ts_sw_project(test)
  
  prediction <- ts_predict(obj, io$input, steps_ahead)
  
  obj$test_pred <- prediction
  obj$test_value <- io$output
  
  obj$test_smape <- TSPred::sMAPE(obj$test_value, obj$test_pred)  
  obj$test_mse <- TSPred::MSE(obj$test_value, obj$test_pred)  
  
  return(obj)
}

#class ts_nnet

ts_nnet <- function(preprocess, input_size) {
  obj <- ts_swmodel(preprocess, input_size)
  obj$maxit <- 50000
  class(obj) <- append("ts_nnet", class(obj))  
  return(obj)
}

ts_invoke_train.ts_nnet <- function(obj, X, Y, arguments = NULL) {
  tuned <- tune(nnet, X, Y, maxit=obj$maxit, trace=FALSE, ranges=list(decay=seq(0, 1, 0.01), size=(1:ncol(X))))
  obj$mdl <- tuned$best.model
  return(obj)
}

#class ts_svm

ts_svm <- function(preprocess, input_size) {
  obj <- ts_swmodel(preprocess, input_size)
  class(obj) <- append("ts_svm", class(obj))  
  return(obj)
}

ts_invoke_train.ts_svm <- function(obj, X, Y, arguments = NULL) {
  tuned <- tune(svm, X, Y, ranges=list(epsilon=seq(0,1,0.1), cost=1:100))
  obj$mdl <- tuned$best.model
  return(obj)
}


#class ts_rf

ts_rf <- function(preprocess, input_size) {
  obj <- ts_swmodel(preprocess, input_size)
  obj$ntree <- 500
  class(obj) <- append("ts_rf", class(obj))  
  return(obj)
}

ts_invoke_train.ts_rf <- function(obj, X, Y, arguments = NULL) {
  tuned <- tune(randomForest, X, Y, importance=TRUE, ranges=list(ntree=seq(10, obj$ntree, 10)))
  obj$mdl <- tuned$best.model
  return(obj)
}



#class ts_mlp

ts_mlp <- function(preprocess, input_size) {
  obj <- ts_swmodel(preprocess, input_size)
  obj$maxit <- 50000
  class(obj) <- append("ts_mlp", class(obj))  
  return(obj)
}

ts_invoke_train.ts_mlp <- function(obj, X, Y, arguments = NULL) {
  obj$mdl <- NULL
  max_smape <- -1
  for (i in 1:ncol(X)) {
    mlmodel <- RSNNS::mlp(X, Y, size=i, maxit=obj$maxit, learnFuncParams=c(0.1))
    smape <- TSPred::sMAPE(Y, mlmodel$fitted.values)    
    if ((smape < max_smape) || is.null(obj$mdl)) {
      max_smape <- smape
      obj$mdl <- mlmodel
      obj$size <- i
    }
  }
  for (j in seq(0.02, 0.2, 0.02)) {
    mlmodel <- RSNNS::mlp(X, Y, size=obj$size, maxit=obj$maxit, learnFuncParams=c(j))
    smape <- TSPred::sMAPE(Y, mlmodel$fitted.values)    
    if ((smape < max_smape) || is.null(obj$mdl)) {
      max_smape <- smape
      obj$mdl <- mlmodel
      obj$learn <- j
    }
  }
  return(obj)
}


#class ts_elm

ts_elm <- function(preprocess, input_size) {
  obj <- ts_swmodel(preprocess, input_size)
  class(obj) <- append("ts_elm", class(obj))  
  return(obj)
}

ts_invoke_train.ts_elm <- function(obj, X, Y, arguments = NULL) {
  obj$mdl <- NULL
  max_smape <- -1
  for (i in 1:ncol(X)) {
    mlmodel <- elm_train(X, Y, nhid = i, actfun = 'purelin', init_weights = "uniform_positive", bias = FALSE, verbose = FALSE)
    smape <- TSPred::sMAPE(Y, mlmodel$predictions)    
    if ((smape < max_smape) || is.null(obj$mdl)) {
      max_smape <- smape
      obj$mdl <- mlmodel
    }
  }
  return(obj)
}

ts_invoke_predict.ts_elm <- function(obj, X) {
  prediction <- elm_predict(obj$mdl, X)
  return(prediction)
}



#class ts_tensor_cnn

ts_tensor_cnn <- function(preprocess, input_size) {
  obj <- ts_swmodel(preprocess, input_size)
  obj$cnn_epochs <- 2000
  class(obj) <- append("ts_tensor_cnn", class(obj))  
  return(obj)
}

ts_invoke_train.ts_tensor_cnn <- function(obj, X, Y, arguments = NULL) {
  build_model <- function(train_df) {
    set.seed(1)
    
    spec <- feature_spec(train_df, t0 ~ . ) %>% 
      step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
      fit()
    
    input <- layer_input_from_dataset(train_df %>% select(-t0))
    
    output <- input %>% 
      layer_dense_features(dense_features(spec)) %>% 
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1) 
    
    model <- keras_model(input, output)
    
    model %>% 
      compile(
        loss = "mse",
        optimizer = optimizer_rmsprop(),
        metrics = list("mean_absolute_error")
      )
    
    return(model)
  }
  
  obj$mdl <- NULL
  
  XY <- data.frame(X)
  XY$t0 <- Y
  
  model <- build_model(XY)
  
  print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      if (epoch %% 800 == 0) cat("\n")
      if (epoch %% 10 == 0) cat(".")
    }
  )    
  
  history <- model %>% fit(
    x = XY %>% select(-t0),
    y = XY$t0,
    epochs = obj$cnn_epochs,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(print_dot_callback)
  )  
  cat("\n")
  
  obj$mdl <- model
  
  return(obj)
}

ts_invoke_predict.ts_tensor_cnn <- function(obj, X) {
  X <- data.frame(X)
  prediction <- (obj$mdl %>% predict(X))  
  return(prediction)
}


#class ts_tensor_lstm

ts_tensor_lstm <- function(preprocess, input_size) {
  obj <- ts_swmodel(preprocess, input_size)
  obj$lstm_epochs <- 2000
  class(obj) <- append("ts_tensor_lstm", class(obj))  
  return(obj)
}

ts_invoke_train.ts_tensor_lstm <- function(obj, X, Y, arguments = NULL) {
  obj$mdl <- NULL
  
  print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      if (epoch %% 800 == 0) cat("\n")
      if (epoch %% 10 == 0) cat(".")
    }
  )    
  
  set.seed(1)
  batch.size <- 1
  size <- ncol(X)
  
  X <- array(as.vector(X), dim=(c(dim(X),1)))
  
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = 100,
               input_shape = c(size, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_lstm(units = 50,
               return_sequences = FALSE,
               stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  model %>%
    compile(loss = 'mae', optimizer = 'adam')
  
  
  model %>% fit(x = X,
                y = Y,
                batch_size = batch.size,
                epochs = obj$lstm_epochs,
                verbose = 0,
                shuffle = FALSE,
                callbacks = list(print_dot_callback)
  )
  model %>% reset_states()
  cat("\n")
  
  obj$mdl <- model
  
  return(obj)
}

ts_invoke_predict.ts_tensor_lstm <- function(obj, X) {
  X <- array(as.vector(X), dim=(c(dim(X),1)))
  batch.size <- 1
  prediction <- obj$mdl %>% predict(X, batch_size = batch.size) %>% .[,1]
  return(prediction)
}


# utility functions


ts_plot_series <- function(y, yadj, ypre, modelname) {
  ntrain <- length(yadj)
  smape_train <- TSPred::sMAPE(y[1:ntrain], yadj)*100
  smape_test <- TSPred::sMAPE(y[(ntrain+1):(ntrain+length(ypre))], ypre)*100
  par(xpd=TRUE)      
  plot(1:length(y), y, main = modelname, xlab = sprintf("time [smape train=%.2f%%], [smape test=%.2f%%]", smape_train, smape_test), ylab="value")
  lines(1:ntrain, yadj, col="blue")  
  lines((ntrain+1):(ntrain+length(ypre)), ypre, col="green")  
  par(xpd=FALSE)      
}