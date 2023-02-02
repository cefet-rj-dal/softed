ex <- example_series_3w[2,1:4]
ex <- as.data.frame(lapply(ex, as.character))

evts <- evts_3w[[ex$tipo]][[ex$well]][[ex$var]][[ex$method]]
ref <- ref_3w[[ex$tipo]][[ex$well]]
names(ref) <- c("time","event")

serie <- read.csv(paste0("../detection_data/3W_examples/",ex$well,".csv"))
#browser()
serie <- serie[c("timestamp",gsub("-",".",ex$var))]
serie$timestamp <- as.POSIXct(serie$timestamp,tz = "GMT")


evts <- ref
evts$event <- FALSE
#evts$event[which(ref$event==1)-1] <- TRUE
#evts$event[which(ref$event==1)-2] <- TRUE
#evts$event[which(ref$event==1)-3] <- TRUE
#evts$event[which(ref$event==1)+9] <- TRUE
#evts$event[which(ref$event==1)+10] <- TRUE
#evts$event[which(ref$event==1)+11] <- TRUE
#evts$event[which(ref$event==1)+12] <- TRUE
#evts$event[which(ref$event==1)-10] <- TRUE
evts$event[which(ref$event==1)] <- TRUE
#evts$event[which(ref$event==1)-40] <- TRUE
#evts$event[which(ref$event==1)-41] <- TRUE
evts <- evts[evts$event,]


#Plot
print(evtplot(serie,evts, ref))
#Evaluate
soft_hard_comparison(evts, ref)

#==== NAB score ====
score_data <- ref
colnames(score_data) <- c("timestamp","is.real.anomaly")
score_data$is.anomaly <- score_data$timestamp %in% evts$time
score_data$value <- serie[,2]
saida_nab <- otsad::GetDetectorScore(score_data, print = FALSE, title = "")
saida_nab
