library(dplyr)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R")

load("../data/train.RData")
load("../data/trend.RData")
load("../data/reference.RData")

cnes <- names(train)[-1]
#tmn_evts <- lapply(cnes,function(s) filter(tabela_input_quando, serie==s))
#names(tmn_evts) <- cnes

#tmn_evts <- lapply(tmn_evts, function(var) list(SCP=var))
#tmn_evts <- list(TMN=tmn_evts)



reference[-1] <- as.integer(reference[-1] != 0)
ref_evts <- lapply(cnes,function(s) select(reference, date, contains(s)))
names(ref_evts) <- cnes
#ref_evts <- list(TMN=ref_evts)

train <- lapply(cnes,function(s) select(train, date, contains(s)))
names(train) <- cnes

trend <- lapply(cnes,function(s) select(trend, date, contains(s)))
names(trend) <- cnes