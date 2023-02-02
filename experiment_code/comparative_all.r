source("exp_func_SoftED.r")
load("datasets_metrics.RData")

require(ggplot2)
require(RColorBrewer)
require(Cairo)

# 3W
#================ Metric comparison results - SoftED vs Hard =====================
data3w_results_soft <- k_results_3w(evaluate_metrics_3w, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft")
data3w_analysis_soft <- k_evt_exp_analysis_3w(data3w_results_soft, top=3)
#================ Metric comparison results - NAB vs Hard =====================
data3w_results_nab <- data_results_3w(evaluate_metrics_3w, metrics=c("F1","precision","recall"), rival="NAB")
data3w_analysis_nab <- all_evt_exp_analysis_3w(data3w_results_nab, top=3)

# NAB
#================ Metric comparison results - SoftED vs Hard =====================
NAB_results_soft <- k_results(evaluate_metrics_NAB, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft")
NAB_analysis_soft <- k_evt_exp_analysis(NAB_results_soft, top=3)
#================ Metric comparison results - NAB vs Hard =====================
NAB_results_nab <- data_results(evaluate_metrics_NAB, metrics=c("F1","precision","recall"), rival="NAB")
NAB_analysis_nab <- all_evt_exp_analysis(NAB_results_nab, top=3)

# Yahoo
#================ Metric comparison results - SoftED vs Hard =====================
yahoo_results_soft <- k_results(evaluate_metrics_yahoo, k_values=seq(15, 60, by = 15), metrics=c("F1","precision","recall"), rival="soft")
yahoo_analysis_soft <- k_evt_exp_analysis(yahoo_results_soft, top=3)
#================ Metric comparison results - NAB vs Hard =====================
yahoo_results_nab <- data_results(evaluate_metrics_yahoo, metrics=c("F1","precision","recall"), rival="NAB")
yahoo_analysis_nab <- all_evt_exp_analysis(yahoo_results_nab, top=3)

#TMN
#================ Metric comparison results - SoftED vs Hard =====================
tmn_results_soft <- k_results(evaluate_metrics_tmn, k_values=c(6,12,15,24,seq(15, 60, by = 15)), metrics=c("F1","precision","recall"), rival="soft")
tmn_analysis_soft <- k_evt_exp_analysis(tmn_results_soft, top=3)
#================ Metric comparison results - NAB vs Hard =====================
tmn_results_nab <- data_results(evaluate_metrics_tmn, metrics=c("F1","precision","recall"), rival="NAB")
tmn_analysis_nab <- all_evt_exp_analysis(tmn_results_nab, top=3)


datasets_analysis_soft <- list(NAB=NAB_analysis_soft$`15`$F1,
                               Yahoo=yahoo_analysis_soft$`15`$F1,
                               NMR=tmn_analysis_soft$`15`$F1,
                               '3W'=data3w_analysis_soft$`15`$F1)

datasets_analysis_nab <- list(NAB=NAB_analysis_nab$F1,
                               Yahoo=yahoo_analysis_nab$F1,
                               NMR=tmn_analysis_nab$F1,
                               '3W'=data3w_analysis_nab$F1)


datasets_s_analysis_soft <- list(NAB=NAB_analysis_soft,
                               Yahoo=yahoo_analysis_soft,
                               NMR=tmn_analysis_soft,
                               '3W'=data3w_analysis_soft)

datasets_s_analysis_nab <- list(NAB=NAB_analysis_nab,
                              Yahoo=yahoo_analysis_nab,
                              NMR=tmn_analysis_nab,
                              '3W'=data3w_analysis_nab)


datasets_results_soft <- list(NAB=NAB_results_soft,
                              Yahoo=yahoo_results_soft,
                              NMR=tmn_results_soft,
                              '3W'=data3w_results_soft)

datasets_metrics <- list(NAB=evaluate_metrics_NAB,
                         Yahoo=evaluate_metrics_yahoo,
                         NMR=evaluate_metrics_tmn,
                         '3W'=evaluate_metrics_3w)

get_perc_diff_datasets <- function(datasets_analysis,rival=c("SoftED","NAB")){
  perc_datasets <- data.frame()
  #browser()
  for(name_dataset in names(datasets_analysis)){
    dataset <- datasets_analysis[[name_dataset]]
    len_dataset <- sum(dataset$len_diff$value)
    perc_diff <- merge(dataset$perc_diff,dataset$len_diff,by="L1")
    names(perc_diff) <- c("L1","group","perc","total")
    perc_diff$value <- perc_diff$perc *  perc_diff$total
    
    perc_diff <- aggregate(perc_diff$value, list(group = perc_diff$group), sum)
    names(perc_diff) <- c("group","value")
    perc_diff$perc <- perc_diff$value/len_dataset
    perc_diff$dataset <- name_dataset
    perc_diff$rival <- rival
    if(name_dataset %in% c("NAB","Yahoo")) perc_diff$evt <- "anomalies"
    else if (name_dataset %in% c("NMR","3W")) perc_diff$evt <- "change points"
    
    perc_datasets <- rbind(perc_datasets,perc_diff)
  }
  return(perc_datasets)
}
get_perc_top_datasets <- function(datasets_analysis,k=15,rival=c("SoftED","NAB")){
  perc_datasets <- data.frame()
  #browser()
  for(name_dataset in names(datasets_analysis)){
    if(rival == "SoftED") dataset <- datasets_analysis[[name_dataset]][[as.character(k)]]$F1
    else dataset <- datasets_analysis[[name_dataset]]$F1
    len_dataset <- sum(dataset$len_top$value)
    perc_top <- merge(dataset$perc_top,dataset$len_top,by="L1")
    names(perc_top) <- c("L1","group","perc","total")
    perc_top$value <- perc_top$perc *  perc_top$total
    
    perc_top <- aggregate(perc_top$value, list(group = perc_top$group), sum)
    names(perc_top) <- c("group","value")
    perc_top$perc <- perc_top$value/len_dataset
    perc_top$dataset <- name_dataset
    perc_top$rival <- rival
    if(name_dataset %in% c("NAB","Yahoo")) perc_top$evt <- "anomalies"
    else if (name_dataset %in% c("NMR","3W")) perc_top$evt <- "change points"
    
    perc_datasets <- rbind(perc_datasets,perc_top)
  }
  return(perc_datasets)
}
get_perc_tops <- function(datasets_results,k=15,tops=c(1:3),rival="SoftED"){
  perc_tops <- data.frame()
  #browser()
  results_tops <- list()
  for(top in tops){
    results_tops[[top]] <- list()
    for(name_dataset in names(datasets_results)){
      results <- datasets_results[[name_dataset]]
      if(name_dataset == "3W") data_results <- k_evt_exp_analysis_3w(results, top=top)
      else  data_results <- k_evt_exp_analysis(results, top=top)
      results_tops[[top]][[name_dataset]] <- data_results
    }
  }
  for(top in tops){
    perc_tops <- rbind(perc_tops,cbind(top=top,get_perc_top_datasets(results_tops[[top]],k=k,rival=rival)))
  }
  
  return(perc_tops)
}
get_diff_datasets <- function(datasets_analysis,rival=c("SoftED","NAB")){
  diff_datasets <- data.frame()
  #browser()
  for(name_dataset in names(datasets_analysis)){
    dataset <- datasets_analysis[[name_dataset]]
    for(k in names(dataset)){
      if(!k %in% seq(15, 60, by = 15)) next
      for(metric in names(dataset[[k]])){
        mean_diff <- mean(dataset[[k]][[metric]]$diff_data$value)
        diff <- cbind(dataset=name_dataset,k=k,metric=metric,rival=rival,diff=mean_diff)
        diff_datasets <- rbind(diff_datasets,diff)
      }
    }
  }
  return(diff_datasets)
}
get_times_datasets <- function(datasets_metrics,k=c(15),time="elapsed",metrics=c("hard","soft","nab")){
  times_datasets <- data.frame()
  
  get_elements <- function(x, element) {
    if(is.list(x)){
      if(element %in% names(x)) x[[element]]
      else lapply(x, get_elements, element = element)
    }
  }
  get_times <- function(x, metric, k=15, time="elapsed") {
    if(is.list(x)){
      if(metric %in% names(x)) ifelse(metric=="soft",x[[metric]][[as.character(k)]][time],x[[metric]][time])
      else lapply(x, get_times, metric = metric)
    }
  }
  
  #browser()
  for(name_dataset in names(datasets_metrics)){
    dataset <- datasets_metrics[[name_dataset]]
    for(metric in metrics){
      mean_time <- dataset |>
        get_elements("time") |>
        get_times(metric, k=k, time=time)|>
        unlist() |>
        mean()
      times_datasets <- rbind(times_datasets,cbind(dataset=name_dataset,metric=metric,mean_time=mean_time))
    }
  }
  return(times_datasets)
}


perc_diff <- rbind(get_perc_diff_datasets(datasets_analysis_soft,rival="SoftED"),
                   get_perc_diff_datasets(datasets_analysis_nab,rival="NAB"))

perc_tops <- get_perc_tops(datasets_results_soft,tops=c(1:3),rival="SoftED")

mean_diffs <- rbind(get_diff_datasets(datasets_s_analysis_soft,rival="SoftED"),
                    get_diff_datasets(datasets_s_analysis_nab,rival="NAB"))

mean_times <- get_times_datasets(datasets_metrics,k=15,time="elapsed",metrics=c("hard","soft","nab"))


#=========== plots ============

# EXP 1.1
exp11 <- perc_diff[perc_diff$rival=="SoftED",]

#Percentage overall results
#tolerance incorporated
round(sum(exp11[exp11$group=="percH0" | exp11$group=="percNaNH",]$value)/sum(exp11$value),2)
#detections matched events
round(sum(exp11[exp11$group=="percE0",]$value)/sum(exp11$value),2)
#no close detections
round(sum(exp11[exp11$group=="percNaN2",]$value)/sum(exp11$value),2)
#evaluations made possible
round(sum(exp11[exp11$group=="percNaNH",]$value)/sum(exp11$value),2)

exp11_groups <- exp11[exp11$group=="percH0" | exp11$group=="percNaNH",]
exp11_groups <- aggregate(exp11_groups$perc, list(dataset = exp11_groups$dataset), sum)
exp11_groups$group <- "Incorporated tolerance"
exp11_groups_aux <- rbind(cbind(exp11[exp11$group=="percE0",c("dataset","perc")],group="Confirmed results"),
                          cbind(exp11[exp11$group=="percNaN2",c("dataset","perc")],group="Inaccurate results"))
names(exp11_groups_aux) <- c("dataset","x", "group")
exp11_groups <- rbind(exp11_groups,exp11_groups_aux)
exp11_groups$group <- factor(exp11_groups$group, levels = c("Inaccurate results","Confirmed results","Incorporated tolerance"))
exp11_groups$dataset <- factor(exp11_groups$dataset, levels = c("3W","NMR","NAB","Yahoo"))
exp11_groups$x  <- ifelse(exp11_groups$group == "Inaccurate results", -exp11_groups$x, exp11_groups$x)

p <- 
  ggplot(exp11_groups, aes(x=dataset, y=x, fill=group))+
  geom_bar(width = .6, stat = "identity", color="white")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  labs(x="Dataset", y = "SoftED method evaluations")+
  geom_text(aes(label = ifelse(x<0,scales::percent(-x,accuracy =1),"")), position = position_stack(vjust = 0.5))+
  geom_text(aes(label = ifelse(x<0,"",scales::percent(x,accuracy =1))), position = position_stack(vjust = 0.5))+
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(expand = expansion(mult = c(0.1, .1)))+
  scale_fill_manual(values=c("#4472C4",brewer.pal(9,"Greys")[4],brewer.pal(9,"Reds")[3]),name="",
                    breaks=c("Incorporated tolerance", "Confirmed results", "Inaccurate results"))+
  theme(legend.position="bottom")+
  coord_flip()

  
file_name <- paste0("plots/exp11.pdf")
CairoPDF(file_name,width=6,height=4)
print(p)
dev.off()

exp11_groups_aux <- rbind(cbind(exp11[exp11$group=="percH0",c("dataset","perc")],group="Increased precision/recall"),
                          cbind(exp11[exp11$group=="percNaNH",c("dataset","perc")],group="Reclaimed precision/recall"))
names(exp11_groups_aux) <- c("dataset","x", "group")
exp11_groups <- exp11_groups_aux
exp11_groups$group <- factor(exp11_groups$group, levels = c("Increased precision/recall","Reclaimed precision/recall"),
                             labels = c("Increased F1","Enabled F1 computation (previously n/a)"))
exp11_groups$dataset <- factor(exp11_groups$dataset, levels = c("3W","NAB","Yahoo","NMR"))

p <- 
  ggplot(exp11_groups, aes(x=dataset, y=x, fill=group))+
  geom_bar(width = .6, stat = "identity", color="white")+
  theme_bw()+
  labs(x="Dataset", y = "SoftED method evaluations")+
  geom_text(aes(label = ifelse(scales::percent(x,accuracy=1) == "1%", scales::percent(x,accuracy =0.1), scales::percent(x,accuracy =1))), position = position_stack(vjust = 0.5))+
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  scale_fill_manual(values=c("#a2b8e2","#698ed0"),name="")+
  theme(legend.position="bottom")

file_name <- paste0("plots/exp11_2.pdf")
CairoPDF(file_name,width=6,height=4)
print(p)
dev.off()


# EXP 1.2
exp12 <- perc_tops[perc_tops$rival=="SoftED",]

#Percentage overall results
exp12_top <- exp12[exp12$top==3,]
#climbed ranking
round(sum(exp12_top[exp12_top$group=="percNewTop" | exp12_top$group=="percTop",]$value)/sum(exp12_top$value),2)
#same ranking
round(sum(exp12_top[exp12_top$group=="percSameTop",]$value)/sum(exp12_top$value),2)
#dropped ranking
round(sum(exp12_top[exp12_top$group=="percLowerTop",]$value)/sum(exp12_top$value),2)


exp12 <- exp12[exp12$group=="percNewTop" | exp12$group=="percTop",]
exp12 <- aggregate(exp12$perc, list(dataset = exp12$dataset, top=exp12$top), sum)
exp12$top <- factor(exp12$top, levels = c(1,2,3), labels = c("Top 1", "Top 2", "Top 3"))
  
p <- 
  ggplot(exp12, aes(x=reorder(dataset,x), y=x, group=reorder(top,x)))+
  geom_errorbar(aes(ymin = 0, ymax = x),width = 0, color="black",position = position_dodge(width = .8)) +
  geom_point(aes(x=dataset, y=x, color=reorder(top,x)), shape=17, position=position_dodge(width = .8), size=4) +
  theme_bw()+
  labs(x="Dataset", y = "Detectors climbed (to) the top")+
  geom_text(aes(label = scales::percent(x,accuracy =1)), color="black",position=position_dodge(width = .8), vjust = -0.9)+
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(expand = expansion(mult = c(0, .2)))+
  scale_color_manual(values=c(brewer.pal(9,"Greys")[4],brewer.pal(9,"Greys")[5],brewer.pal(9,"Greys")[6]),name="")+
  theme(legend.position="bottom")

exp12 <- perc_tops[perc_tops$rival=="SoftED",]
exp12_groups <- exp12[exp12$group=="percNewTop" | exp12$group=="percTop",]
exp12_groups <- aggregate(exp12_groups$perc, list(dataset = exp12_groups$dataset, top=exp12_groups$top), sum)
exp12_groups$group <- "Climbed to/within the top"
exp12_groups_aux <- rbind(cbind(exp12[exp12$group=="percSameTop",c("dataset","perc","top")],group="Maintained position"),
                          cbind(exp12[exp12$group=="percLowerTop",c("dataset","perc","top")],group="Pushed down"))
names(exp12_groups_aux) <- c("dataset","x","top", "group")
exp12_groups <- rbind(exp12_groups,exp12_groups_aux)
exp12_groups$group <- factor(exp12_groups$group, levels = c("Pushed down","Maintained position","Climbed to/within the top"),
                             labels=c("Dropped to position","Maintained position","Climbed to position"))
exp12_groups$dataset <- factor(exp12_groups$dataset, levels = c("3W","NAB","Yahoo","NMR"))
exp12_groups$top <- factor(exp12_groups$top, levels = c(3,2,1), labels = c("Top 3", "Top 2", "Top 1"))

p <- 
  ggplot(exp12_groups, aes(x=top, y=x, group=reorder(top,x), fill=group))+
  geom_bar(width = .1, position="stack", stat = "identity", color="white")+
  #geom_errorbar(aes(ymin = 0, ymax = x),width = 0, color="black",position = position_dodge(width = .8)) +
  geom_point(aes(x=top, y=ifelse(group == "Climbed to position",x,NA)), color="#4472C4", shape=17, position=position_dodge(width = .8), size=4) +
  theme_bw()+
  facet_wrap(~ dataset, ncol=2)+
  labs(x="Dataset", y = "Method ranking changes")+
  geom_text(aes(label = ifelse(x > 0, ifelse(x < 0.1, paste0("0",scales::percent(x,accuracy =1)), scales::percent(x,accuracy =1)), "")), color="black",position=position_stack(vjust = .5), hjust = -0.5,size=3)+
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(expand = expansion(mult = c(.2, .2)))+
  scale_fill_manual(values=c("#4472C4",brewer.pal(9,"Greys")[5],brewer.pal(9,"Greys")[4]),name="",
                    breaks=c("Climbed to position", "Maintained position", "Dropped to position"))+
  theme(legend.position="bottom")+
  guides(fill = guide_legend(override.aes = list(shape = NA)))


file_name <- paste0("plots/exp12.pdf")
CairoPDF(file_name,width=6,height=4)
print(p)
dev.off()

  
# EXP 1.3
#F1 table
exp13_F1 <- mean_diffs[mean_diffs$rival=="SoftED" & mean_diffs$metric=="F1",]
exp13_F1$diff <- round(as.numeric(exp13_F1$diff),4)
reshape2::dcast(exp13_F1,formula = dataset~k,fun.aggregate = sum,value.var = "diff")

#precision recall plot
exp13 <- mean_diffs[mean_diffs$rival=="SoftED" & mean_diffs$metric!="F1",]
exp13$diff <- as.numeric(exp13$diff)

p <- ggplot(exp13, aes(x=reorder(dataset,diff), y=diff, fill=reorder(k, sort(as.numeric(k))))) + 
  geom_bar(position="dodge", stat = "identity")+
  facet_wrap(~ metric,labeller = labeller(metric = c("precision"="Precision","recall"="Recall")))+
  theme_bw()+
  labs(x="Datasets", y = paste0("SoftED - Hard metrics (mean)"))+
  scale_fill_manual(values=c(brewer.pal(9,"Greys")[4],brewer.pal(9,"Greys")[5],brewer.pal(9,"Greys")[6],brewer.pal(9,"Greys")[7]),name="k")+
  theme(legend.position="bottom")

file_name <- paste0("plots/exp13.pdf")
CairoPDF(file_name,width=6,height=3)
print(p)
dev.off()


# EXP 1.4
exp14 <- perc_diff
exp14 <- exp14[exp14$group=="percH0" | exp14$group=="percNaNH",]
exp14 <- aggregate(exp14$perc, list(dataset = exp14$dataset, rival=exp14$rival), sum)

p <- 
  ggplot(exp14, aes(x=reorder(dataset,-x), y=x, fill=rival))+
  geom_bar(width = .6, position="dodge", stat = "identity", color="white")+
  theme_bw()+
  labs(x="Dataset", y = "Incorporated temporal tolerance")+
  geom_text(aes(label = scales::percent(x,accuracy =1)), position=position_dodge(width = .6), vjust = -0.2)+
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  scale_fill_manual(values=c(brewer.pal(9,"Greys")[4],brewer.pal(9,"Greys")[5]),name="Approach")+
  theme(legend.position="bottom")

exp14 <- reshape2::dcast(exp14,formula = dataset~rival,fun.aggregate = sum,value.var = "x")
exp14$tol_diff <-  exp14$NAB-exp14$SoftED
p <- 
  ggplot(exp14, aes(x=reorder(dataset,-tol_diff), y=tol_diff))+
  geom_bar(width = .6, position="dodge", stat = "identity", color="white", fill=brewer.pal(9,"Greys")[4])+
  geom_hline(yintercept = 0)+
  theme_bw()+
  labs(x="Dataset", y = "NAB incorporated temporal tolerance")+
  geom_text(aes(label = paste0(ifelse(tol_diff>0,"+",""),scales::percent(tol_diff,accuracy =1)),
                y=tol_diff+ifelse(tol_diff>=0,0.01, -0.05)), position=position_dodge(width = .6), vjust = -0.2)+
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
  theme(legend.position="bottom")

file_name <- paste0("plots/exp14.pdf")
CairoPDF(file_name,width=6,height=3)
print(p)
dev.off()

exp14_times <- mean_times

roundUpToNearestMagnitude <- function(n) {
  if (n == 0) return(1)
  negative <- n < 0
  log = log10(abs(n))
  decimalPlaces = ifelse(log > 0, ceiling(log), floor(log) + 1)
  rounded = 10^decimalPlaces
  return(ifelse(negative, -rounded,rounded))
}

aggregate(exp14_times$mean_time, list(metric = exp14_times$metric), mean)
aggregate(exp14_times$mean_time, list(metric = exp14_times$metric), function(x) ceiling(log10(mean(x))))
aggregate(exp14_times$mean_time, list(metric = exp14_times$metric), function(x) roundUpToNearestMagnitude(mean(x)))


exp14_times$metric <- factor(exp14_times$metric, levels = c("hard","soft","nab"),
                             labels = c("hard","SoftED","NAB score"))
exp14_times$mean_time <- as.numeric(exp14_times$mean_time)
exp14_times$dataset <- factor(exp14_times$dataset, levels = c("3W","NAB","Yahoo","NMR"))

p <- 
  ggplot(exp14_times, aes(x=reorder(dataset,mean_time), y=mean_time, fill=metric, group=metric)) + 
  geom_bar(position="dodge", stat = "identity")+
  facet_wrap(~ dataset, scales = "free")+
  theme_bw()+
  labs(x="Datasets", y = "Computation time (mean)")+
  scale_fill_manual(values=c(brewer.pal(9,"Greys")[4],brewer.pal(9,"Greys")[5],brewer.pal(9,"Greys")[6]),name="Metrics")+
  theme(legend.position="bottom")

file_name <- paste0("plots/exp14_2.pdf")
CairoPDF(file_name,width=6,height=4)
print(p)
dev.off()