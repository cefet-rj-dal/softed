library(ggplot2)

#========== Preparing the data ===========
questionnaire <- read.csv("D:/OneDrive/Artigos/2021-05-ASC-SoftMetric/experiment/quali_survey/questionnaire.csv", sep=";")
names(questionnaire) <- c("time","email","domain","workEvtDet",
                          "ex1_method","ex1_comment","ex1_metrics",
                          "ex2_method","ex2_comment","ex2_metrics",
                          "ex3_method","ex3_comment","ex3_metrics",
                          "ex4_method","ex4_comment","ex4_metrics",
                          "ex5_method","ex5_comment","ex5_metrics",
                          "ex6_method","ex6_comment","ex6_metrics")

questionnaire$specialist <- rep(0,nrow(questionnaire))
questionnaire$specialist[c(18,19,47,56,60,61,62,63,64,65,67,69,70)] <- 1

questionnaire$institution <- rep("CEFET/RJ",nrow(questionnaire))
questionnaire$institution[c(58,66,68)] <- "PCDaS"

questionnaire$institution[c(18,19,47)] <- "Petrobras"
questionnaire$institution[c(60,62,67,69,70)] <- "Fiocruz"
questionnaire$institution[c(56,61,63,64,65)] <- "COR"

questionnaire <- data.frame(lapply(questionnaire, function(x) gsub(", ", ",", x)))
questionnaire <- data.frame(lapply(questionnaire, function(x) gsub("Method A,Method B", "Both", x)))
questionnaire$ex3_method <- gsub("Method A,None", "None", questionnaire$ex3_method)


#========== Preparing proportions - Profiles ===========
#Domains
domains <- as.data.frame(table(unlist(strsplit(as.character(questionnaire$domain), ','))))
names(domains) <- c("Domain","Responses")
domains$Domain <- c("Education", "Extractivism and Mining", "Financial",
                    "Industry", "IT", "Public Services", "Health",
                    "Telecommunications", "Transport")
domains$prop <- prop.table(domains$Responses)


#Work with event detection
questionnaire$workEvtDet = replace(x = questionnaire$workEvtDet, 
                                   list =  !questionnaire$workEvtDet %in% c('Yes', 'No'), 
                                   values =  'Other')
workevtdet <- as.data.frame(table(questionnaire$workEvtDet))
names(workevtdet) <- c("Work","Responses")
workevtdet$prop <- prop.table(workevtdet$Responses)

# ---------- Plots ----------
#Domains
ggplot(domains,aes(x = reorder(Domain, Responses), y = Responses)) + 
  geom_col(position = 'dodge') + 
  labs(x="Domains")+
  coord_flip(clip = "off") +
  theme_bw() +
  geom_text(aes(label = paste0(Responses, " (", round(prop*100,1),"%)")), hjust = -0.1)

#Work with event detection
ggplot(workevtdet,aes(x = reorder(Work, Responses), y = Responses)) + 
  geom_col(position = 'dodge') + 
  labs(x="Work with event detection")+
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = paste0(Responses, " (", round(prop*100,1),"%)")), hjust = -0.1)



#========== Preparing proportions - Questions ===========
#Questions
prop_question <- function(questionnaire,question,responses=c("all","specialists","volunteers")){
  
  responses <- match.arg(responses)
  questionnaire <- switch(responses,
                          "specialists"=questionnaire[as.numeric(questionnaire$specialist)==1,],
                          "volunteers"=questionnaire[!as.numeric(questionnaire$specialist)==1,],
                          "all"=questionnaire)
  
  ex_q1 <- as.data.frame(table(questionnaire[[paste0("ex",question,"_method")]]))
  names(ex_q1) <- c("Method","Responses")
  ex_q1$Method <- factor(ex_q1$Method, levels = c("None","Both","Method B","Method A"))
  ex_q1$prop <- prop.table(ex_q1$Responses)
  
  #Stopped here
  library(data.table)
  ex_metrics <- strsplit(as.character(questionnaire[[paste0("ex",question,"_metrics")]]), ',')
  #ex1_metrics[[44]] <- ex1_metrics[[65]] <- "Other"
  ex_metrics <- lapply(ex_metrics,function(x) { x[x %in% c("TP","FP","TN","FN","Precision","Recall")] <- "F1"
                                       x[!x %in% c("F1","NAB score")] <- "Other"
                                       x})
  #browser()
  ex_metrics <- lapply(ex_metrics,function(x) unique(x[x %in% c("F1","NAB score","Other")])) #
  ex_q2 <- as.data.frame(table(unlist(ex_metrics)))
  names(ex_q2) <- c("Metric","Responses")
  ex_q2$Metric <- factor(ex_q2$Metric, levels = c("Other","NAB score","F1"))#
  ex_q2$prop <- prop.table(ex_q2$Responses)
  
  return(list(ex_q1=ex_q1,ex_q2=ex_q2))
}
# ---------- Plots ----------
plot_question <- function(question_prop){
  p <- ggplot(question_prop$ex_q1,aes(x = Method, y = Responses)) + 
    geom_col(position = 'dodge') + 
    labs(x="Method")+
    theme_bw() +
    geom_text(aes(label = paste0(Responses, " (", round(prop*100,0),"%)")), hjust = -0.2)+
    scale_x_discrete(drop = FALSE)+
    scale_y_continuous(expand = expansion(mult = c(0, .3))) +
    coord_flip()
  print(p)
  p <- ggplot(question_prop$ex_q2,aes(x = Metric, y = Responses)) + 
    geom_col(position = 'dodge') + 
    labs(x="Metric")+
    theme_bw() +
    geom_text(aes(label = paste0(Responses, " (", round(prop*100,0),"%)")), hjust = -0.2)+
    scale_x_discrete(drop = FALSE)+
    scale_y_continuous(expand = expansion(mult = c(0, .3))) +
    coord_flip()
  
  print(p)
}



#Questions
q1 <- prop_question(questionnaire,1,responses="specialists")
plot_question(q1)

q2 <- prop_question(questionnaire,2,responses="specialists")
plot_question(q2)

q3 <- prop_question(questionnaire,3,responses="specialists")
plot_question(q3)

q4 <- prop_question(questionnaire,4,responses="specialists")
plot_question(q4)

q5 <- prop_question(questionnaire,5,responses="specialists")
plot_question(q5)

q6 <- prop_question(questionnaire,6,responses="specialists")
plot_question(q6)
