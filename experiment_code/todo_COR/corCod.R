


setwd("~/COR/series_cor")
files <- list.files(pattern = "*.Rds")

for (i in 1:length(files)) {
  setwd("~/COR/series_cor")
  test <- read_rds(files[i])
  events_scp <- evtdet.seminalChangePoint(test, w=96,na.action=na.omit)
  setwd("~/COR/W96/SCP/")
  saveRDS(events_scp, file = files[i])
}
