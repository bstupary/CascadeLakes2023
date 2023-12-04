
PROref.tax <- Cntl22[,-c(3:5)]
PROref.cwm <- CWM.Sites22.controls.rm[,-c(3,4)]
ref.years <- Cntl22$Year

All22.a <- as.data.frame(All22)
PROref.tax <- Sites22[,-c(3:5)]
PROref.cwm <- CWM.Sites22[,-c(3:5)]
ref.years <- Sites22.rm$Year


protest.all <- protest(PROref.tax, PROref.cwm)
print(protest.Ref, digits = 6)
