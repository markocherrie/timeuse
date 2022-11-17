##########################################################################################
library(dplyr)

# read in the data
df<-read.csv("data/UKDA-8741-tab/tab/uk_6_wave_caddi.tab", sep="\t")

df<-df %>%
  tidyr::pivot_longer(loc1:loc144) 


# filter to working age
df<-df %>%
  #filter(KindOfDay==1)%>%
  filter(diaryord==1)%>% 
  filter(age>20 & age<64) %>%
  select(survey, mainid, dmonth, dday, sex, age, occgroup, name, value, ghq1:ghq12)


df$myID<-paste0(df$survey, "_", df$mainid, "_", df$dmonth, "_", df$dday)

df$duplicated<-!duplicated(df$myID)
notduplicated<-subset(df, duplicated==T)
notduplicated <- notduplicated[!duplicated(notduplicated$mainid),]


df<-df[df$myID %in% unique(notduplicated$myID),]

#mutate(WhereWhen2=ifelse(WhereWhen<11 & WhereWhen>=90, NA, WhereWhen))


table(df$occgroup)

# loc is the location - home/work/other
# who(a/b/c) is the person they are with
# dev is device
# enj is enjoyment



# create the colours for the categories
checklabels<-unique(df$value)
library(Polychrome)
Palll <- createPalette(length(checklabels),  c("#ff0000", "#00ff00", "#0000ff"), target = c("normal"))[1:3]
Palll <- sample(Palll)
d<-cbind(as.data.frame(checklabels),as.data.frame(Palll))
#write.csv(d, "data/colourcodebook.csv", row.names=F)



checkgroups<-tidyr::pivot_wider(df, id_cols=c(survey, mainid, dday), names_from=name,
                                values_from = value)

timelist<-format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "10 min"),
                  "%H%M", tz="GMT")
timelist<-c(timelist[-1:-24][1:120],timelist[1:24])


library("TraMineR")
mvad.alphab <- checkgroups_labels
mvad.seq <- seqdef(checkgroups, 4:147, alphabet = c(201, 202,203))

seqdplot(mvad.seq, border = NA, with.legend=FALSE, xtlab=timelist)


