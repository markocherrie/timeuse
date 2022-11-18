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


df$value[df$value==201]<-"Home"
df$value[df$value==202]<-"Work"
df$value[df$value==203]<-"Other"

df$occgroup[df$occgroup==1]<-"1-Semi or unskilled manual work"
df$occgroup[df$occgroup==2]<-"2-Skilled manual worker"
df$occgroup[df$occgroup==3]<-"3-Administrative (not managerial or supervisory responsibility)"
df$occgroup[df$occgroup==4]<-"4-Supervisory/ junior managerial"
df$occgroup[df$occgroup==5]<-"5-Intermediate managerial/ professional/ administrative"
df$occgroup[df$occgroup==6]<-"6-Higher managerial/ professional/ administrative"
df$occgroup[df$occgroup==-7]<-"Not applicable"


df$survey[df$survey!=1]<-"Covid"
df$survey[df$survey==1]<-"Pre-Covid"

df$survey<-factor(df$survey, levels=c("Pre-Covid", "Covid"))

########################################################


df_subset <- df %>% filter(dday > 1 & dday <=7) %>%
  filter(occgroup=="1-Semi or unskilled manual work" |
           occgroup=="2-Skilled manual worker") 

df_subset <- df %>% filter(dday > 1 & dday <=7) %>%
  filter(occgroup=="3-Administrative (not managerial or supervisory responsibility)" |
           occgroup=="4-Supervisory/ junior managerial") 

df_subset <- df %>% filter(dday > 1 & dday <=7) %>%
  filter(occgroup=="5-Intermediate managerial/ professional/ administrative" |
           occgroup=="6-Higher managerial/ professional/ administrative") 



#df_subset <- df

checkgroups<-tidyr::pivot_wider(df_subset, id_cols=c(survey, mainid, dday, occgroup), names_from=name,
                                values_from = value)

timelist<-format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "10 min"),
                  "%H%M", tz="GMT")
timelist<-c(timelist[-1:-24][1:120],timelist[1:24])



library("TraMineR")

mvad.seq <- seqdef(checkgroups, 5:148, alphabet = c("Home", "Other","Work"))

seqdplot(mvad.seq, border = NA, with.legend=T, xtlab=timelist, group=checkgroups$survey)


seqHtplot(mvad.seq, group=checkgroups$survey, xtlab=timelist, xlab="Time (24hr)")



#seqmtplot(mvad.seq, with.legend = T, group=checkgroups$survey)

#mvad.om <- seqdist(mvad.seq, method = "OM", indel = 1, sm = "TRATE")

# 201 is home
# 202 is work
# 203 is other

# build an entropy model

df_subset <- df %>% filter(dday > 1 & dday <=7)
checkgroups<-tidyr::pivot_wider(df_subset, id_cols=c(survey, mainid, dday, occgroup), names_from=name,
                                values_from = value)

timelist<-format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "10 min"),
                  "%H%M", tz="GMT")
timelist<-c(timelist[-1:-24][1:120],timelist[1:24])
mvad.seq <- seqdef(checkgroups, 5:148, alphabet = c("Home", "Other","Work"))

### model
entropies <- seqient(mvad.seq)
lm.ent <- lm(entropies ~ occgroup, checkgroups)
summary(lm.ent)



