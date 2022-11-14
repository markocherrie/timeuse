
# read in the data
df<-read.csv("data/UKDA-8128-tab/tab/uktus15_diary_ep_long.tab", sep="\t")

# filter to working age
df<-df %>%
  filter(KindOfDay==1)%>%
  filter(Trip==1)%>% 
  filter(DVAge>20 & DVAge<64)
#mutate(WhereWhen2=ifelse(WhereWhen<11 & WhereWhen>=90, NA, WhereWhen))

# create the colours for the categories
checklabels<-unique(df$WhereWhen)
library(Polychrome)
Palll <- createPalette(length(checklabels),  c("#ff0000", "#00ff00", "#0000ff"), target = c("normal"))
Palll <- sample(Palll)
d<-cbind(as.data.frame(checklabels),as.data.frame(Palll))
#write.csv(d, "data/colourcodebook.csv", row.names=F)


df<-df %>%
  select(serial, pnum, DiaryDate_Act, WhereWhen, epnum, tid, eptime, WithAlone, WithSpouse, WithMother, WithFather,
         WithChild, WithOther, WithOtherYK, WithMiss, WithNA)


df2<-df %>%filter(pnum==1 & serial ==c(12201003))

codebook<-read.csv("data/codebook.csv")
codebook$WhereWhen<-gsub("Value = ", "", codebook$WhereWhen)
codebook$WhereWhen<-as.numeric(codebook$WhereWhen)
codebook$WhereWhenLabel<-gsub("Label = ", "", codebook$WhereWhenLabel)
df2<-merge(df2, codebook, by="WhereWhen")

library(ggplot2)

df2$WhereWhenLabel<-as.character(df2$WhereWhenLabel)
df2$serial<-as.character(df2$serial)

df2<-df2[order(df2$tid),]

ggplot(df2, aes(x=serial, y=eptime, fill=WhereWhenLabel))+ 
  geom_bar(stat="identity") +
  facet_wrap(~WithAlone)
