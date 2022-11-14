##########################################################################################

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

################################################

library(dplyr)
timeforeachdsoc<-function(typeofstrat, typeofwhereTF, typeofwhere, dsocnum, serialid){
df_aux<-read.csv("data/UKDA-8128-tab/tab/uktus15_individual.tab", sep="\t")
df_aux <- df_aux %>% select(serial, pnum, dsoc)

df<-read.csv("data/UKDA-8128-tab/tab/uktus15_diary_wide.tab", sep="\t")
df<-df %>%
  filter(KindOfDay==1)%>%
  filter(Trip==1)%>% 
  filter(DVAge>20 & DVAge<64)
df<-merge(df, df_aux, by=c("serial", "pnum"))

# Filter by SOC code
if(typeofstrat=="occ"){
df<-df %>% filter(dsoc==dsocnum)
}else if(typeofstrat=="ind"){
df<-df %>% filter(serial==serialid, pnum==1)
}

#
df<-df[,c(1,2, 14,608:751)]

##
checkgroups<-df %>% tidyr::pivot_longer(., wher_1:wher_144)

codebook<-read.csv("data/codebook.csv")
codebook$WhereWhen<-gsub("Value = ", "", codebook$WhereWhen)
codebook$WhereWhen<-as.numeric(codebook$WhereWhen)
codebook$WhereWhenLabel<-gsub("Label = ", "", codebook$WhereWhenLabel)
checkgroups<-merge(checkgroups, codebook, by.x="value",by.y="WhereWhen")
colourcodebook<-read.csv("data/colourcodebook.csv")
checkgroups<-merge(checkgroups, colourcodebook, by.x="value", by.y="checklabels")

e<-checkgroups %>% distinct(WhereWhenLabel, value, Palll)

checkgroups_labels<-e$WhereWhenLabel
checkgroups_colours<-e$Palll

checkgroups$name<-gsub("wher_", "", checkgroups$name)
checkgroups$name<-as.numeric(checkgroups$name)

checkgroups <- checkgroups %>% arrange(serial, pnum, DiaryDate_Act, name)

if(typeofwhereTF==T){
  checkgroups <- checkgroups %>% mutate(WhereWhenLabel, ifelse(WhereWhenLabel==typeofwhere, 1,0))
  checkgroups$WhereWhenLabel<-NULL
  colnames(checkgroups)[7]<-"WhereWhenLabel"
  checkgroups_labels<- checkgroups %>% distinct(WhereWhenLabel)
} else{
  checkgroups
}

checkgroups<-tidyr::pivot_wider(checkgroups, id_cols=c(serial, pnum, DiaryDate_Act), names_from=name,
                            values_from = WhereWhenLabel)

timelist<-format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "10 min"),
                  "%H%M", tz="GMT")
timelist<-c(timelist[-1:-24][1:120],timelist[1:24])


library("TraMineR")
mvad.alphab <- checkgroups_labels
if(typeofwhereTF=T){
  mvad.seq <- seqdef(checkgroups, 4:147, alphabet=c(1,0))
} else(
  mvad.seq <- seqdef(checkgroups, 4:147, alphabet = mvad.alphab)
)
mvad.om <- seqdist(mvad.seq, method = "OM", indel = 1, sm = "TRATE")

####
png(paste0("output/",typeofstrat, typeofwhereTF, typeofwhere, dsocnum, serialid, "_seq",".png"))
if(typeofwhereTF=T){
seqdplot(mvad.seq, border = NA, with.legend=FALSE, xtlab=timelist,
         main=paste0(typeofstrat,"-", dsocnum))
} else(
  seqdplot(mvad.seq, border = NA, with.legend=FALSE, cpal=checkgroups_colours, xtlab=timelist,
           main=paste0(typeofstrat,"-", dsocnum)) 
)
dev.off()

png(paste0("output/",typeofstrat, typeofwhereTF, typeofwhere, dsocnum, serialid, "_legend",".png"))
op <- par(mar=c(5.1,0.1,0.1,0.1))
if(typeofwhereTF=T){
seqlegend(mvad.seq, ncol=1, cex=0.75)
} else{
seqlegend(mvad.seq, ncol=1, cex=0.75, cpal=checkgroups_colours)
}
par(op)
dev.off()
}


timeforeachdsoc("occ", T, "Home", 1, NA)


timeforeachdsoc("occ", 1, NA)
timeforeachdsoc("occ", 2, NA)
timeforeachdsoc("occ", 3, NA)
timeforeachdsoc("occ", 4, NA)
timeforeachdsoc("occ", 5, NA)
timeforeachdsoc("occ", 6, NA)
timeforeachdsoc("occ", 7, NA)
timeforeachdsoc("occ", 8, NA)
timeforeachdsoc("occ", 9, NA)

timeforeachdsoc("ind", NA, 14280417)
timeforeachdsoc("ind", NA, 12201003)


##################################

df<-df %>%
  select(serial, pnum, DiaryDate_Act, WhereWhen, epnum, tid, eptime, WithAlone, WithSpouse, WithMother, WithFather,
         WithChild, WithOther, WithOtherYK, WithMiss, WithNA)

# check it equals 1440
# whichserial<-df %>% group_by(serial, pnum, DiaryDate_Act) %>% summarise(eptimesum = sum(eptime))
  
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


# descriptions with or without others...



#df<-read.csv("data/UKDA-8128-tab/tab/uktus15_diary_wide.tab", sep="\t")



df<-read.csv("data/UKDA-8128-tab/tab/uktus15_wksched.tab", sep="\t")

# -1 is Item not applicable
#  1 is Did not work
#  2 is Worked


# only complete entries
df<-df[df$compl_wk=="Complete record",]


# only 1 per person
library(dplyr)
df<- df %>% distinct(serial, .keep_all = TRUE) %>%
     filter(dvage>20 & dvage<64)

df<-df %>% mutate_at(
  vars(t0400_0415_d1:t0345_0400_d7),
  ~ifelse(.x ==-1, NA, .x)
)



df<-merge(df, df_aux, by="serial")

# sales and customer services
df<-df %>% filter(dsoc==1)

df$rowsum<-rowSums (df[14:685], na.rm = FALSE)

df<-df %>% filter(rowsum>0)


library("TraMineR")
mvad.alphab <- c(1,2)
mvad.seq <- seqdef(df, 14:685, alphabet = mvad.alphab)
mvad.om <- seqdist(mvad.seq, method = "OM", indel = 1, sm = "TRATE")

####

seqdplot(mvad.seq, border = NA, )


#### cluster
library("cluster")
clusterward <- agnes(mvad.om, diss = TRUE, method = "ward")


mvad.cl4 <- cutree(clusterward, k = 4)
cl4.lab <- factor(mvad.cl4, labels = paste("Cluster", 1:4))

seqdplot(mvad.seq, group = cl4.lab, border = NA)
