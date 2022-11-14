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
if(typeofwhereTF==T){
  mvad.seq <- seqdef(checkgroups, 4:147, alphabet=c(1,0))
} else(
  mvad.seq <- seqdef(checkgroups, 4:147, alphabet = mvad.alphab)
)
mvad.om <- seqdist(mvad.seq, method = "OM", indel = 1, sm = "TRATE")

####
png(paste0("output/",typeofstrat, typeofwhereTF, typeofwhere, dsocnum, serialid, "_seq",".png"))
if(typeofwhereTF==T){
seqdplot(mvad.seq, border = NA, with.legend=FALSE, xtlab=timelist,
         main=paste0(typeofstrat,"-", dsocnum))
} else(
  seqdplot(mvad.seq, border = NA, with.legend=FALSE, cpal=checkgroups_colours, xtlab=timelist,
           main=paste0(typeofstrat,"-", dsocnum)) 
)
dev.off()

png(paste0("output/",typeofstrat, typeofwhereTF, typeofwhere, dsocnum, serialid, "_legend",".png"))
op <- par(mar=c(5.1,0.1,0.1,0.1))
if(typeofwhereTF==T){
seqlegend(mvad.seq, ncol=1, cex=0.75)
} else{
seqlegend(mvad.seq, ncol=1, cex=0.75, cpal=checkgroups_colours)
}
par(op)
dev.off()
}


timeforeachdsoc("occ", T, "Home", 1, NA)
timeforeachdsoc("occ", T, "Working place or school", 1, NA)
timeforeachdsoc("occ", T, "Working place or school", 2, NA)
timeforeachdsoc("occ", T, "Working place or school", 3, NA)
timeforeachdsoc("occ", T, "Working place or school", 4, NA)
timeforeachdsoc("occ", T, "Working place or school", 5, NA)
timeforeachdsoc("occ", T, "Working place or school", 6, NA)
timeforeachdsoc("occ", T, "Working place or school", 7, NA)
timeforeachdsoc("occ", T, "Working place or school", 8, NA)
timeforeachdsoc("occ", T, "Working place or school", 9, NA)

######

# old command

timeforeachdsoc("occ", F, NA , 1, NA)


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
