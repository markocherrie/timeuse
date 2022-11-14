
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