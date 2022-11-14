library(atus)


#data(atuscps)


data(atusresp)

officeworkers<-atusresp %>%
  filter(occup_code=="office_admin")%>%
  filter(ind_code=="public_admin")

officeworkers$tucaseid<-as.character(officeworkers$tucaseid)

id<-sample(officeworkers$tucaseid, 1)
data(atusact)
df<-atusact
df<-ungroup(df)
df$tucaseid<-as.character(df$tucaseid)
df<-df[df$tucaseid==id,]