#Combine
P_1_3 <-read.table("E:\\IRDpower_revision\\P(1,3)_power.txt")
P_1_4 <-read.table("E:\\IRDpower_revision\\P(1,4)_power.txt")
colnames(P_1_3) <- c("a1","b1","c1","d1","e1","f1","h1")
colnames(P_1_4) <- c("a2","b2","c2","d2","e2","f2","h2")

cbind(P_1_3[,-1],P_1_4[,-1])


X2_4 <-read.table("E:\\IRDpower_revision\\X2(4)_power.txt")
W_2 <-read.table("E:\\IRDpower_revision\\W(2)_power.txt")
colnames(X2_4) <- c("a1","b1","c1","d1","e1","f1","h1")
colnames(W_2) <- c("a2","b2","c2","d2","e2","f2","h2")

cbind(X2_4[,-1],W_2[,-1])


Ln_1_1_2 <-read.table("E:\\IRDpower_revision\\Ln(1,1.2)_power.txt")
Ln_1_2 <-read.table("E:\\IRDpower_revision\\Ln(1,2)_power.txt")
colnames(Ln_1_1_2) <- c("a1","b1","c1","d1","e1","f1","h1")
colnames(Ln_1_2) <- c("a2","b2","c2","d2","e2","f2","h2")

cbind(Ln_1_1_2[,-1],Ln_1_2[,-1])









library(reshape2)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(ggthemr)

table2 <- read.table('E:\\IRDpower_revision\\IRD_table2_res.txt',sep = '&')
table2_5 <- table2[,c(1,2,3,4)]
table2_5_name <- c('Scheme_number','T_{CRKL}','T_{CKL}','W')
colnames(table2_5) <- table2_5_name
table2_5
dim(table2_5)

Censoring_model <-rep(c('Left censored','Right censored','Two sided censored'),10) 
Censoring_model
table2_5$Censoring_type <- Censoring_model

head(table2_5)

table2_5_shaped<- melt(table2_5,id=c('Scheme_number','Censoring_type'))

ggthemr('solarized') 
ggplot(table2_5_shaped,aes(x=Scheme_number,y=value,fill=variable))+geom_bar(stat = 'identity',width = 1.2, position = position_dodge(2))+
  labs(x='Scheme number',y='Power',title = 'Power analysis (Sig=5%)',fill="Test statistics")+
  theme(axis.text.x=element_text(angle=0,size=16))+
  theme(axis.text.y=element_text(angle=0,size=16))+
  theme(title=element_text(size=40, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(. ~ Censoring_type)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))



########
table2_10 <- table2[,c(1,5,6,7)]
table2_10_name <- c('Scheme_number','T_{CRKL}','T_{CKL}','W')
colnames(table2_10) <- table2_10_name
head(table2_10)
dim(table2_10)

Censoring_model <-rep(c('Left censored','Right censored','Two sided censored'),10) 
Censoring_model
table2_10$Censoring_type <- Censoring_model

head(table2_10)

table2_10_shaped<- melt(table2_10,id=c('Scheme_number','Censoring_type'))

ggthemr('solarized') 
ggplot(table2_10_shaped,aes(x=Scheme_number,y=value,fill=variable))+geom_bar(stat = 'identity',width = 1.2, position = position_dodge(2))+
  labs(x='Scheme number',y='Power',title = 'Power analysis (Sig=10%)',fill="Test statistics")+
  theme(axis.text.x=element_text(angle=0,size=16))+
  theme(axis.text.y=element_text(angle=0,size=16))+
  theme(title=element_text(size=40, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(. ~ Censoring_type)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))




#Í¼Æ¬Êä³öÎªpdf 22*10
#######################table3
table3 <- read.table('E:\\IRDpower_revision\\IRD_table3_res.txt',sep = '&')
table3_5 <- table3[,c(1,2,3,4)]
table3_5_name <- c('Scheme_number','T_{CRKL}','T_{CKL}','W')
colnames(table3_5) <- table3_5_name
head(table3_5)
dim(table3_5)

Censoring_model <-rep(c('Left censored','Right censored','Two sided censored'),10) 
Censoring_model
table3_5$Censoring_type <- Censoring_model

head(table3_5)

table3_5_shaped<- melt(table3_5,id=c('Scheme_number','Censoring_type'))

ggthemr('solarized') 
ggplot(table3_5_shaped,aes(x=Scheme_number,y=value,fill=variable))+geom_bar(stat = 'identity',width = 1.2, position = position_dodge(2))+
  labs(x='Scheme number',y='Power',title = 'Power analysis (Sig=5%)',fill="Test statistics")+
  theme(axis.text.x=element_text(angle=0,size=16))+
  theme(axis.text.y=element_text(angle=0,size=16))+
  theme(title=element_text(size=40, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(. ~ Censoring_type)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))



########
table3_10 <- table3[,c(1,5,6,7)]
table3_10_name <- c('Scheme_number','T_{CRKL}','T_{CKL}','W')
colnames(table3_10) <- table3_10_name
head(table3_10)
dim(table3_10)

Censoring_model <-rep(c('Left censored','Right censored','Two sided censored'),10) 
Censoring_model
table3_10$Censoring_type <- Censoring_model

table3_10

table3_10_shaped<- melt(table3_10,id=c('Scheme_number','Censoring_type'))

ggthemr('solarized') 
ggplot(table3_10_shaped,aes(x=Scheme_number,y=value,fill=variable))+geom_bar(stat = 'identity',width = 1.2, position = position_dodge(2))+
  labs(x='Scheme number',y='Power',title = 'Power analysis (Sig=10%)',fill="Test statistics")+
  theme(axis.text.x=element_text(angle=0,size=16))+
  theme(axis.text.y=element_text(angle=0,size=16))+
  theme(title=element_text(size=40, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(. ~ Censoring_type)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))




#######################table4
table4 <- read.table('E:\\IRDpower_revision\\IRD_table4_res.txt',sep = '&')
table4_5 <- table4[,c(1,2,3,4)]
table4_5_name <- c('Scheme_number','T_{CRKL}','T_{CKL}','W')
colnames(table4_5) <- table4_5_name
head(table4_5)
dim(table4_5)

Censoring_model <-rep(c('Left censored','Right censored','Two sided censored'),10) 
Censoring_model
table4_5$Censoring_type <- Censoring_model

head(table4_5)

table4_5_shaped<- melt(table4_5,id=c('Scheme_number','Censoring_type'))

ggthemr('solarized') 
ggplot(table4_5_shaped,aes(x=Scheme_number,y=value,fill=variable))+geom_bar(stat = 'identity',width = 1.2, position = position_dodge(2))+
  labs(x='Scheme number',y='Power',title = 'Power analysis (Sig=5%)',fill="Test statistics")+
  theme(axis.text.x=element_text(angle=0,size=16))+
  theme(axis.text.y=element_text(angle=0,size=16))+
  theme(title=element_text(size=40, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(. ~ Censoring_type)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))



########
table4_10 <- table4[,c(1,5,6,7)]
table4_10_name <- c('Scheme_number','T_{CRKL}','T_{CKL}','W')
colnames(table4_10) <- table4_10_name
head(table4_10)
dim(table4_10)

Censoring_model <-rep(c('Left censored','Right censored','Two sided censored'),10) 
Censoring_model
table4_10$Censoring_type <- Censoring_model

table4_10

table4_10_shaped<- melt(table4_10,id=c('Scheme_number','Censoring_type'))

ggthemr('solarized') 
ggplot(table4_10_shaped,aes(x=Scheme_number,y=value,fill=variable))+geom_bar(stat = 'identity',width = 1.2, position = position_dodge(2))+
  labs(x='Scheme number',y='Power',title = 'Power analysis (Sig=10%)',fill="Test statistics")+
  theme(axis.text.x=element_text(angle=0,size=16))+
  theme(axis.text.y=element_text(angle=0,size=16))+
  theme(title=element_text(size=40, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(. ~ Censoring_type)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))



