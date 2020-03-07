GEUI<-function(r,R){
  q<-length(R)
  m <- q+r
  ita<-rep(0,q)
  ita[1]<-(m-r+sum(R))/(1+r+m-r+sum(R))#即alpha(m)
  for(j in 2:q){
    ita[j]<-(m-(j-1)-r+sum(R[j:q]))/(1+m-(j-1)-r+sum(R[j:q]))
  }#(m-r-(j-1)+sum(R[j:q]))/(1+m-r-(j-1)+sum(R[j:q]))
  eta<-rep(0,q) 
  for(i in 1:q ){
    eta[i]<-1-prod(ita[1:i])
  }
  return(eta)
}

Gptype2_W_T_H0_HLD<-function(theta,r,R){
  q<-length(R)
  n <- q+r+sum(R)
  V_m <- rbeta(1,n-r,r+1)
  V<-rep(0,q) 
  V[q] <- V_m
  a <- rep(0,q-1)
  for(i in 1:(q-1) ){
    a[i]<-i+sum(R[(q-i+1):q])
  }
  m=q+r
  W <- runif(m-r-1)
  
  for(i in 1:(q-1)){
    V[i]<-W[i]^(1/a[i])
  }
  
  U <- rep(0,q)
  for(i in 1:q){
    U[i]<-1-prod(V[(q-i+1):q])
  }
  return(theta*(log((1+U)/(1-U))))
}


W_HLD_H0 <- function(R){
  q<-length(R)
  theta <-1
  r <- 1
  P <- GEUI(r,R)
  A<-rep(0,q)
  for(i in 1:q){
    A[i] <- -log(1-P[i])-0.5*(P[i]+1)*log((1+P[i])/(1-P[i]))
  }
  
  B<-rep(0,q)
  for(i in 1:q){
    B[i]<- 0.5*(1+P[i])
  }
  
  X<-Gptype2_W_T_H0_HLD(theta,r,R)
  n <- q+r+sum(R)
  m <- q+r
  theta_hat<- (sum((R+1)*B*X)-(n-r)*B[1]*X[1])/(m-r-1-sum((R+1)*A)+(n-r)*A[1])
  #print(theta_hat)
  W <- 2*sum((R+1)*(-log((2*exp(-X/theta_hat))/(1+exp(-X/theta_hat)))))-2*(n-r)*(-log((2*exp(-X[1]/theta_hat))/(1+exp(-X[1]/theta_hat))))
  return(W)
  
}


get_W_H0 <- function(sim,R){
  W_H0_HLD_stat <- numeric(sim) 
  for (i in 1:sim) {
    W_H0_HLD_stat[i] <- W_HLD_H0(R)
  }
  return(W_H0_HLD_stat)
}


get_Q_T <- function(X,R,theta_hat){
  r <- 1
  q<-length(R)
  n <- q+r+sum(R)
  m <- q+r
  PP <- (-log((2*exp(-X/theta_hat))/(1+exp(-X/theta_hat))))
  Q <- numeric(length(X))
  Q[1] <-(n-r)*PP[1]
  for (j in 2:(length(X))) {
    Q[j] <- (n-r-sum(R[1:(j-1)])-(j-1))*(PP[j]-PP[j-1])#PP 即论文中的P，为了和下面的GEUI作区分
  }
  TT <- numeric(m-r-2) 
  for (k in 1:length(TT)) {
    TT[k] <- 2*(log((sum(Q[2:(length(X))]))/(sum(Q[2:(k+1)]))))
  }
  return(sum(TT))
  
}

T_HLD_H0 <- function(R){
  q<-length(R)
  theta <-1
  r <- 1
  P <- GEUI(r,R)
  A<-rep(0,q)
  for(i in 1:q){
    A[i] <- -log(1-P[i])-0.5*(P[i]+1)*log((1+P[i])/(1-P[i]))
  }
  
  B<-rep(0,q)
  for(i in 1:q){
    B[i]<- 0.5*(1+P[i])
  }
  
  X<-Gptype2_W_T_H0_HLD(theta,r,R)
  n <- q+r+sum(R)
  m <- q+r
  theta_hat<- (sum((R+1)*B*X)-(n-r)*B[1]*X[1])/(m-r-1-sum((R+1)*A)+(n-r)*A[1])
  #print(theta_hat)
  T_H0 <- get_Q_T(X,R,theta_hat)
  return(T_H0)
  
}


get_T_H0 <- function(sim,R){
  T_HLD_H0_stat <- numeric(sim) 
  for (i in 1:sim) {
    T_HLD_H0_stat[i] <- T_HLD_H0(R)
  }
  return(T_HLD_H0_stat)
}

#########################




W_H1_HLD <- function(R,parameter){
  q<-length(R)
  
  r <- 1
  P <- GEUI(r,R)
  A<-rep(0,q)
  for(i in 1:q){
    A[i] <- -log(1-P[i])-0.5*(P[i]+1)*log((1+P[i])/(1-P[i]))
  }
  
  B<-rep(0,q)
  for(i in 1:q){
    B[i]<- 0.5*(1+P[i])
  }
  
  X<-Gptype2_H1_W_T(r,R,parameter)
  n <- q+r+sum(R)
  m <- q+r
  theta_hat<- (sum((R+1)*B*X)-(n-r)*B[1]*X[1])/(m-r-1-sum((R+1)*A)+(n-r)*A[1])
  W_H1 <- 2*sum((R+1)*(-log((2*exp(-X/theta_hat))/(1+exp(-X/theta_hat)))))-2*(n-r)*(-log((2*exp(-X[1]/theta_hat))/(1+exp(-X[1]/theta_hat))))
  return(W_H1)
  
}


get_W_H1 <- function(sim,R,parameter){
  W_HLD_H1_stat <- numeric(sim) 
  for (i in 1:sim) {
    W_HLD_H1_stat[i] <- W_H1_HLD(R,parameter)
  }
  return(W_HLD_H1_stat)
}



T_HLD_H1 <- function(R,parameter){
  q<-length(R)
  r <- 1
  P <- GEUI(r,R)
  A<-rep(0,q)
  for(i in 1:q){
    A[i] <- -log(1-P[i])-0.5*(P[i]+1)*log((1+P[i])/(1-P[i]))
  }
  
  B<-rep(0,q)
  for(i in 1:q){
    B[i]<- 0.5*(1+P[i])
  }
  
  
  X<-Gptype2_H1_W_T(r,R,parameter)
  n <- q+r+sum(R)
  m <- q+r
  theta_hat<- (sum((R+1)*B*X)-(n-r)*B[1]*X[1])/(m-r-1-sum((R+1)*A)+(n-r)*A[1])
  
  T_H1 <- get_Q_T(X,R,theta_hat)
  return(T_H1)
  
}

get_T_H1 <- function(sim,R,parameter){
  T_HLD_H1_stat <- numeric(sim) 
  for (i in 1:sim) {
    T_HLD_H1_stat[i] <- T_HLD_H1(R,parameter)
  }
  return(T_HLD_H1_stat)
}




#######################################
###################################

#r=1
R13 <-c(rep(0,18),10) 
R17 <- c(rep(0,18),20)
R_all_explore <- list(R13,R17)
Gptype2_H1_W_T<-function(r,R,parameter){
  q<-length(R)
  n <- q+r+sum(R)
  V_m <- rbeta(1,n-r,r+1)
  V<-rep(0,q) 
  V[q] <- V_m
  a <- rep(0,q-1)
  for(i in 1:(q-1) ){
    a[i]<-i+sum(R[(q-i+1):q])
  }
  m=q+r
  W <- runif(m-r-1)
  
  for(i in 1:(q-1)){
    V[i]<-W[i]^(1/a[i])
  }
  
  U <- rep(0,q)
  for(i in 1:q){
    U[i]<-1-prod(V[(q-i+1):q])
  }
  return(qchisq(U,df=parameter))
}
#Xm/(1-x^(1/a))  Xm为scale  a为shape

sim <- 15000
par_seq <- seq(1,20,1)
par_seq_index <- 1:length(par_seq)
W_T_power_par_explore <- matrix(NA,nrow=length(par_seq),ncol =9)#一列初值和两个删失下两个统计量在两个显著性水平下的power

for(k in 1:length(R_all_explore)){
  R <- R_all_explore[[k]]

  for (l in par_seq_index){
    parameter <- par_seq[l]
    W_HLD_H0_res <-get_W_H0(sim,R)
    q25 <- quantile(W_HLD_H0_res,0.025)
    q975 <- quantile(W_HLD_H0_res,0.975)
    q5 <- quantile(W_HLD_H0_res,0.05)
    q95 <- quantile(W_HLD_H0_res,0.95)
    
    T_HLD_H0_res <-get_T_H0(sim,R)
    
    tq25 <- quantile(T_HLD_H0_res,0.025)
    tq975 <- quantile(T_HLD_H0_res,0.975)
    tq5 <- quantile(T_HLD_H0_res,0.05)
    tq95 <- quantile(T_HLD_H0_res,0.95)
    print(paste('R=',k,'l=',l,'case','quantile get!'))
    W_HLD_H1_res <-get_W_H1(sim,R,parameter) 
    T_HLD_H1_res <-get_T_H1(sim,R,parameter)
    #注意下面power的结果的合并的顺序不要乱，要和论文一致。
    power_res<- cbind(sum((W_HLD_H1_res<q25)|(W_HLD_H1_res>q975))/sim,
                      sum((T_HLD_H1_res<tq25)|(T_HLD_H1_res>tq975))/sim,
                      sum((W_HLD_H1_res<q5)|(W_HLD_H1_res>q95))/sim,
                      sum((T_HLD_H1_res<tq5)|(T_HLD_H1_res>tq95))/sim
    )
    
    W_T_power_par_explore[l,(4*k-2):(4*k+1)]<- power_res
    #每4个为一组，为一个删失模式下的两个统计量在两个显著性水平下的power
    #第一列留出来放初值
    print(paste('R=',k,'l=',l,'case','power get!'))
  }
  
}

W_T_power_par_explore[,1] <- par_seq#参数初值放在第一列


round(W_T_power_par_explore,5)




#####################plot
library(ggplot2)
library(ggthemr)
library(ggthemes)
library(reshape2)
W_par <- read.table("E:\\data\\scale_test_par_explore\\par2\\par2_W_scale_test_par_explore.txt")

W_par_exp <- W_par[,-1]
colnames(W_par_exp) <- c('Shape_parameter','W_C.S(18*0,10)_Sig=5%','T_C.S(18*0,10)_Sig=5%'
                         ,'W_C.S(18*0,10)_Sig=10%','T_C.S(18*0,10)_Sig=10%',
                         'W_C.S(18*0,20)_Sig=5%','T_C.S(18*0,20)_Sig=5%',
                         'W_C.S(18*0,20)_Sig=10%','T_C.S(18*0,20)_Sig=10%')

W_par_exp_m<- melt(W_par_exp,id.vars='Shape_parameter')
C_S <- c(rep('C.S(18*0,10)',80),rep('C.S(18*0,20)',80))
Sig_level <- c(rep('Sig=5%',40),rep('Sig=10%',40),rep('Sig=5%',40),rep('Sig=10%',40))            
Tests <- c(rep('W',20),rep('T',20),rep('W',20),rep('T',20),
                     rep('W',20),rep('T',20),rep('W',20),rep('T',20))   

W_par_exp_m$Censoring_schme <- C_S
W_par_exp_m$Significance_level <- Sig_level
W_par_exp_m$Test_statistics <- Tests
W_par_exp_m

library(ggthemr) 
ggthemr('solarized')
ggplot(W_par_exp_m,aes(x=Shape_parameter,y=value,colour=Test_statistics))+
  geom_point(aes(shape=Test_statistics))+geom_line()+
  labs(y='Power result',title = 'The Shape parameter versus Power (H1:Weibull distribution)')+
  scale_x_continuous(breaks=seq(0,2,0.1))+
  theme(axis.text.x=element_text(angle=-60,size=20))+
  theme(axis.text.y=element_text(angle=0,size=20))+
  theme(title=element_text(size=35, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(Significance_level~Censoring_schme)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))

####
G_par <- read.table("E:\\data\\scale_test_par_explore\\par2\\par2_G_scale_test_par_explore.txt")

G_par_exp <- G_par[,-1]
colnames(G_par_exp) <- c('Shape_parameter','W_C.S(18*0,10)_Sig=5%','T_C.S(18*0,10)_Sig=5%'
                         ,'W_C.S(18*0,10)_Sig=10%','T_C.S(18*0,10)_Sig=10%',
                         'W_C.S(18*0,20)_Sig=5%','T_C.S(18*0,20)_Sig=5%',
                         'W_C.S(18*0,20)_Sig=10%','T_C.S(18*0,20)_Sig=10%')

G_par_exp_m<- melt(G_par_exp,id.vars='Shape_parameter')
C_S <- c(rep('C.S(18*0,10)',80),rep('C.S(18*0,20)',80))
Sig_level <- c(rep('Sig=5%',40),rep('Sig=10%',40),rep('Sig=5%',40),rep('Sig=10%',40))            
Tests <- c(rep('W',20),rep('T',20),rep('W',20),rep('T',20),
           rep('W',20),rep('T',20),rep('W',20),rep('T',20))   

G_par_exp_m$Censoring_schme <- C_S
G_par_exp_m$Significance_level <- Sig_level
G_par_exp_m$Test_statistics <- Tests
G_par_exp_m

library(ggthemr) 
ggthemr('solarized')
ggplot(G_par_exp_m,aes(x=Shape_parameter,y=value,colour=Test_statistics))+
  geom_point(aes(shape=Test_statistics))+geom_line()+
  labs(y='Power result',title = 'The Shape parameter versus Power (H1:Gamma distribution)')+
  scale_x_continuous(breaks=seq(0,2,0.1))+
  theme(axis.text.x=element_text(angle=-60,size=20))+
  theme(axis.text.y=element_text(angle=0,size=20))+
  theme(title=element_text(size=35, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(Significance_level~Censoring_schme)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))



####
Ln_par <- read.table("E:\\data\\scale_test_par_explore\\par2\\par2_Ln_scale_test_par_explore.txt")

Ln_par_exp <- Ln_par[,-1]
colnames(Ln_par_exp) <- c('Scale_parameter','W_C.S(18*0,10)_Sig=5%','T_C.S(18*0,10)_Sig=5%'
                         ,'W_C.S(18*0,10)_Sig=10%','T_C.S(18*0,10)_Sig=10%',
                         'W_C.S(18*0,20)_Sig=5%','T_C.S(18*0,20)_Sig=5%',
                         'W_C.S(18*0,20)_Sig=10%','T_C.S(18*0,20)_Sig=10%')

Ln_par_exp_m<- melt(Ln_par_exp,id.vars='Scale_parameter')
C_S <- c(rep('C.S(18*0,10)',80),rep('C.S(18*0,20)',80))
Sig_level <- c(rep('Sig=5%',40),rep('Sig=10%',40),rep('Sig=5%',40),rep('Sig=10%',40))            
Tests <- c(rep('W',20),rep('T',20),rep('W',20),rep('T',20),
           rep('W',20),rep('T',20),rep('W',20),rep('T',20))   

Ln_par_exp_m$Censoring_schme <- C_S
Ln_par_exp_m$Significance_level <- Sig_level
Ln_par_exp_m$Test_statistics <- Tests
Ln_par_exp_m

library(ggthemr) 
ggthemr('solarized')
ggplot(Ln_par_exp_m,aes(x=Scale_parameter,y=value,colour=Test_statistics))+
  geom_point(aes(shape=Test_statistics))+geom_line()+
  labs(y='Power result',title = 'The scale parameter versus Power (H1:Log normal distribution)')+
  scale_x_continuous(breaks=seq(0,2,0.1))+
  theme(axis.text.x=element_text(angle=-60,size=20))+
  theme(axis.text.y=element_text(angle=0,size=20))+
  theme(title=element_text(size=35, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(Significance_level~Censoring_schme)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))




####
P_par <- read.table("E:\\data\\scale_test_par_explore\\par2\\par2_P_scale_test_par_explore.txt")

P_par_exp <- P_par[,-1]
colnames(P_par_exp) <- c('Shape_parameter','W_C.S(18*0,10)_Sig=5%','T_C.S(18*0,10)_Sig=5%'
                          ,'W_C.S(18*0,10)_Sig=10%','T_C.S(18*0,10)_Sig=10%',
                          'W_C.S(18*0,20)_Sig=5%','T_C.S(18*0,20)_Sig=5%',
                          'W_C.S(18*0,20)_Sig=10%','T_C.S(18*0,20)_Sig=10%')

P_par_exp_m<- melt(P_par_exp,id.vars='Shape_parameter')
C_S <- c(rep('C.S(18*0,10)',80),rep('C.S(18*0,20)',80))
Sig_level <- c(rep('Sig=5%',40),rep('Sig=10%',40),rep('Sig=5%',40),rep('Sig=10%',40))            
Tests <- c(rep('W',20),rep('T',20),rep('W',20),rep('T',20),
           rep('W',20),rep('T',20),rep('W',20),rep('T',20))   

P_par_exp_m$Censoring_schme <- C_S
P_par_exp_m$Significance_level <- Sig_level
P_par_exp_m$Test_statistics <- Tests
P_par_exp_m

library(ggthemr) 
ggthemr('solarized')
ggplot(P_par_exp_m,aes(x=Shape_parameter,y=value,colour=Test_statistics))+
  geom_point(aes(shape=Test_statistics))+geom_line()+
  labs(y='Power result',title = 'The Shape parameter versus Power (H1:Parto distribution)')+
  scale_x_continuous(breaks=seq(0,2,0.1))+
  theme(axis.text.x=element_text(angle=-60,size=20))+
  theme(axis.text.y=element_text(angle=0,size=20))+
  theme(title=element_text(size=35, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(Significance_level~Censoring_schme)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))



####
X2_par <- read.table("E:\\data\\scale_test_par_explore\\par2\\par2_X2_scale_test_par_explore.txt")

X2_par_exp <- X2_par[,-1]
colnames(X2_par_exp) <- c('freedom_degree','W_C.S(18*0,10)_Sig=5%','T_C.S(18*0,10)_Sig=5%'
                         ,'W_C.S(18*0,10)_Sig=10%','T_C.S(18*0,10)_Sig=10%',
                         'W_C.S(18*0,20)_Sig=5%','T_C.S(18*0,20)_Sig=5%',
                         'W_C.S(18*0,20)_Sig=10%','T_C.S(18*0,20)_Sig=10%')

X2_par_exp_m<- melt(X2_par_exp,id.vars='freedom_degree')
C_S <- c(rep('C.S(18*0,10)',80),rep('C.S(18*0,20)',80))
Sig_level <- c(rep('Sig=5%',40),rep('Sig=10%',40),rep('Sig=5%',40),rep('Sig=10%',40))            
Tests <- c(rep('W',20),rep('T',20),rep('W',20),rep('T',20),
           rep('W',20),rep('T',20),rep('W',20),rep('T',20))   

X2_par_exp_m$Censoring_schme <- C_S
X2_par_exp_m$Significance_level <- Sig_level
X2_par_exp_m$Test_statistics <- Tests
X2_par_exp_m

library(ggthemr) 
ggthemr('solarized')
ggplot(X2_par_exp_m,aes(x=freedom_degree,y=value,colour=Test_statistics))+
  geom_point(aes(shape=Test_statistics))+geom_line()+
  labs(y='Power result',title = 'The degree of freedom versus Power (H1:Chisq distribution)')+
  scale_x_continuous(breaks=seq(1,20,1))+
  theme(axis.text.x=element_text(angle=-60,size=20))+
  theme(axis.text.y=element_text(angle=0,size=20))+
  theme(title=element_text(size=35, face="italic",hjust=0.5,lineheight=0.2))+
  theme(legend.position="top",legend.text = element_text(size = 30))+
  theme(legend.title = element_text(size=30))+
  facet_grid(Significance_level~Censoring_schme)+
  theme(strip.text=element_text(colour = "purple", face="italic", size=rel(2.5)))



