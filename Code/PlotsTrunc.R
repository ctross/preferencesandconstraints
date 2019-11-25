################################################################################
library(colorspace)
  Type <- c("Int",rep("Focal",8),rep("Alter",9),rep("Dyadic",6))
  
  IDs <- c("Int",
         "Age","Male","Indigenous","Cant Work","Grip Strength","Depressed","Food Insecure","Material Wealth",  
         "Age","Male","Indigenous","Cant Work","Grip Strength","Depressed","Food Insecure","Material Wealth","Out Migrated", 
         "Relatedness","Same Ethnicity","Friends","Married","Same Sex","Reciprocation")




################################################################################
BG<-rstan::extract(fit_Trunc,pars="BG")$BG
BL<-rstan::extract(fit_Trunc,pars="BL")$BL
BP<-rstan::extract(fit_Trunc,pars="BP")$BP
BT<-rstan::extract(fit_Trunc,pars="BT")$BT


sample_eff<-apply(BG,2,quantile,probs=c(0.05,0.5,0.95))          
df_Hg<-data.frame(IDs=IDs, Type=Type, Group="Allocation",
                 LI=sample_eff[1,]/(sample_eff[3,]-sample_eff[1,]),
                 Median=sample_eff[2,]/(sample_eff[3,]-sample_eff[1,]),
                 HI=sample_eff[3,]/(sample_eff[3,]-sample_eff[1,])
                 ) 
               
sample_eff<-apply(BL,2,quantile,probs=c(0.05,0.5,0.95))        
df_Hl<-data.frame(IDs=IDs, Type=Type, Group="Leaving",
                 LI=sample_eff[1,]/(sample_eff[3,]-sample_eff[1,]),
                 Median=sample_eff[2,]/(sample_eff[3,]-sample_eff[1,]),
                 HI=sample_eff[3,]/(sample_eff[3,]-sample_eff[1,])
                 ) 
               
sample_eff<-apply(BP,2,quantile,probs=c(0.05,0.5,0.95))          
df_Hp<-data.frame(IDs=IDs, Type=Type, Group="Reducing",
                 LI=sample_eff[1,]/(sample_eff[3,]-sample_eff[1,]),
                 Median=sample_eff[2,]/(sample_eff[3,]-sample_eff[1,]),
                 HI=sample_eff[3,]/(sample_eff[3,]-sample_eff[1,])
                 ) 

sample_eff<-apply(BT,2,quantile,probs=c(0.05,0.5,0.95))       
df_Ht<-data.frame(IDs=IDs, Type=Type, Group="Transfers",
                 LI=sample_eff[1,]/(sample_eff[3,]-sample_eff[1,]),
                 Median=sample_eff[2,]/(sample_eff[3,]-sample_eff[1,]),
                 HI=sample_eff[3,]/(sample_eff[3,]-sample_eff[1,])
                 ) 
                                                                
################################################################################
df_H1 <- rbind(df_Ht,df_Hg,df_Hl,df_Hp)

df_H1$Type <- factor(df_H1$Type, levels(df_H1$Type)[c(3,1,2,4)])

df_H1 <- df_H1[which(df_H1$Type != "Int"),]
colnames(df_H1)[1] <- "Variable"

p <- ggplot(df_H1,aes(x=Variable,y=Median,ymin=LI,ymax=HI))+ 
     geom_linerange(size=1,position = position_dodge(.5))+geom_point(size=2,position = position_dodge(.5))+
     facet_grid(Type~Group,scales="free")+geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines"))  
   p  
   
ggsave("All_Games-Standardized-Trunc.pdf",p,height=8, width=14)

################################################################################
df_H1 <- df_H1[which(df_H1$Group == "Allocation" | df_H1$Group == "Transfers"),]

p <- ggplot(df_H1,aes(x=Variable,y=Median,ymin=LI,ymax=HI))+ 
     geom_linerange(size=1,position = position_dodge(.5))+geom_point(size=2,position = position_dodge(.5))+
     facet_grid(Type~Group,scales="free")+geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines"))

  p 
    
ggsave("Giving_Games-Standardized-Trunc.pdf",p,height=8, width=14)



################################################################################
 sample_eff<-apply(BG,2,quantile,probs=c(0.05,0.5,0.95))          
df_Hg<-data.frame(IDs=IDs, Type=Type, Group="Allocation",
                 LI=sample_eff[1,],
                 Median=sample_eff[2,],
                 HI=sample_eff[3,]
                 ) 
               
sample_eff<-apply(BL,2,quantile,probs=c(0.05,0.5,0.95))        
df_Hl<-data.frame(IDs=IDs, Type=Type, Group="Leaving",
                 LI=sample_eff[1,],
                 Median=sample_eff[2,],
                 HI=sample_eff[3,]
                 ) 
               
sample_eff<-apply(BP,2,quantile,probs=c(0.05,0.5,0.95))          
df_Hp<-data.frame(IDs=IDs, Type=Type, Group="Reducing",
                 LI=sample_eff[1,],
                 Median=sample_eff[2,],
                 HI=sample_eff[3,]
                 ) 

sample_eff<-apply(BT,2,quantile,probs=c(0.05,0.5,0.95))       
df_Ht<-data.frame(IDs=IDs, Type=Type, Group="Transfers",
                 LI=sample_eff[1,],
                 Median=sample_eff[2,],
                 HI=sample_eff[3,]
                 )   
                 
################################################################################
df_H1 <- rbind(df_Ht,df_Hg,df_Hl,df_Hp)

df_H1$Type <- factor(df_H1$Type, levels(df_H1$Type)[c(3,1,2,4)])

df_H1 <- df_H1[which(df_H1$Type != "Int"),]
colnames(df_H1)[1] <- "Variable"

p <- ggplot(df_H1,aes(x=Variable,y=Median,ymin=LI,ymax=HI))+ 
     geom_linerange(size=1,position = position_dodge(.5))+geom_point(size=2,position = position_dodge(.5))+
     facet_grid(Type~Group,scales="free")+geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines"))

  p 
    
ggsave("All_Games-NonStandardized-Trunc.pdf",p,height=8, width=14)

################################################################################
df_H1 <- df_H1[which(df_H1$Group == "Allocation" | df_H1$Group == "Transfers"),]

p <- ggplot(df_H1,aes(x=Variable,y=Median,ymin=LI,ymax=HI))+ 
     geom_linerange(size=1,position = position_dodge(.5))+geom_point(size=2,position = position_dodge(.5))+
     facet_grid(Type~Group,scales="free")+geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) 

  p 
    
ggsave("Giving_Games-NonStandardized-Trunc.pdf",p,height=8, width=14)

















