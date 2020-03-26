## You will require some packages to run this code. 
## If you don't have any of these installed, first run install.packages("igraph"), etc. for each missing package
## You should first change the working directory to match the folders in which you have stored your data files.
## This can be accomplished by selecting "change working directory" from R's dropdown menu.

## Read in the files that include details of each individual and of each sharing unit
indiv <- read.csv("Data/ENDOW-Ross-Site1-Individual.csv",  header=TRUE, as.is=TRUE)
su <- read.csv("Data/ENDOW-Ross-Site1-Household.csv", header=TRUE, as.is=TRUE)

## Read in the node list file
nl <- read.csv ( file =  "Data/ENDOW-Ross-Site1-Networks.csv", na="", header=TRUE, as.is=TRUE)
gl <- read.csv ( file =  "Data/ENDOW-Ross-Site1-Games.csv", na="", header=TRUE, as.is=TRUE)

###############
##  KINSHIP  ##
###############
kin <- read.csv ( file = "Data/ENDOW-Ross-Site1-Kinship.csv")
kin$gender[kin$sex == "M"] <- 1
kin$gender[kin$sex == "F"] <- 2
kin$gender[is.na(kin$sex)] <- 3
kin$gender[kin$sex == "NA"] <- 3

ped <- pedigree(id=kin$ego, dadid=kin$father, momid=kin$mother, sex = kin$gender, missid = 0)

#plot(ped, cex = .7) ## This is not a necessary step, but it can be helpful to visualize the kinship relations.

rel <- melt ( 2*(as.matrix(kinship(ped))) , varnames = c('i', 'j'), value.name = "r", na.rm = T) ## Multiply by 2 because kinship2 is allelic.

rel1 <- subset (rel, i!=j) ## Removes ego to ego (same person in both columns)
### Next code halves the dataset, retaining only one version of the symmetric dyads
### specifically the one in which "i" comes first in alphabetical order over "j"
rel2 <- subset (rel1, as.character(rel1$i) < as.character(rel1$j))

### Adds in the sharing unit code for each i and j
rel2$i_su <- indiv$HHID [  match(rel2$i,indiv$PID)]
rel2$j_su <- indiv$HHID [  match(rel2$j,indiv$PID)]

## Takes out the individuals who are not members of the study community
rel3 <- subset ( rel2, !(is.na(i_su) | is.na(j_su) ) )

## The following orders the households in alphanumeric order for the creation of a household-dyad
## ID variable.
rel3$first <- ifelse ( as.character(rel3$i_su) < as.character(rel3$j_su), paste(rel3$i_su), paste(rel3$j_su))
rel3$second <- ifelse ( as.character(rel3$i_su) > as.character(rel3$j_su), paste(rel3$i_su), paste(rel3$j_su))
rel3$su_dyad <- paste (rel3$first, "_", rel3$second, sep = "")

## This generates average inter-household relatedness.
su_r <-ddply(rel3, .(su_dyad), plyr::summarize, avg_r=mean(r))

##############
## DISTANCE ##
##############

su_distance <- distm (cbind (su$X/50, su$Y/50))
rownames(su_distance) = su$HHID
colnames(su_distance) = su$HHID

# "ul" is for unit list (as in "edgelist")
ul1 <- melt (su_distance , varnames = c ('ui', 'uj'), value.name = "distance")

## Creation of unit-level dyad ID, as before
ul1$first <- ifelse ( as.character(ul1$ui) < as.character(ul1$uj), paste(ul1$ui), paste(ul1$uj))
ul1$second <- ifelse ( as.character(ul1$ui) > as.character(ul1$uj), paste(ul1$ui), paste(ul1$uj))
ul1$su_dyad <- paste (ul1$first, "_", ul1$second, sep = "")

## Combine the distance and kinship into 
## a single data frame, matching on the "su_dyad" ID
unit_dyads <- merge(ul1,su_r,by=c("su_dyad"))
unit_dyads <- unit_dyads[,c(2,3,4,7)]
unit_dyads$distance <- 6000*(unit_dyads$distance/max(unit_dyads$distance,na.rm=TRUE))

######################
## SUPPORT NETWORKS ##
######################

## We're first generating networks that are as general as possible -- they allow nodes to be either individuals or households

## Create edgelists for each network question
## It is very important that the question number be matched
## correctly to the order in the data needs document.
names<-unique(nl$Question)
el <- vector("list",length(names))
gel <- vector("list",length(names))

for(i in 1:length(names)){
el[[i]]<- nl[which(nl$Question==i),]
}

for(i in 1:3){
gel[[i]]<- gl[which(gl$Question==i),]
}

## We need to reverse tie directionality for some edges (the double-sampled ones)

el[[3]]<-as.data.frame(el[[3]][,c(2,1,3)]) 
colnames(el[[3]])<-c("PID","AID","Question")

## And then combine them into single networks, e.g., a behavioral network, a loan network...
## For now, this uses a simple union rule.
 
elLoan <- rbind(el[[5]])
snLoan <- graph.data.frame(d = elLoan, directed=TRUE)

elExchange <- rbind(el[[3]],el[[4]])
snExchange <- graph.data.frame(d = elExchange, directed = TRUE)

elBehavioral <- rbind(el[[2]])
snBehavioral <- graph.data.frame(d = elBehavioral, directed = TRUE) ## If we think these are undirected, can easily say directed = FALSE

elInformation <- rbind(el[[1]])
snInformation <- graph.data.frame(d = elInformation, directed = TRUE)

elEverMarried <- rbind(el[[6]])
snEverMarried <- graph.data.frame(d = elEverMarried, directed = TRUE)

elMarried <- rbind(el[[7]])
snMarried <- graph.data.frame(d = elEverMarried, directed = TRUE)

elGive <- rbind(gel[[1]])

elLeave <- rbind(gel[[2]])

elPunish <- rbind(gel[[3]])

                              
 G <- as.matrix(elGive)
 G <- G[complete.cases(G),]
 G <- G[which(!G[,2] %in% c("KUT","X2C")),]
labels <- unique( c(G[,1], G[,2]) )

A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[ G[,1:2] ] <- as.numeric( G[,3] )
net = network(A, directed = TRUE)
 ggnet2(net, size="degree", mode = "fruchtermanreingold", layout.par = list(cell.jitter = 1.175), label = TRUE, label.size = 3)
 
 A_Give <- A
 
 
  G <- as.matrix(elLeave)
 G <- G[complete.cases(G),]
 G <- G[which(!G[,2] %in% c("KUT","X2C")),]
labels      <- unique( c(G[,1], G[,2]) )
A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[ G[,1:2] ] <- as.numeric( G[,3] )
net = network(A, directed = TRUE)
 ggnet2(net, size="degree", mode = "fruchtermanreingold", layout.par = list(cell.jitter = 1.175), label = TRUE, label.size = 3)
 
 A_Leave <- A
 
 
  G <- as.matrix(elPunish)
 G <- G[complete.cases(G),]
 G <- G[which(!G[,2] %in% c("KUT","X2C")),]
labels      <- unique( c(G[,1], G[,2]) )
A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[ G[,1:2] ] <- as.numeric( G[,3] )
net = network(A, directed = TRUE)
 ggnet2(net, size="degree", mode = "fruchtermanreingold", layout.par = list(cell.jitter = 1.175), label = TRUE, label.size = 3)
 
 A_Punish <- A
  

G <- as.matrix(elInformation)
G <- G[which((G[,1] %in% colnames(A_Give)) & G[,2] %in% colnames(A_Give)),]
A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[ G[,1:2] ] <- rep(1,length( G[,3] ))
net = network(A, directed = TRUE)
 ggnet2(net, size="degree", mode = "fruchtermanreingold", layout.par = list(cell.jitter = 1.175), label = TRUE, label.size = 3)
 
 A_Friends <- A

G <- as.matrix(elExchange)
G <- G[which((G[,1] %in% colnames(A_Give)) & G[,2] %in% colnames(A_Give)),]
A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[ G[,1:2] ] <- rep(1,length( G[,3] ))
net = network(A, directed = TRUE)
 ggnet2(net, size="degree", mode = "fruchtermanreingold", layout.par = list(cell.jitter = 1.175), label = TRUE, label.size = 3)
 
 A_Exchange <- A
 
  diag(A_Exchange) <- 0
  diag(A_Exchange) <- 116 - rowSums(A_Exchange)
 
 G <- as.matrix(elEverMarried)
G <- G[which((G[,1] %in% colnames(A_Give)) & G[,2] %in% colnames(A_Give)),]
A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[ G[,1:2] ] <- rep(1,length( G[,3] ))
net = network(A, directed = TRUE)
 ggnet2(net, size="degree", mode = "fruchtermanreingold", layout.par = list(cell.jitter = 1.175), label = TRUE, label.size = 3)
 
 A_Married <- A
 

G <- as.matrix(rel1)
G <- G[which((G[,1] %in% colnames(A_Give)) & G[,2] %in% colnames(A_Give)),]
A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[ G[,1:2] ] <-  as.numeric( G[,3] )
net = network(A, directed = TRUE)
 ggnet2(net, size="degree", mode = "fruchtermanreingold", layout.par = list(cell.jitter = 1.175), label = TRUE, label.size = 3)
 
 A_Kin <- A
 
 A_Self<-diag(nrow(A_Give))

MissingFocal <- rep(0,length(A_Give[1,]))
MissingFocal[which(rowSums(A_Give)==0)] <- 1

  
############################################################### Covars
DD<-merge(indiv,su,by="HHID",all.x=TRUE)
DD<-DD[which(DD$PID %in% colnames(A_Give)),]

DD<-DD[match(colnames(A_Give),DD$PID),]

   A_SameSex<- A_Give
  
  for(i in 1:nrow(A_Self)){
  for(j in 1:nrow(A_Self)){
  A_SameSex[i,j] <- ifelse(DD$Sex[i]==DD$Sex[j],1,0)
  }}
  
  A_SameEthnicity<- A_Give
  
  for(i in 1:nrow(A_Self)){
  for(j in 1:nrow(A_Self)){
  A_SameEthnicity[i,j] <- ifelse(DD$Ethnicity[i]==DD$Ethnicity[j],1,0)
  }}
  
  
  A_Community<- A_Give
  
  for(i in 1:nrow(A_Self)){
  for(j in 1:nrow(A_Self)){
  A_Community[i,j] <- ifelse(DD$Barrio.x[i]==DD$Barrio.x[j],1,0)
  }}

colnames(A_Give)==colnames(A_Leave)
colnames(A_Give)==colnames(A_Punish)
colnames(A_Give)==colnames(A_SameEthnicity)
colnames(A_Give)==colnames(A_Kin)
colnames(A_Give)==colnames(A_Friends)
colnames(A_Give)==colnames(A_Community)
colnames(A_Give)==colnames(A_Married)
colnames(A_Give)==colnames(A_Exchange)
colnames(A_Give)==DD$PID

cc <- read.csv("Data/GodsAndPoliceFreeListCodes.csv")
ccCodes<-c(as.character(cc$Codes),as.character(unique(cc$NewCodes)))
ccNewCodes<-c(as.character(cc$NewCodes),as.character(unique(cc$NewCodes)))

DD2 <- as.matrix(DD[,c("GodLikes1","GodLikes2","GodLikes3","GodLikes4","GodLikes5","GodDislikes1","GodDislikes2","GodDislikes3","GodDislikes4","GodDislikes5")])
 
for(i in 1:dim(DD2)[1]){
 for(j in 1:10){      
     if(!is.na(DD2[i,j]) 
     &
     length(which(as.character(DD2[i,j])==as.character(ccCodes)))>0
     ){
    DD2[i,j] <- as.character(ccNewCodes[which(as.character(DD2[i,j])==as.character(ccCodes))])
      }
     else{
     DD2[i,j] <- NA
     }
     }
} 

MorMiss<-ifelse(is.na(DD2) | DD2=="D/K",1,0)
Mor<-ifelse(DD2=="Morality",1,0)
MorMiss<-rowSums(MorMiss,na.rm=TRUE) 
Mor<-rowSums(Mor,na.rm=TRUE) 


DD$Age[which(DD$PID=="VCT")]<-65
DD$Married[which(DD$PID=="YGN")]<-"Yes"

BMI<-DD$Weight/((DD$Height/100)*(DD$Height/100))
BMI[which(is.na(BMI))]<-mean(BMI,na.rm=TRUE)

Sad<-DD$Depressed
Sad[which(is.na(Sad))]<-median(Sad,na.rm=TRUE)

Grip<-DD$Grip
Grip[which(is.na(Grip))]<-mean(Grip,na.rm=TRUE)

Edu<-DD$EducationYears
Edu[which(is.na(Edu))]<-median(Edu,na.rm=TRUE)

PA<-DD$PA
PA[which(is.na(PA))]<-median(PA,na.rm=TRUE)

RelPub <- ifelse(DD$ReligionPublic=="AFEWTIMESPERWEEK" | DD$ReligionPublic=="MORETHANONCEPERWEEK" | DD$ReligionPublic=="ONCEPERWEEK", 1,2) 
RelPri <- ifelse(DD$ReligionPrivate=="EVERYDAY" | DD$ReligionPrivate=="MORETHANONCEPERDAY", 1,2)
GodIneq <- ifelse(DD$GodInequality=="YES", 1,2)

Mor2 <- ifelse(Mor>4,1,2)
P<-25
Pnc <- 12
Psc <- 16

Pb1<-29
Pb2<-115
Pb3<-11

ids <- 1:116

colnames(A_Give) <- colnames(A_Leave) <- colnames(A_Punish) <- colnames(A_Exchange) <- colnames(A_Community) <- colnames(A_Married) <- ids
colnames(A_SameSex) <- colnames(A_SameEthnicity) <- colnames(A_Self) <- colnames(A_Kin) <- colnames(A_Friends) <- ids

rownames(A_Give) <- rownames(A_Leave) <- rownames(A_Punish) <- rownames(A_Exchange) <- rownames(A_Community) <- rownames(A_Married) <- ids
rownames(A_SameSex) <- rownames(A_SameEthnicity) <- rownames(A_Self) <- rownames(A_Kin) <- rownames(A_Friends) <- ids

model_dat_Coast <-list(
N=nrow(A_Self),
P=P,
Pnc=Pnc,
Psc=Psc,
Pb1=Pb1,
Pb2=Pb2,
Pb3=Pb3,

Giving=A_Give,
Taking=A_Leave,
Reducing=A_Punish,  
Transfer=A_Exchange,

SameSex=A_SameSex,
SameEthnicity=A_SameEthnicity,
Other=ifelse(A_Self==1,0,1),
Relatedness=A_Kin*2,
Friends=A_Friends,
Marriage=A_Married,

Age=DD$Age/max(DD$Age,na.rm=TRUE),                      
Male=ifelse(DD$Sex=="M",1,0),                       
CantWork=ifelse(DD$AbilityToWork=="Full",0,1),
Grip=Grip/max(Grip,na.rm=TRUE),
Sad=Sad,
NoFood=ifelse(DD$DaysWithoutSufficientFood>0,1,0),
GoodsValues=DD$GoodsValues/max(DD$GoodsValues,na.rm=TRUE),
Indigenous=ifelse(DD$Ethnicity=="EMBERA",1,0),
NotThere=DD$NotThere,
MissingFocal=MissingFocal
)

                                      
rm(list=setdiff(ls(), c("model_dat_Coast")))
save.image("ColombianDataWithImputations.RData")
rm(list=ls(all=TRUE))






  
 