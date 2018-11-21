

####################################### Stan Model
model_code_controls_trunc="
data{
 int N;
 int Pb1;

 int Giving[N,N];
 int Taking[N,N];
 int Reducing[N,N];
 int Transfer[N,N];

 matrix[N,N] SameEthnicity;
 matrix[N,N] SameSex;
 matrix[N,N] Other;
 matrix[N,N] Relatedness;
 matrix[N,N] Friends;
 matrix[N,N] Community;
 matrix[N,N] Marriage;

 vector[N] Age;
 vector[N] Male;
 vector[N] Mar;
 vector[N] CantWork;
 vector[N] Status;
 vector[N] BMI;
 vector[N] Grip;
 vector[N] Edu;
 vector[N] PA;
 vector[N] Sad;
 vector[N] Poor;
 vector[N] NoFood;
 vector[N] GoodsValues;
 vector[N] Indigenous;
 vector[N] NotThere;
 vector[N] MissingFocal;
}

transformed data{
 matrix[N,N] tGiving;
 matrix[N,N] tTaking;
 matrix[N,N] tReducing;
 matrix[N,N] tTransfer;
 
 for(i in 1:N){
  for(j in 1:N){
  tGiving[i,j] = floor(Giving[j,i]);
  tTaking[i,j] = floor(Taking[j,i]);
  tReducing[i,j] = floor(Reducing[j,i]);
  tTransfer[i,j] = floor(Transfer[j,i]);
}
}
}

parameters {
 vector[Pb1] BG; 
 vector[Pb1] BL; 
 vector[Pb1] BP; 
 vector[Pb1] BT; 
}

model{
//######################################################## Local storage
 real FocalFactors;
 vector[N] TargetFactors;
 row_vector[N] DyadFactors;
 vector[N] Theta;
 real ThetaI;

 //######################################################## Priors
 BG ~ normal(0,1.5);
 
 BL ~ normal(0,1.5);
 
 BP ~ normal(0,1.5);
 
 BT ~ normal(0,1.5);

//######################################################## Model Allocation Data
 for(i in 1:N){
  if(MissingFocal[i]==0){
  FocalFactors  = BG[1] + BG[2]*Age[i] + BG[3]*Male[i] + BG[4]*Indigenous[i] + BG[5]*CantWork[i] + 
                  BG[6]*Grip[i] + BG[7]*Edu[i] + BG[8]*PA[i] + BG[9]*Sad[i] +  BG[10]*NoFood[i] + 
                  BG[11]*GoodsValues[i];  
  
  TargetFactors = BG[12]*Age + BG[13]*Male + BG[14]*Indigenous + BG[15]*CantWork  + BG[16]*Grip + 
                  BG[17]*Edu + BG[18]*PA + BG[19]*Sad + BG[20]*NoFood + BG[21]*GoodsValues + 
                  BG[22]*NotThere; 
    
  DyadFactors   = BG[23]*Relatedness[i] + BG[24]*SameEthnicity[i] + BG[25]*Friends[i] + BG[26]*Community[i] + 
                  BG[27]*Marriage[i] + BG[28]*SameSex[i] + BG[29]*tGiving[i];

  Giving[i] ~ multinomial(softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i])));
  }
 }  
 
//######################################################## Model Taking Data
 for(i in 1:N){
     if(MissingFocal[i]==0){
  FocalFactors  = BL[1] + BL[2]*Age[i] + BL[3]*Male[i] + BL[4]*Indigenous[i] + BL[5]*CantWork[i] + 
                  BL[6]*Grip[i] + BL[7]*Edu[i] + BL[8]*PA[i] + BL[9]*Sad[i] +  BL[10]*NoFood[i] + 
                  BL[11]*GoodsValues[i];  
  
  TargetFactors = BL[12]*Age + BL[13]*Male + BL[14]*Indigenous + BL[15]*CantWork  + BL[16]*Grip + 
                  BL[17]*Edu + BL[18]*PA + BL[19]*Sad + BL[20]*NoFood + BL[21]*GoodsValues + 
                  BL[22]*NotThere; 
    
  DyadFactors   = BL[23]*Relatedness[i] + BL[24]*SameEthnicity[i] + BL[25]*Friends[i] + BL[26]*Community[i] + 
                  BL[27]*Marriage[i] + BL[28]*SameSex[i] + BL[29]*tTaking[i];

  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Taking[i] ~ multinomial(Theta);
  
  
  ThetaI = Theta[i];             // Adjust for constriants of game
  Theta[i] = 0;
  Theta = Theta + ThetaI;  
  
  target += -sum(log(Theta));
  
  
  }
 }   
 
//######################################################## Model Punishment Data
 for(i in 1:N){
     if(MissingFocal[i]==0){
  FocalFactors  = BP[1] + BP[2]*Age[i] + BP[3]*Male[i] + BP[4]*Indigenous[i] + BP[5]*CantWork[i] + 
                  BP[6]*Grip[i] + BP[7]*Edu[i] + BP[8]*PA[i] + BP[9]*Sad[i] +  BP[10]*NoFood[i] + 
                  BP[11]*GoodsValues[i];  
  
  TargetFactors = BP[12]*Age + BP[13]*Male + BP[14]*Indigenous + BP[15]*CantWork  + BP[16]*Grip + 
                  BP[17]*Edu + BP[18]*PA + BP[19]*Sad + BP[20]*NoFood + BP[21]*GoodsValues + 
                  BP[22]*NotThere; 
    
  DyadFactors   = BP[23]*Relatedness[i] + BP[24]*SameEthnicity[i] + BP[25]*Friends[i] + BP[26]*Community[i] + 
                  BP[27]*Marriage[i] + BP[28]*SameSex[i] + BP[29]*tReducing[i];

  Reducing[i] ~ multinomial(softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i])));
 }     
 }
 
//######################################################## Model Transfer Data
 for(i in 1:N){
      if(MissingFocal[i]==0){
  FocalFactors  = BT[1] + BT[2]*Age[i] + BT[3]*Male[i] + BT[4]*Indigenous[i] + BT[5]*CantWork[i] + 
                  BT[6]*Grip[i] + BT[7]*Edu[i] + BT[8]*PA[i] + BT[9]*Sad[i] +  BT[10]*NoFood[i] + 
                  BT[11]*GoodsValues[i];  
  
  TargetFactors = BT[12]*Age + BT[13]*Male + BT[14]*Indigenous + BT[15]*CantWork  + BT[16]*Grip + 
                  BT[17]*Edu + BT[18]*PA + BT[19]*Sad + BT[20]*NoFood + BT[21]*GoodsValues + 
                  BT[22]*NotThere; 
    
  DyadFactors   = BT[23]*Relatedness[i] + BT[24]*SameEthnicity[i] + BT[25]*Friends[i] + BT[26]*Community[i] + 
                  BT[27]*Marriage[i] + BT[28]*SameSex[i] + BT[29]*tTransfer[i];
  
  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Transfer[i] ~ multinomial(Theta);
  
  
  ThetaI = Theta[i];             // Adjust for constriants of game
  Theta[i] = 0;
  Theta = Theta + ThetaI;  
  
  target += -sum(log(Theta));
  
   }
 }             
                       
}


"                                                       
 
 
 