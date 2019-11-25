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
 matrix[N,N] Marriage;

 vector[N] Age;
 vector[N] Male;
 vector[N] CantWork;
 vector[N] Grip;
 vector[N] Sad;
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
 vector[24] BG; 
 vector[24] BL; 
 vector[24] BP; 
 vector[24] BT; 

 vector[N] F_raw[4]; 
 vector[N] A_raw[4]; 

 vector<lower=0>[4] F_SD;
 vector<lower=0>[4] A_SD;
}

transformed parameters{
 vector[N] F[4]; 
 vector[N] A[4];  

 for(i in 1:4){
  F[i] = F_raw[i]*F_SD[i];
  A[i] = A_raw[i]*A_SD[i];
 }

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

 for(i in 1:4){
  F_raw[i] ~ normal(0,1);
  A_raw[i] ~ normal(0,1);
 }

 F_SD ~ exponential(1.5);
 A_SD ~ exponential(1.5);

//######################################################## Model Allocation Data
 for(i in 1:N){
  if(MissingFocal[i]==0){
  FocalFactors  = BG[1] + BG[2]*Age[i] + BG[3]*Male[i] + BG[4]*Indigenous[i] + BG[5]*CantWork[i] + 
                  BG[6]*Grip[i] + BG[7]*Sad[i] +  BG[8]*NoFood[i] +  BG[9]*GoodsValues[i] + F[1,i];  
  
  TargetFactors = BG[10]*Age + BG[11]*Male + BG[12]*Indigenous + BG[13]*CantWork + BG[14]*Grip + 
                  BG[15]*Sad + BG[16]*NoFood + BG[17]*GoodsValues + BG[18]*NotThere + A[1]; 
    
  DyadFactors   = BG[19]*Relatedness[i] + BG[20]*SameEthnicity[i] + BG[21]*Friends[i] + 
                  BG[22]*Marriage[i] + BG[23]*SameSex[i] + BG[24]*tGiving[i];

  Giving[i] ~ multinomial(softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i])));
  }
 }  
 
//######################################################## Model Taking Data
 for(i in 1:N){
     if(MissingFocal[i]==0){
  FocalFactors  = BL[1] + BL[2]*Age[i] + BL[3]*Male[i] + BL[4]*Indigenous[i] + BL[5]*CantWork[i] + 
                  BL[6]*Grip[i] + BL[7]*Sad[i] +  BL[8]*NoFood[i] +  BL[9]*GoodsValues[i] + F[2,i];  
  
  TargetFactors = BL[10]*Age + BL[11]*Male + BL[12]*Indigenous + BL[13]*CantWork + BL[14]*Grip + 
                  BL[15]*Sad + BL[16]*NoFood + BL[17]*GoodsValues + BL[18]*NotThere + A[2]; 
    
  DyadFactors   = BL[19]*Relatedness[i] + BL[20]*SameEthnicity[i] + BL[21]*Friends[i] + 
                  BL[22]*Marriage[i] + BL[23]*SameSex[i] + BL[24]*tTaking[i];

  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Taking[i] ~ multinomial(Theta);  
  }
 }   
 
//######################################################## Model Punishment Data
 for(i in 1:N){
     if(MissingFocal[i]==0){
  FocalFactors  = BP[1] + BP[2]*Age[i] + BP[3]*Male[i] + BP[4]*Indigenous[i] + BP[5]*CantWork[i] + 
                  BP[6]*Grip[i] + BP[7]*Sad[i] +  BP[8]*NoFood[i] +  BP[9]*GoodsValues[i] + F[3,i];  
  
  TargetFactors = BP[10]*Age + BP[11]*Male + BP[12]*Indigenous + BP[13]*CantWork + BP[14]*Grip + 
                  BP[15]*Sad + BP[16]*NoFood + BP[17]*GoodsValues + BP[18]*NotThere + A[3]; 
    
  DyadFactors   = BP[19]*Relatedness[i] + BP[20]*SameEthnicity[i] + BP[21]*Friends[i] + 
                  BP[22]*Marriage[i] + BP[23]*SameSex[i] + BP[24]*tReducing[i];

  Reducing[i] ~ multinomial(softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i])));
 }     
 }
 
//######################################################## Model Transfer Data
 for(i in 1:N){
      if(MissingFocal[i]==0){
  FocalFactors  = BT[1] + BT[2]*Age[i] + BT[3]*Male[i] + BT[4]*Indigenous[i] + BT[5]*CantWork[i] + 
                  BT[6]*Grip[i] + BT[7]*Sad[i] +  BT[8]*NoFood[i] +  BT[9]*GoodsValues[i] + F[4,i];  
  
  TargetFactors = BT[10]*Age + BT[11]*Male + BT[12]*Indigenous + BT[13]*CantWork + BT[14]*Grip + 
                  BT[15]*Sad + BT[16]*NoFood + BT[17]*GoodsValues + BT[18]*NotThere + A[4]; 
    
  DyadFactors   = BT[19]*Relatedness[i] + BT[20]*SameEthnicity[i] + BT[21]*Friends[i] + 
                  BT[22]*Marriage[i] + BT[23]*SameSex[i] + BT[24]*tTransfer[i];
  
  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Transfer[i] ~ multinomial(Theta);
   }
 }             
                       
}
                                                     
 
 
 