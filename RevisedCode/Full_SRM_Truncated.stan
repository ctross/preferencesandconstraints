
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
 vector[23] BG; 
 vector[23] BL; 
 vector[23] BP; 
 vector[23] BT; 

 vector[2] FA_raw[N, 4]; 
 vector<lower=0>[2] FA_SD[4];
 cholesky_factor_corr[2] FA_L_chol[4];

 matrix[N,N] D_raw[4];
 real<lower=0> D_SD[4];
 cholesky_factor_corr[2] D_L_chol[4];
}

model{
//######################################################## Local storage
 real FocalFactors;
 vector[N] TargetFactors;
 row_vector[N] DyadFactors;
 vector[N] Theta;
 real ThetaI;

 vector[N] F[4];
 vector[N] A[4];

 vector[2] FA[N, 4];  
 matrix[N,N] D[4];
 vector[2] scrap;

//######################################################## Priors
 BG ~ normal(0,1.5);
 
 BL ~ normal(0,1.5);

 BP ~ normal(0,1.5);
 
 BT ~ normal(0,1.5);

 for(i in 1:N){
 for(p in 1:4){
  FA_raw[i,p] ~ normal(0,1);
 }}

 for(p in 1:4){
  to_vector(D_raw[p]) ~ normal(0,1);
 }

for(p in 1:4){
 FA_SD[p] ~ exponential(1.5);
 FA_L_chol[p] ~ lkj_corr_cholesky(2);
}

 for(p in 1:4){
 D_SD[p] ~ exponential(1.5);
 D_L_chol[p] ~ lkj_corr_cholesky(2);
}

//######################################################## Transformed Priors


 for(i in 1:N){
  for(p in 1:4){
   FA[i,p] = FA_SD[p] .* (FA_L_chol[p]*FA_raw[i, p]);
  }}

 for(p in 1:4){
 for(i in 1:N){
  F[p,i] = FA[i,p,1];
  A[p,i] = FA[i,p,2];
  }}

for(p in 1:4){
   for(i in 1:(N-1)){
   for(j in (i+1):N){  
      scrap[1] = D_raw[p,i,j];
      scrap[2] = D_raw[p,j,i];
      scrap = rep_vector(D_SD[p],2) .* (D_L_chol[p]*scrap);
    D[p,i,j] = scrap[1];           
    D[p,j,i] = scrap[2];                       
    }}
    
   for(i in 1:N){
    D[p,i,i] = -99;                                
    }
  }


//######################################################## Model Allocation Data
 for(i in 1:N){
  if(MissingFocal[i]==0){
  FocalFactors  = BG[1] + BG[2]*Age[i] + BG[3]*Male[i] + BG[4]*Indigenous[i] + BG[5]*CantWork[i] + 
                  BG[6]*Grip[i] + BG[7]*Sad[i] +  BG[8]*NoFood[i] +  BG[9]*GoodsValues[i] + F[1,i];  
  
  TargetFactors = BG[10]*Age + BG[11]*Male + BG[12]*Indigenous + BG[13]*CantWork + BG[14]*Grip + 
                  BG[15]*Sad + BG[16]*NoFood + BG[17]*GoodsValues + BG[18]*NotThere + A[1];  
    
  DyadFactors   = BG[19]*Relatedness[i] + BG[20]*SameEthnicity[i] + BG[21]*Friends[i] + 
                  BG[22]*Marriage[i] + BG[23]*SameSex[i] + D[1][i];

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
                  BL[22]*Marriage[i] + BL[23]*SameSex[i] + D[2][i];

  //Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  //Taking[i] ~ multinomial(Theta);  
  //ThetaI = Theta[i];             // Adjust for constriants of game
  //Theta[i] = 0;
  //Theta = Theta + ThetaI;  
  //target += -sum(log(Theta));
  //This model does not run right under both truncation and full SRM, many divergent transitions
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
                  BP[22]*Marriage[i] + BP[23]*SameSex[i] + D[3][i];

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
                  BT[22]*Marriage[i] + BT[23]*SameSex[i] + D[4][i];
  
  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Transfer[i] ~ multinomial(Theta);

  ThetaI = Theta[i];             // Adjust for constriants of game
  Theta[i] = 0;
  Theta = Theta + ThetaI;  
  
  target += -sum(log(Theta));

   }
 }                                 
}
                                                     
generated quantities{
 matrix[2,2] FA_corr[4];  
 matrix[2,2] D_corr[4];
 real FA_Rho[4];
 real D_Rho[4];

 for(p in 1:4){
  FA_corr[p] = tcrossprod(FA_L_chol[p]); 
  D_corr[p] = tcrossprod(D_L_chol[p]); 
  FA_Rho[p] = FA_corr[p,1,2]; 
  D_Rho[p] = D_corr[p,1,2];
 }
}   
                 
               
 