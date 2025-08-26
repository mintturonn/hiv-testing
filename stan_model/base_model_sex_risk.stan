 data {
  int timespan;
  real tstep;
 // int tstep_inv;
  int timestep;
  int tlength[timespan];
  int ag;
  int d;
  int s;
  int r;
  vector[timestep] yrs;
  vector[timespan] yrs0;
  int dv[9];
  int dmsmv[5];
  int dm[4];
  int d_denom;
  int d_denom2;
  int d_denom3;
  // int drec[5];
  // int d_rec_tested[2,5];
  int d_testvol[3];
  real beta_testcov[2];
  int d_tested_f[ag,9];
  int d_tested_m[ag,9];
  int d_tested_msm[ag-2,5];
  
  int d_tested_f2[3,4];
  int d_tested_m2[3,4];
  int d_tested_msm2[3,4];
  
  int d_tested_recent_f2[3,4];
  int d_tested_recent_m2[3,4];
  int d_tested_recent_msm2[3,4];
  int d_tested_recent_msm[ag-2,5];
  
  int d_tested_f_r1[ag-2];
  int d_tested_f_r2[ag-2];
  int d_tested_m_r1[ag-2];
  int d_tested_m_r2[ag-2];

  real n0[ag, s];
  real aging[ag-1];
  real mort[4,s];
  real popgr[s];
  real risk_pr[ag,s];
  real risk_ageout[ag-1,s];

}

parameters {

  real<lower=0, upper = 1> beta_rr1_w;
  real<lower=0, upper = 1> beta_rr2_w;
  real<lower=0, upper = 1> beta_rr3_w;
  real<lower=0, upper = 1> beta_rr1_msw;
  real<lower=0, upper = 1> beta_rr2_msw;
  real<lower=0, upper = 1> beta_rr3_msw;
  real<lower=0, upper = 1> beta_rr1_msm;
  real<lower=0, upper = 1> beta_rr2_msm;
  real<lower=0, upper = 1> beta_rr3_msm;
  
  real<lower=0> rr_beta2_w;
  real<lower=0> rr_beta2_msw;
  real<lower=0> rr_beta2_msm;
  
  real<lower=0, upper = 1> rr_lr_w;
  real<lower=0, upper = 1> rr_lr_msw;
  real<lower=0, upper = 1> rr_lr_msm;
  
  real<lower=0, upper = 1> beta_scr[ag,s]; //by age and sex and risk
  real<lower=0> mu_test;
  real<lower=0, upper = 1> coverage;
  matrix<lower=0, upper = 1>[ag,s] bp0;
  real<lower=0> bp0_rr[s];
}

transformed parameters{
  
  real<lower=0, upper = 1> beta_hr1[ag];
  real<lower=0, upper = 1> beta_hr2[ag];
  real<lower=0, upper = 1> beta_hr3[ag];
  real<lower=0, upper = 1> beta[ag,s,r,timespan]; //by age and sex and risk and year 
  real<lower=0, upper = 1> beta2[ag,s,r,timespan]; // by age and sex and hiv status and risk  and year

  real<lower=0> test_nums_ts[ag,s,r,timestep]; 
  real<lower=0> test_rec_nums_ts[ag,s,r,timestep]; 
  real<lower=0> test_nums[ag,s,r,timespan];
  real<lower=0> test_rec_nums[ag,s,r,timespan];
  real<lower=0, upper = 1> pr_tested[ag,s,timespan];
  real<lower=0, upper = 1> pr_tested_r1[ag,s,timespan];
  real<lower=0, upper = 1.5> pr_tested_r2[ag,s,timespan];
  real<lower=0, upper = 1> pr_rec_tested[ag,s,timespan]; 
  real<lower=0, upper = 1> pr_rec_tested_r1[ag,s,timespan]; 
  real<lower=0, upper = 1.5> pr_rec_tested_r2[ag,s,timespan]; 
  real testvol_est[3];

  real<lower=0> s0[ag,s,r];
  real<lower=0> b0[ag,s,r];
  real<lower=0> S[ag,s,r,timestep]; 
  real<lower=0> B[ag,s,r,timestep]; 
  
 real ageout_temp[s,timestep];
// 
  /////  ===== Women testing =====
  for (i in 1:ag) {
    // HR Women
    beta_hr1[i] = beta_scr[i,1]   *  (1 + beta_rr1_w);
    beta_hr2[i] = beta_hr1[i]     *  (1 + beta_rr2_w);
    beta_hr3[i] = beta_hr2[i]     *  (1 + beta_rr3_w);

    for (y in 1:timespan) {
      if (yrs0[y] <= 2005) {
      // Women HR
        beta[i,1,2,y]    = beta_scr[i,1]    + (beta_hr1[i] - beta_scr[i,1])    * (yrs0[y] - 1999) / (2005 - 1999);
        beta2[i,1,2,y] =  beta[i,1,2,y];  //rr_beta2_w * beta[i,1,2,y];  // test again 
      // Women LR
        beta[i,1,1,y]  =  rr_lr_w  * beta[i,1,2,y];
        beta2[i,1,1,y] =  rr_lr_w  * beta2[i,1,2,y];  // test again 
        
     } else if (yrs0[y] <= 2014) { 
      // Women HR
        beta[i,1,2,y]  = beta_hr1[i] + (beta_hr2[i] - beta_hr1[i]) * (yrs0[y] - 2005) / (2014 - 2005);
        beta2[i,1,2,y] = beta[i,1,2,y]; //rr_beta2_w * beta[i,1,2,y]; // test again 
      // Women LR
        beta[i,1,1,y]  =  rr_lr_w  * beta[i,1,2,y];
        beta2[i,1,1,y] =  rr_lr_w  * beta2[i,1,2,y];  // test again 
       
     } else {
      // Women HR
        beta[i,1,2,y]  = beta_hr2[i] + (beta_hr3[i] - beta_hr2[i]) * (yrs0[y] - 2014) / (2024 - 2014);
        beta2[i,1,2,y] = beta[i,1,2,y]; // rr_beta2_w * beta[i,1,2,y]; // test again 
       
      // Women LR
        beta[i,1,1,y]  =  rr_lr_w  * beta[i,1,2,y];
        beta2[i,1,1,y] =  rr_lr_w  * beta2[i,1,2,y];  // test again 
    }
   }
  }
  
  /////  ===== MSW testing =====
  for (i in 1:ag) {
    // HR MSW
    beta_hr1[i] = beta_scr[i,2]   * (1 + beta_rr1_msw);
    beta_hr2[i] = beta_hr1[i]     * (1 + beta_rr2_msw);
    beta_hr3[i] = beta_hr2[i]     * (1 + beta_rr3_msw);

    for (y in 1:timespan) {
      if (yrs0[y] <= 2005) {
      // MSW HR
        beta[i,2,2,y]    = beta_scr[i,2]    + (beta_hr1[i] - beta_scr[i,2])    * (yrs0[y] - 1999) / (2005 - 1999);
        beta2[i,2,2,y] =  beta[i,2,2,y];  //rr_beta2_msw * beta[i,2,2,y];  // test again 
      // MSW LR
        beta[i,2,1,y]    =  rr_lr_msw  * beta[i,2,2,y];
        beta2[i,2,1,y] =  rr_lr_msw  * beta2[i,2,2,y];  // test again 
        
     } else if (yrs0[y] <= 2014) { 
      // MSW HR
        beta[i,2,2,y]    = beta_hr1[i] + (beta_hr2[i] - beta_hr1[i]) * (yrs0[y] - 2005) / (2014 - 2005);
        beta2[i,2,2,y] = beta[i,2,2,y]; //rr_beta2_msw * beta[i,2,2,y]; // test again 
      // MSW LR
        beta[i,2,1,y]    =  rr_lr_msw  * beta[i,2,2,y];
        beta2[i,2,1,y] =  rr_lr_msw  * beta2[i,2,2,y];  // test again 
       
     } else {
      // MSW HR
        beta[i,2,2,y]    = beta_hr2[i] + (beta_hr3[i] - beta_hr2[i]) * (yrs0[y] - 2014) / (2024 - 2014);
        beta2[i,2,2,y] = beta[i,2,2,y]; //rr_beta2_msw * beta[i,2,2,y]; // test again 
      // MSW LR
        beta[i,2,1,y] =  rr_lr_msw  * beta[i,2,2,y];
        beta2[i,2,1,y] =  rr_lr_msw  * beta2[i,2,2,y];  // test again 
    }
   }
  }
  
  /////  ===== MSM testing =====
  for (i in 1:ag) {
    // HR MSM
    beta_hr1[i] = beta_scr[i,3]   * (1 + beta_rr1_msm);
    beta_hr2[i] = beta_hr1[i]     * (1 + beta_rr2_msm);
    beta_hr3[i] = beta_hr2[i]     * (1 + beta_rr3_msm);

    for (y in 1:timespan) {
      if (yrs0[y] <= 2005) {
      // MSM HR
        beta[i,3,2,y]    = beta_scr[i,3]    + (beta_hr1[i] - beta_scr[i,3])    * (yrs0[y] - 1999) / (2005 - 1999);
        beta2[i,3,2,y] =  beta[i,3,2,y];  // rr_beta2_msm * beta[i,3,2,y];  // test again 
      // MSM LR
        beta[i,3,1,y]    =  rr_lr_msm * beta[i,3,2,y];
        beta2[i,3,1,y] =  rr_lr_msm * beta2[i,3,2,y];  // test again 
      
     } else if (yrs0[y] <= 2014) { 
      // MSM HR
        beta[i,3,2,y]    = beta_hr1[i] + (beta_hr2[i] - beta_hr1[i]) * (yrs0[y] - 2005) / (2014 - 2005);
        beta2[i,3,2,y] = beta[i,3,2,y]; //rr_beta2_msm * beta[i,3,2,y]; // test again 
      // MSM LR
        beta[i,3,1,y]    =  rr_lr_msm  * beta[i,3,2,y];
        beta2[i,3,1,y] =  rr_lr_msm  * beta2[i,3,2,y];  // test again 
    
     } else {
      // MSM HR
        beta[i,3,2,y]    = beta_hr2[i] + (beta_hr3[i] - beta_hr2[i]) * (yrs0[y] - 2014) / (2024 - 2014);
        beta2[i,3,2,y] = beta[i,3,2,y]; // rr_beta2_msm * beta[i,3,2,y]; // test again 
      // MSM LR
        beta[i,3,1,y]    =  rr_lr_msm  * beta[i,3,2,y];
        beta2[i,3,1,y] =  rr_lr_msm  * beta2[i,3,2,y];  // test again 
    }
   }
  }

 for (k in 1:s){  
    b0[1,k,1] = (1-risk_pr[1,k])*bp0_rr[k]*bp0[1,k]*n0[1,k];
    b0[2,k,1] = (1-risk_pr[2,k])*bp0_rr[k]*bp0[2,k]*n0[2,k];
    b0[3,k,1] = (1-risk_pr[3,k])*bp0_rr[k]*bp0[3,k]*n0[3,k];
    b0[4,k,1] = (1-risk_pr[4,k])*bp0_rr[k]*bp0[4,k]*n0[4,k];
    b0[5,k,1] = (1-risk_pr[5,k])*bp0_rr[k]*bp0[5,k]*n0[5,k];
    b0[6,k,1] = (1-risk_pr[6,k])*bp0_rr[k]*bp0[6,k]*n0[6,k];
    b0[7,k,1] = (1-risk_pr[7,k])*bp0_rr[k]*bp0[7,k]*n0[7,k];
    
    b0[1,k,2] = risk_pr[1,k]*bp0[1,k]*n0[1,k];
    b0[2,k,2] = risk_pr[2,k]*bp0[2,k]*n0[2,k];
    b0[3,k,2] = risk_pr[3,k]*bp0[3,k]*n0[3,k];
    b0[4,k,2] = risk_pr[4,k]*bp0[4,k]*n0[4,k];
    b0[5,k,2] = risk_pr[5,k]*bp0[5,k]*n0[5,k];
    b0[6,k,2] = risk_pr[6,k]*bp0[6,k]*n0[6,k];
    b0[7,k,2] = risk_pr[7,k]*bp0[7,k]*n0[7,k];

    s0[1,k,1] = (1-risk_pr[1,k])*n0[1,k] - b0[1,k,1] ;
    s0[2,k,1] = (1-risk_pr[2,k])*n0[2,k] - b0[2,k,1] ;
    s0[3,k,1] = (1-risk_pr[3,k])*n0[3,k] - b0[3,k,1] ;
    s0[4,k,1] = (1-risk_pr[4,k])*n0[4,k] - b0[4,k,1] ;
    s0[5,k,1] = (1-risk_pr[5,k])*n0[5,k] - b0[5,k,1] ;
    s0[6,k,1] = (1-risk_pr[6,k])*n0[6,k] - b0[6,k,1] ;
    s0[7,k,1] = (1-risk_pr[7,k])*n0[7,k] - b0[7,k,1] ;
    
    s0[1,k,2] = risk_pr[1,k]*n0[1,k] - b0[1,k,2] ;
    s0[2,k,2] = risk_pr[2,k]*n0[2,k] - b0[2,k,2] ;
    s0[3,k,2] = risk_pr[3,k]*n0[3,k] - b0[3,k,2] ;
    s0[4,k,2] = risk_pr[4,k]*n0[4,k] - b0[4,k,2] ;
    s0[5,k,2] = risk_pr[5,k]*n0[5,k] - b0[5,k,2] ;
    s0[6,k,2] = risk_pr[6,k]*n0[6,k] - b0[6,k,2] ;
    s0[7,k,2] = risk_pr[7,k]*n0[7,k] - b0[7,k,2] ;
  }
  
 for (i in 1:ag){
      for (k in 1:s){
         for (c in 1:r){
           for (y in 1:timestep){
       S[i,k,c,y] = 0.0;
       B[i,k,c,y] = 0.0;
         }
       }
     }
    }
      
 for (i in 1:ag){ 
     for (k in 1:s){  
    
  // lower risk     
   S[i,k,1,1] = s0[i,k,1];
   B[i,k,1,1] = b0[i,k,1];
   
  // higher risk 
   S[i,k,2,1] = s0[i,k,2];
   B[i,k,2,1] = b0[i,k,2];
   
   test_nums_ts[i,k,1,1] = 0.0;
   test_nums_ts[i,k,2,1] = 0.0;
   test_rec_nums_ts[i,k,1,1] = 0.0;
   test_rec_nums_ts[i,k,2,1] = 0.0;
  }
 }
    ageout_temp[1,1] = 0;
    ageout_temp[2,1] = 0;
    ageout_temp[3,1] = 0;
  //  test_ts[1] = 0;
  
 // print(to_int((timestep-1.5) / 2 + 1));

for (y in 2:(timestep) ){
   int year_idx = to_int((y-1.5) / 2 + 1); // integer division
    //print(year_idx);
    for (k in 1:s){   
          ageout_temp[k,y-1] =   popgr[k]* ( mort[1,k]*(S[4,k,1,y-1] + B[4,k,1,y-1] ) +
                                             mort[2,k]*(S[5,k,1,y-1] + B[5,k,1,y-1] ) +
                                             mort[3,k]*(S[6,k,1,y-1] + B[6,k,1,y-1] ) +
                                             mort[4,k]*(S[7,k,1,y-1] + B[7,k,1,y-1] ) +
                                             mort[1,k]*(S[4,k,2,y-1] + B[4,k,2,y-1] ) +
                                             mort[2,k]*(S[5,k,2,y-1] + B[5,k,2,y-1] ) +
                                             mort[3,k]*(S[6,k,2,y-1] + B[6,k,2,y-1] ) +
                                             mort[4,k]*(S[7,k,2,y-1] + B[7,k,2,y-1] )  ) ;
// HIV negative population    
// lower risk
          S[1,k,1,y] = S[1,k,1,y-1] + ( - beta[1,k,1,year_idx]*S[1,k,1,y-1] - S[1,k,1,y-1]*aging[1] + (1-risk_pr[1,k])*ageout_temp[k,y-1] )* tstep;
          B[1,k,1,y] = B[1,k,1,y-1] + (   beta[1,k,1,year_idx]*S[1,k,1,y-1]   -B[1,k,1,y-1]*aging[1] )* tstep  ;
          
          S[2,k,1,y] = S[2,k,1,y-1] + ( - beta[2,k,1,year_idx]*S[2,k,1,y-1]  + S[1,k,1,y-1]*aging[1] +risk_ageout[1,k]*S[1,k,2,y-1]*aging[1]-S[2,k,1,y-1]*aging[2])* tstep ;
          B[2,k,1,y] = B[2,k,1,y-1] + (   beta[2,k,1,year_idx]*S[2,k,1,y-1] +B[1,k,1,y-1]*aging[1] +risk_ageout[1,k]*B[1,k,2,y-1]*aging[1]-B[2,k,1,y-1]*aging[2] )* tstep ;
         
          S[3,k,1,y] = S[3,k,1,y-1] + ( - beta[3,k,1,year_idx]*S[3,k,1,y-1] + S[2,k,1,y-1]*aging[2]+risk_ageout[2,k]*S[2,k,2,y-1]*aging[2]-S[3,k,1,y-1]*aging[3] )* tstep ;
          B[3,k,1,y] = B[3,k,1,y-1] + (   beta[3,k,1,year_idx]*S[3,k,1,y-1] +B[2,k,1,y-1]*aging[2] +risk_ageout[2,k]*B[2,k,2,y-1]*aging[2]-B[3,k,1,y-1]*aging[3] )* tstep ;
          
          S[4,k,1,y] = S[4,k,1,y-1] + ( - beta[4,k,1,year_idx]*S[4,k,1,y-1] +S[3,k,1,y-1]*aging[3] +risk_ageout[3,k]*S[3,k,2,y-1]*aging[3]-S[4,k,1,y-1]*aging[4] - S[4,k,1,y-1]*mort[1,k])* tstep ;
          B[4,k,1,y] = B[4,k,1,y-1] + (   beta[4,k,1,year_idx]*S[4,k,1,y-1] +B[3,k,1,y-1]*aging[3] +risk_ageout[3,k]*B[3,k,2,y-1]*aging[3]-B[4,k,1,y-1]*aging[4] - B[4,k,1,y-1]*mort[1,k])* tstep ;
          
          S[5,k,1,y] = S[5,k,1,y-1] + ( - beta[5,k,1,year_idx]*S[5,k,1,y-1] +S[4,k,1,y-1]*aging[4] +risk_ageout[4,k]*S[4,k,2,y-1]*aging[4]-S[5,k,1,y-1]*aging[5] - S[5,k,1,y-1]*mort[2,k])* tstep ;
          B[5,k,1,y] = B[5,k,1,y-1] + (   beta[5,k,1,year_idx]*S[5,k,1,y-1] +B[4,k,1,y-1]*aging[4] +risk_ageout[4,k]*B[4,k,2,y-1]*aging[4]-B[5,k,1,y-1]*aging[5] - B[5,k,1,y-1]*mort[2,k])* tstep ;
     
          S[6,k,1,y] = S[6,k,1,y-1] + ( - beta[6,k,1,year_idx]*S[6,k,1,y-1]  +S[5,k,1,y-1]*aging[5] +risk_ageout[5,k]*S[5,k,2,y-1]*aging[5]-S[6,k,1,y-1]*aging[6] - S[6,k,1,y-1]*mort[3,k])* tstep ;
          B[6,k,1,y] = B[6,k,1,y-1] + (   beta[6,k,1,year_idx]*S[6,k,1,y-1] +B[5,k,1,y-1]*aging[5] +risk_ageout[5,k]*B[5,k,2,y-1]*aging[5]-B[6,k,1,y-1]*aging[6] - B[6,k,1,y-1]*mort[3,k])* tstep ;
 
          S[7,k,1,y] = S[7,k,1,y-1] + ( - beta[7,k,1,year_idx]*S[7,k,1,y-1] +S[6,k,1,y-1]*aging[6] +risk_ageout[6,k]*S[6,k,2,y-1]*aging[6]- S[7,k,1,y-1]*mort[4,k])* tstep ;
          B[7,k,1,y] = B[7,k,1,y-1] + (   beta[7,k,1,year_idx]*S[7,k,1,y-1] +B[6,k,1,y-1]*aging[6] +risk_ageout[6,k]*B[6,k,2,y-1]*aging[6]- B[7,k,1,y-1]*mort[4,k])* tstep ;
 
// higher risk
          S[1,k,2,y] = S[1,k,2,y-1] + ( - beta[1,k,2,year_idx]*S[1,k,2,y-1]  -S[1,k,2,y-1]*aging[1] + risk_pr[1,k]*ageout_temp[k,y-1] )* tstep;
          B[1,k,2,y] = B[1,k,2,y-1] + (   beta[1,k,2,year_idx]*S[1,k,2,y-1]  -B[1,k,2,y-1]*aging[1] )* tstep  ;
          
          S[2,k,2,y] = S[2,k,2,y-1] + ( - beta[2,k,2,year_idx]*S[2,k,2,y-1]  +(1-risk_ageout[1,k])*S[1,k,2,y-1]*aging[1] -S[2,k,2,y-1]*aging[2])* tstep ;
          B[2,k,2,y] = B[2,k,2,y-1] + (   beta[2,k,2,year_idx]*S[2,k,2,y-1]  +(1-risk_ageout[1,k])*B[1,k,2,y-1]*aging[1] -B[2,k,2,y-1]*aging[2] )* tstep ;
         
          S[3,k,2,y] = S[3,k,2,y-1] + ( - beta[3,k,2,year_idx]*S[3,k,2,y-1]  +(1-risk_ageout[2,k])*S[2,k,2,y-1]*aging[2] -S[3,k,2,y-1]*aging[3] )* tstep ;
          B[3,k,2,y] = B[3,k,2,y-1] + (   beta[3,k,2,year_idx]*S[3,k,2,y-1]  +(1-risk_ageout[2,k])*B[2,k,2,y-1]*aging[2] -B[3,k,2,y-1]*aging[3] )* tstep ;
          
          S[4,k,2,y] = S[4,k,2,y-1] + ( - beta[4,k,2,year_idx]*S[4,k,2,y-1]  +(1-risk_ageout[3,k])*S[3,k,2,y-1]*aging[3] -S[4,k,2,y-1]*aging[4] - S[4,k,2,y-1]*mort[1,k])* tstep ;
          B[4,k,2,y] = B[4,k,2,y-1] + (   beta[4,k,2,year_idx]*S[4,k,2,y-1]  +(1-risk_ageout[3,k])*B[3,k,2,y-1]*aging[3] -B[4,k,2,y-1]*aging[4] - B[4,k,2,y-1]*mort[1,k])* tstep ;
          
          S[5,k,2,y] = S[5,k,2,y-1] + ( - beta[5,k,2,year_idx]*S[5,k,2,y-1]  +(1-risk_ageout[4,k])*S[4,k,2,y-1]*aging[4] -S[5,k,2,y-1]*aging[5] - S[5,k,2,y-1]*mort[2,k])* tstep ;
          B[5,k,2,y] = B[5,k,2,y-1] + (   beta[5,k,2,year_idx]*S[5,k,2,y-1]  +(1-risk_ageout[4,k])*B[4,k,2,y-1]*aging[4] -B[5,k,2,y-1]*aging[5] - B[5,k,2,y-1]*mort[2,k])* tstep ;
     
          S[6,k,2,y] = S[6,k,2,y-1] + ( - beta[6,k,2,year_idx]*S[6,k,2,y-1]  +(1-risk_ageout[5,k])*S[5,k,2,y-1]*aging[5] -S[6,k,2,y-1]*aging[6] - S[6,k,2,y-1]*mort[3,k])* tstep ;
          B[6,k,2,y] = B[6,k,2,y-1] + (   beta[6,k,2,year_idx]*S[6,k,2,y-1]  +(1-risk_ageout[5,k])*B[5,k,2,y-1]*aging[5] -B[6,k,2,y-1]*aging[6] - B[6,k,2,y-1]*mort[3,k])* tstep ;
 
          S[7,k,2,y] = S[7,k,2,y-1] + ( - beta[7,k,2,year_idx]*S[7,k,2,y-1]  +(1-risk_ageout[6,k])*S[6,k,2,y-1]*aging[6] -S[7,k,2,y-1]*mort[4,k])* tstep ;
          B[7,k,2,y] = B[7,k,2,y-1] + (   beta[7,k,2,year_idx]*S[7,k,2,y-1]  +(1-risk_ageout[6,k])*B[6,k,2,y-1]*aging[6] -B[7,k,2,y-1]*mort[4,k])* tstep ;
          
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////            

// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////          
// summary estimates
 for (c in 1:r){
          test_rec_nums_ts[1,k,c,y] = test_rec_nums_ts[1,k,c,y-1] + ( beta2[1,k,c,year_idx]*B[1,k,c,y-1]) *tstep;
          test_rec_nums_ts[2,k,c,y] = test_rec_nums_ts[2,k,c,y-1] + ( beta2[2,k,c,year_idx]*B[2,k,c,y-1]) *tstep;
          test_rec_nums_ts[3,k,c,y] = test_rec_nums_ts[3,k,c,y-1] + ( beta2[3,k,c,year_idx]*B[3,k,c,y-1]) *tstep;
          test_rec_nums_ts[4,k,c,y] = test_rec_nums_ts[4,k,c,y-1] + ( beta2[4,k,c,year_idx]*B[4,k,c,y-1]) *tstep;
          test_rec_nums_ts[5,k,c,y] = test_rec_nums_ts[5,k,c,y-1] + ( beta2[5,k,c,year_idx]*B[5,k,c,y-1]) *tstep;
          test_rec_nums_ts[6,k,c,y] = test_rec_nums_ts[6,k,c,y-1] + ( beta2[6,k,c,year_idx]*B[6,k,c,y-1]) *tstep;
          test_rec_nums_ts[7,k,c,y] = test_rec_nums_ts[7,k,c,y-1] + ( beta2[7,k,c,year_idx]*B[7,k,c,y-1]) *tstep;
   
          test_nums_ts[1,k,c,y] = test_nums_ts[1,k,c,y-1] + ( beta[1,k,c,year_idx]*S[1,k,c,y-1]  +  beta2[1,k,c,year_idx]*B[1,k,c,y-1]) *tstep;
          test_nums_ts[2,k,c,y] = test_nums_ts[2,k,c,y-1] + ( beta[2,k,c,year_idx]*S[2,k,c,y-1]  +  beta2[2,k,c,year_idx]*B[2,k,c,y-1]) *tstep;
          test_nums_ts[3,k,c,y] = test_nums_ts[3,k,c,y-1] + ( beta[3,k,c,year_idx]*S[3,k,c,y-1]  +  beta2[3,k,c,year_idx]*B[3,k,c,y-1]) *tstep;
          test_nums_ts[4,k,c,y] = test_nums_ts[4,k,c,y-1] + ( beta[4,k,c,year_idx]*S[4,k,c,y-1]  +  beta2[4,k,c,year_idx]*B[4,k,c,y-1]) *tstep;
          test_nums_ts[5,k,c,y] = test_nums_ts[5,k,c,y-1] + ( beta[5,k,c,year_idx]*S[5,k,c,y-1]  +  beta2[5,k,c,year_idx]*B[5,k,c,y-1]) *tstep;
          test_nums_ts[6,k,c,y] = test_nums_ts[6,k,c,y-1] + ( beta[6,k,c,year_idx]*S[6,k,c,y-1]  +  beta2[6,k,c,year_idx]*B[6,k,c,y-1]) *tstep;
          test_nums_ts[7,k,c,y] = test_nums_ts[7,k,c,y-1] + ( beta[7,k,c,year_idx]*S[7,k,c,y-1]  +  beta2[7,k,c,year_idx]*B[7,k,c,y-1]) *tstep;
      }
   }
 }



    // for (y in 1:timestep) {
    //  // print("S[",i,",3,",r_,",",y,"] = ", S[i,3,r_,y]);
    //   print("S[7,3,2,52] = ", S[7,3,2,52]);
    // }



  for (i in 1:ag){ //// first year
    for (k in 1:s){
    test_nums[i,k,1,1]     =test_nums_ts[i,k,1,2];
    test_nums[i,k,2,1]     =test_nums_ts[i,k,2,2];
    test_rec_nums[i,k,1,1] =test_rec_nums_ts[i,k,1,2];
    test_rec_nums[i,k,2,1] =test_rec_nums_ts[i,k,2,2];
      }
    }
  
  
     for (y0 in 2:timespan){
         for (i in 1:ag){
            for (k in 1:s){

    test_nums[i,k,1,y0]     = (test_nums_ts[i,k,1,tlength[y0]]-test_nums_ts[i,k,1,tlength[y0]-2]);
    test_nums[i,k,2,y0]     = (test_nums_ts[i,k,2,tlength[y0]]-test_nums_ts[i,k,2,tlength[y0]-2]);
    test_rec_nums[i,k,1,y0] = (test_rec_nums_ts[i,k,1,tlength[y0]]-test_rec_nums_ts[i,k,1,tlength[y0]-2]);
    test_rec_nums[i,k,2,y0] = (test_rec_nums_ts[i,k,2,tlength[y0]]-test_rec_nums_ts[i,k,2,tlength[y0]-2]);
          }
        }
   // test[y0-1] = test_ts[tlength[y0]]-test_ts[tlength[y0-1]];    
      }

  testvol_est[1] = sum(test_nums[,1,1,20])+sum(test_nums[,1,2,20])+sum(test_nums[,2,1,20])+sum(test_nums[,2,2,20])+sum(test_nums[,3,1,20])+sum(test_nums[,3,2,20]);
  testvol_est[2] = sum(test_nums[,1,1,21])+sum(test_nums[,1,2,21])+sum(test_nums[,2,1,21])+sum(test_nums[,2,2,21])+sum(test_nums[,3,1,21])+sum(test_nums[,3,2,21]);
  testvol_est[3] = sum(test_nums[,1,1,22])+sum(test_nums[,1,2,22])+sum(test_nums[,2,1,22])+sum(test_nums[,2,2,22])+sum(test_nums[,3,1,22])+sum(test_nums[,3,2,22]);

  //print(testvol_est);

   for (y0 in 1:timespan){
     for (i in 1:ag){    
          for (k in 1:s){
    pr_tested[i,k,y0] =   (B[i,k,1,tlength[y0]]+B[i,k,2,tlength[y0]])/
                          (B[i,k,1,tlength[y0]]+S[i,k,1,tlength[y0]]+
                           B[i,k,2,tlength[y0]]+S[i,k,2,tlength[y0]]);
                            
    pr_tested_r1[i,k,y0] = (B[i,k,1,tlength[y0]])/ (B[i,k,1,tlength[y0]]+S[i,k,1,tlength[y0]] );     
    
    pr_tested_r2[i,k,y0] = (B[i,k,2,tlength[y0]])/ (B[i,k,2,tlength[y0]]+S[i,k,2,tlength[y0]]);

       // [ag,s,r,timestep]
       ////////////////////////////////
    pr_rec_tested[i,k,y0] = ((test_nums[i,k,1,y0]+test_nums[i,k,2,y0]))/ (B[i,k,1,tlength[y0]]+B[i,k,2,tlength[y0]]+S[i,k,1,tlength[y0]]+S[i,k,2,tlength[y0]]);
    pr_rec_tested_r1[i,k,y0] = (test_nums[i,k,1,y0])/ (B[i,k,1,tlength[y0]]+S[i,k,1,tlength[y0]]);
    pr_rec_tested_r2[i,k,y0] = (test_nums[i,k,2,y0])/ (B[i,k,2,tlength[y0]]+S[i,k,2,tlength[y0]]);
       }
     }
   }
   
// for (i in 1:ag) {
//   for (y0 in 1:timespan) {
//     print("pr_tested_r2[",i,",3,",y0,"] = ", pr_tested_r2[i,3,y0]);
//   }
// }

  // end of transformed parameters
}

model {
  
  int id_temp;
  int id_temp2;
  int id_temp3;
  int id_temp4;

  bp0_rr[1] ~ beta(14,6);
  bp0_rr[2] ~ beta(14,6);
  bp0_rr[3] ~ beta(14,6);

  bp0[1,1] ~ beta(400, 600);
  bp0[2,1] ~ beta(400, 600);
  bp0[3,1] ~ beta(400, 600);
  bp0[4,1] ~ beta(200, 600);
  bp0[5,1] ~ beta(80, 600);
  bp0[6,1] ~ beta(60, 600);
  bp0[7,1] ~ beta(60, 600);
  
  bp0[1,2] ~ beta(190, 570);
  bp0[2,2] ~ beta(200, 240);
  bp0[3,2] ~ beta(200, 240);
  bp0[4,2] ~ beta(190, 570);
  bp0[5,2] ~ beta(80, 500);
  bp0[6,2] ~ beta(60, 500);
  bp0[7,2] ~ beta(60, 500);
  
  bp0[1,3] ~ beta(300, 150);
  bp0[2,3] ~ beta(300, 150);
  bp0[3,3] ~ beta(300, 150);
  bp0[4,3] ~ beta(300, 150);
  bp0[5,3] ~ beta(300, 150);
  bp0[6,3] ~ beta(200, 150);
  bp0[7,3] ~ beta(200, 150);

 for (i in 1:4){ 
   for (k in 1:2){ 
          beta_scr[i,k] ~ beta(2, 10);
      }
          beta_scr[i,3] ~ beta(2, 5);
    }

  for (i in 5:7){ 
   for (k in 1:2){ 
          beta_scr[i,k] ~ beta(1, 10);
      }
    }
  beta_scr[5,3] ~ beta(2,5);  
  beta_scr[6,3] ~ beta(1,5);  
  beta_scr[7,3] ~ beta(1,5);  

  beta_rr1_w ~ beta(1, 10);
  beta_rr2_w ~ beta(1, 10);
  beta_rr3_w ~ beta(1, 10);
  beta_rr1_msw ~ beta(1, 10);
  beta_rr2_msw ~ beta(1, 10);
  beta_rr3_msw ~ beta(1, 10);
  beta_rr1_msm ~ beta(1, 10);
  beta_rr2_msm ~ beta(1, 10);
  beta_rr3_msm ~ beta(1, 10);
  
  // rr_beta2_w ~ gamma(10, 10);
  // rr_beta2_msw ~ gamma(10, 10);
  // rr_beta2_msm ~ gamma(10, 10);

  rr_lr_w ~ beta(1, 2);
  rr_lr_msw ~ beta(1, 2);
  rr_lr_msm ~ beta(1, 1);

  mu_test ~normal(d_testvol[1]*0.05, d_testvol[1]*0.01);
  coverage ~beta(beta_testcov[1], beta_testcov[2]);

// testing patterns
// msw women
 for (i in 1:ag){
   id_temp = 1;
   for (y in dv){ // 2008-2015
    d_tested_f[i,id_temp] ~ binomial(d_denom2, pr_tested[i,1,y]);
    d_tested_m[i,id_temp] ~ binomial(d_denom2, pr_tested[i,2,y]);
    id_temp = id_temp+1;
   }
 }

for (i in 1:5){
    d_tested_f_r1[i] ~ binomial(d_denom2, pr_tested_r1[i,1,15]);
    d_tested_m_r1[i] ~ binomial(d_denom2, pr_tested_r1[i,2,15]);

    d_tested_f_r2[i] ~ binomial(d_denom2, pr_tested_r2[i,1,15]);
    d_tested_m_r2[i] ~ binomial(d_denom2, pr_tested_r2[i,2,15]);
}
// // msm
for (i in 1:5){
    id_temp2 = 1;
 for (y in dmsmv){
   d_tested_msm[i,id_temp2] ~ binomial(d_denom3, pr_tested_r2[i,3,y]);
   d_tested_recent_msm[i,id_temp2] ~ binomial(d_denom3, pr_rec_tested_r2[i,3,y]);
   id_temp2 = id_temp2+1;
 }
}
// 
// // NSFG data
for (i in 1:3){
   id_temp4 = 1;
  for (y in dm){
   d_tested_recent_m2[i,id_temp4] ~ binomial(d_denom2, pr_rec_tested[i,2,y]);
   d_tested_recent_f2[i,id_temp4] ~ binomial(d_denom2, pr_rec_tested[i,1,y]);
   // d_tested_recent_msm2[i,id_temp2] ~ binomial(d_denom2, pr_rec_tested[i,3,y]);

    //  d_tested_msm2[i,id_temp2] ~ binomial(d_denom3, pr_tested[i,3,y]);
    d_tested_m2[i,id_temp4] ~ binomial(d_denom, pr_tested[i,2,y]);
    d_tested_f2[i,id_temp4] ~ binomial(d_denom, pr_tested[i,1,y]);

     id_temp4 = id_temp4+1;
   }
}

// testing volume
   d_testvol[1] ~ normal(testvol_est[1]*coverage, mu_test);
   d_testvol[2] ~ normal(testvol_est[2]*coverage, mu_test);
   d_testvol[3] ~ normal(testvol_est[3]*coverage, mu_test);

}

generated quantities {
 // real time_to_retest[ag,d,s,timespan];
  real Ntot[ag,s,timespan]; 
  real  Ntot_risk[ag,s,r,timespan];
  real  Nrisk_prop[ag,s,r,timespan];

  
  for (y0 in 2:timespan){
    for (i in 1:ag){
//  time_to_retest[i,y] = 1 / beta2[i,1,k]; // this is a weird place to have this in absence of time-varying change
  for (k in 1:s){
 // rec_tested[i,k,y0-1] =  (R[i,1,k,1,tlength[y0]]+R[i,1,k,2,tlength[y0]])/(R[i,1,k,1,tlength[y0]]+B[i,1,k,1,tlength[y0]]+S[i,1,k,1,tlength[y0]]+R[i,1,k,2,tlength[y0]]+B[i,1,k,2,tlength[y0]]+S[i,1,k,2,tlength[y0]]) ; ;
  Ntot[i,k,y0-1] =  S[i,k,1,tlength[y0]]+B[i,k,1,tlength[y0]]+
                    S[i,k,2,tlength[y0]]+B[i,k,2,tlength[y0]];
  for (c in 1:r){
      Ntot_risk[i,k,c,y0-1] =  S[i,k,c,tlength[y0]]+B[i,k,c,tlength[y0]];
      Nrisk_prop[i,k,c,y0-1] = Ntot_risk[i,k,c,y0-1] /Ntot[i,k,y0-1];
      }
    }
   }
  }

  //print(col(to_matrix(y), 2)+ col(to_matrix(y), 3))
  
}
