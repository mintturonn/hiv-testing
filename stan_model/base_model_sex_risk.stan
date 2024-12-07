//

  
data {
  int<lower=1> n_years;
  int timespan;
  real tstep;
 // int tstep_inv;
  int timestep;
  int t1[timespan+1];
  int ag;
  int d;
  int s;
  int r;
  vector[timestep] yrs;
  int dv[9];
  int dmsmv[5];
  int dm[4];
  int d_denom;
  // int drec[5];
  // int d_rec_tested[2,5];
  int d_testvol[3];
  real beta_testcov[2];
  int d_tested_f[ag,9];
  int d_tested_m[ag,9];
  int d_tested_msm[ag-2,5];
  
  int d_tested_f2[3,4];
  int d_tested_m2[3,4];
  //int d_tested_msm2[3,4];
  
  int d_tested_recent_f2[3,4];
  int d_tested_recent_m2[3,4];
  int d_tested_recent_msm[ag-2,5];
  
  int d_tested_f_r1[ag-2];
  int d_tested_f_r2[ag-2];
  int d_tested_m_r1[ag-2];
  int d_tested_m_r2[ag-2];
 // int d_tested_msm[ag]; 
 // int tested_risk[4, 8];
 // int tested_recent_f[ag,8];
 // int tested_recent_m[ag,8];
 // int tested_recent_msm[ag,8];
  
 int d_hiv_diag[5,timespan-11,s];
  int d_hiv_known[5,timespan-11,s];
  // int d_hivdeaths[5,timespan-11,s];
  // 
  // int d_popsize_f[ag,timespan];
  // int d_popsize_m[ag,timespan];
  
  // int d_deaths_f[ag, timespan-4];
  // int d_deaths_m[ag, timespan-4];
 
  real n0[ag, s];
  real aging[ag-1];
  real mort[4,s];
  real popgr;
  real risk_pr[ag,s];
  real risk_ageout[ag-1,s];
  
  vector[8] beta_prg_alpha;
  vector[8] beta_prg_beta;
  
  vector[5] beta_prep_w_alpha;
  vector[5] beta_prep_w_beta;
  vector[5] beta_prep_msw_alpha;
  vector[5] beta_prep_msw_beta;
  vector[5] beta_prep_msm_alpha;
  vector[5] beta_prep_msm_beta;
  
  vector[5] beta_prep_w_rr_alpha;
  vector[5] beta_prep_w_rr_beta;
  vector[5] beta_prep_msw_rr_alpha;
  vector[5] beta_prep_msw_rr_beta;
  vector[5] beta_prep_msm_rr_alpha;
  vector[5] beta_prep_msm_rr_beta;

  vector[2] beta_pwid_shape;  
  vector[2] test_pr_pwid;
  vector[2] test_pr_prg;
  vector[2] test_pr_prep;
  vector[2] hiv_test_posit_wm;
  vector[2] hiv_test_posit_msm;  

}

// 
transformed data {
  //real x_r[0];
  //int x_i[1] = { N };
  //int timestep = timespan*tstep_inv;
  
}

parameters {

  real<lower=0, upper=1> beta_pwid;  
  real<lower=0, upper=1> beta_prep_w[ag-2,2];
  real<lower=0, upper=1> beta_prep_msw[ag-2,2];
  real<lower=0, upper=1> beta_prep_msm[ag-2,2];
  real<lower=0, upper=1> beta_prg_pr[ag-3,2];
  real<lower=0, upper = 1> beta_scr[ag,s,r]; //by age and sex and risk
  real<lower=0, upper = 1> beta2_scr[ag,d,s,r]; // by age and sex and hiv status and risk
  real<lower=0> tst_prg;
  real<lower=0> tst_pwid;
  real<lower=0> tst_prep;
  real<lower=0> mu_test;
  real<lower=0, upper = 1> coverage;
  real<lower=0> mu_kh[s,5];
  real<lower=0> mu_hd[s,5];
  matrix<lower=0, upper = 0.3>[ag,s] hiv_init;
  matrix<lower=0, upper = 0.5>[ag,s] bp0;
  // real<lower=0, upper = 1> tp[s];
  // real<lower=0, upper = 1> tpp;
  real<lower=0, upper = 1> p0[s];
  real<lower=0, upper = 1> p1[s];
  
}

transformed parameters{
  
  real<lower=0, upper = 1> beta[ag,s,r,timestep]; //by age and sex and risk and year (timestep)
  real<lower=0, upper = 1> beta2[ag,d,s,r,timestep]; // by age and sex and hiv status and risk  and year (timestep)
  real<lower=0, upper = 1> p[s, timestep];

  real<lower=0, upper = 1> beta_pwid_hr[ag,s]; //by age and year [only HIV-]
  real<lower=0, upper = 1> beta_prg[ag,timestep]; //by age and year [only HIV-]
  real<lower=0, upper = 1> beta_prep[ag,s,timestep]; // by age and sex and year [only HIV-]
  real<lower=0> hiv_diag_ts[5,s,r,timestep];
  real<lower=0> known_hiv_prev_ts[5,s,r,timestep];
  real<lower=0> test_nums_ts[ag,s,r,timestep]; 
  real<lower=0> test_rec_nums_ts[ag,s,r,timestep]; 
  real<lower=0> test_numspos_ts[ag,s,r,timestep]; 
  real<lower=0> test_ts[timestep]; 
  // real<lower=0> test_reas_f_ts[ag,4,r,timestep];  // women 4 separate pathways for testing cumulative (combining screening into one)
  // real<lower=0> test_reas_mw_ts[ag,3,r,timestep]; // msw 3 separate pathways for testing cumul 
  // real<lower=0> test_reas_mm_ts[ag,3,r,timestep]; // msm 3 separate pathways for testing cumul 
  real<lower=0> hiv_diag[5,s,timespan];
  real<lower=0> known_hiv_prev[5,s,timespan];
  real<lower=0> Ntot5cat[5,2,timespan];
  real<lower=0> test_nums[ag,s,r,timespan];
  real<lower=0> test_rec_nums[ag,s,r,timespan];
  real<lower=0> test_numspos[ag,s,r,timespan];
  real<lower=0> test_reas_f[ag,5,r];  // last year, women 5 separate pathways for testing (combining screening into one)
  real<lower=0> test_reas_mw[ag,4,r]; // last year, msw 4 separate pathways for testing
  real<lower=0> test_reas_mm[ag,4,r]; // last year, msm 4 separate pathways for testing
  real<lower=0> test[timespan]; 
  real pr_tested[ag,s,timespan];
  real pr_tested_r1[ag,s,timespan];
  real pr_tested_r2[ag,s,timespan];
  real pr_rec_tested[ag,s,timespan]; 
  real pr_rec_tested_r1[ag,s,timespan]; 
  real pr_rec_tested_r2[ag,s,timespan]; 
  real testvol_est[3];

  real<lower=0> s0[ag,s];
  real<lower=0> b0[ag,s];
  real<lower=0> S[ag,d,s,r,timestep]; 
  real<lower=0> B[ag,d,s,r,timestep]; 
  
 real ageout_temp[s,timestep];

// calculate proportion of HR who are PWID 
  for (k in 1:s){
   for (i in 1:5){
     beta_pwid_hr[i,k] =  beta_pwid/risk_pr[i,k];
    }
   for (i in 6:ag){
     beta_pwid_hr[i,k] =  0;
    }
 }
 
 // real<lower=0> test;
 // for (n in 1:N) {
 //   x[n] = x0 + (x1 - x0) / (y1 - y0) * (t[n] - y0); }
for (y in 1:timestep){
  for (i in 1:4){
     beta_prg[i,y] = fmax(0, beta_prg_pr[i,1] + (beta_prg_pr[i,2]-beta_prg_pr[i,1] ) / (2022 - 1999) * (yrs[y] - 1999));
     beta_prep[i,1,y] = 0.0; // populating with zeros
     beta_prep[i,2,y] = 0.0;
     beta_prep[i,3,y] = 0.0;
  } 
  for (i in 5:ag){
    beta_prg[i,y]=0.0;
    beta_prep[i,1,y] = 0.0;
    beta_prep[i,2,y] = 0.0;
    beta_prep[i,3,y] = 0.0;
  }
 }
 
 // starts from 2014 up to 2017  
 for (y in 16:19){
   for (i in 1:5){
   beta_prep[i,1,y] = fmax(0, 0.0 + (beta_prep_w[i,1]*beta_prep_w[i,2]    -0.0 ) / (2017-2014)*(yrs[y]-2014));
   beta_prep[i,2,y] = fmax(0, 0.0 + (beta_prep_msw[i,1]*beta_prep_msw[i,2]-0.0 ) / (2017-2014)*(yrs[y]-2014));
   beta_prep[i,3,y] = fmax(0, 0.0 + (beta_prep_msm[i,1]*beta_prep_msm[i,2]-0.0 ) / (2017-2014)*(yrs[y]-2014));
   }
 } 
 // starts from 2017 up to 2023  
 for (y in 19:timestep){
   for (i in 1:5){
   beta_prep[i,1,y] = fmax(0, beta_prep_w[i,1]*beta_prep_w[i,2]     + (beta_prep_w[i,2]  -beta_prep_w[i,1]  *beta_prep_w[i,2])  /(2022-2017)*(yrs[y]-2017));
   beta_prep[i,2,y] = fmax(0, beta_prep_msw[i,1]*beta_prep_msw[i,2] + (beta_prep_msw[i,2]-beta_prep_msw[i,1]*beta_prep_msw[i,2])/(2022-2017)*(yrs[y]-2017));
   beta_prep[i,3,y] = fmax(0, beta_prep_msm[i,1]*beta_prep_msm[i,2] + (beta_prep_msm[i,2]-beta_prep_msm[i,1]*beta_prep_msm[i,2])/(2022-2017)*(yrs[y]-2017));
   }
 } 
 
for (y in 1:8){
   p[1,y] = p0[1];
   p[2,y] = p0[2];
   p[3,y] = p0[3];
 } 
 
for (y in 9:timestep){
  p[1,y] = p0[1]  + (p1[1] -p0[1]) / (1 + exp(-(yrs[y] - 2015)));
  p[2,y] = p0[2]  + (p1[2] -p0[2]) / (1 + exp(-(yrs[y] - 2015)));
  p[3,y] = p0[3]  + (p1[3] -p0[3]) / (1 + exp(-(yrs[y] - 2015)));
   // p[1,y] = fmax(0, p0[1] +(p1[1]-p0[1])/(2024-2007)*(yrs[y]-2024));
   // p[2,y] = fmax(0, p0[2] +(p1[2]-p0[2])/(2024-2007)*(yrs[y]-2024));
   // p[3,y] = fmax(0, p0[3] +(p1[3]-p0[3])/(2024-2007)*(yrs[y]-2024));
 }  


  for (y in 1:timestep){
    for (i in 1:ag){
      // hiv -
      beta[i,1,1,y] = beta_scr[i,1,1] + tst_prg*beta_prg[i,y]; // lr women no HIV for beta
      beta[i,1,2,y] = beta_scr[i,1,2] + tst_prg*beta_prg[i,y] + tst_pwid *beta_pwid_hr[i,1]; // hr women no HIV for beta  
      beta[i,2,1,y] = beta_scr[i,2,1] ;                       // lr MSW no HIV for beta
      beta[i,2,2,y] = beta_scr[i,2,2] + tst_pwid *beta_pwid_hr[i,2]; // hr MSW no HIV for beta
      beta[i,3,1,y] = beta_scr[i,3,1] ;                      // lr MSM no HIV for beta
      beta[i,3,2,y] = beta_scr[i,3,2] + tst_pwid *beta_pwid_hr[i,3]; // hr MSM no HIV for beta
      
      // hiv -
      beta2[i,1,1,1,y] = beta2_scr[i,1,1,1] + tst_prg*beta_prg[i,y];                                                            // lr women 
      beta2[i,1,1,2,y] = beta2_scr[i,1,1,2] + tst_prg*beta_prg[i,y] + tst_prep*beta_prep[i,1,y] + tst_pwid *beta_pwid_hr[i,1] ; // hr women 
      beta2[i,1,2,1,y] = beta2_scr[i,1,2,1] ;                                                                                   // lr MSW 
      beta2[i,1,2,2,y] = beta2_scr[i,1,2,2] + tst_prep*beta_prep[i,2,y] + tst_pwid *beta_pwid_hr[i,2];                          // hr MSW 
      beta2[i,1,3,1,y] = beta2_scr[i,1,3,1] ;                                                                                   // lr MSM 
      beta2[i,1,3,2,y] = beta2_scr[i,1,3,2] + tst_prep*beta_prep[i,3,y] + tst_pwid *beta_pwid_hr[i,3];                          // hr MSM 
      
      // hiv +
      beta2[i,2,1,1,y] = beta2_scr[i,2,1,1];                     // lr women 
      beta2[i,2,1,2,y] = beta2_scr[i,2,1,2];                     // hr women 
      beta2[i,2,2,1,y] = beta2_scr[i,2,2,1];                     // lr MSW 
      beta2[i,2,2,2,y] = beta2_scr[i,2,2,2];                     // hr MSW 
      beta2[i,2,3,1,y] = beta2_scr[i,2,3,1];                     // lr MSM 
      beta2[i,2,3,2,y] = beta2_scr[i,2,3,2];                      // hr MSM 
      
    }
  }
  
  // print(beta2_scr);
  // print(tst_prg);
  // print(beta_prg);
  // print(tst_prep);
  // print(beta_prep);
  
 for (k in 1:s){  
    b0[1,k] = bp0[1,k]*n0[1,k];
    b0[2,k] = bp0[2,k]*n0[2,k];
    b0[3,k] = bp0[3,k]*n0[3,k];
    b0[4,k] = bp0[4,k]*n0[4,k];
    b0[5,k] = bp0[5,k]*n0[5,k];
    b0[6,k] = bp0[6,k]*n0[6,k];
    b0[7,k] = bp0[7,k]*n0[7,k];

    s0[1,k] = n0[1,k] - b0[1,k] ;
    s0[2,k] = n0[2,k] - b0[2,k] ;
    s0[3,k] = n0[3,k] - b0[3,k] ;
    s0[4,k] = n0[4,k] - b0[4,k] ;
    s0[5,k] = n0[5,k] - b0[5,k] ;
    s0[6,k] = n0[6,k] - b0[6,k] ;
    s0[7,k] = n0[7,k] - b0[7,k] ;
  }
  
 for (i in 1:ag){
    for (j in 1:d){
      for (k in 1:s){
         for (c in 1:r){
           for (y in 1:timestep){
       S[i,j,k,c,y] = 0.0;
       B[i,j,k,c,y] = 0.0;
          }
         }
        }
       }
      }
 
 for (i in 1:ag){ 
     for (k in 1:s){  
    
  // lower risk     
   B[i,2,k,1,1] = (1-risk_pr[i,k])*hiv_init[i,k]*b0[i,k];
   
   S[i,1,k,1,1] = (1-risk_pr[i,k])*s0[i,k];
   B[i,1,k,1,1] = (1-risk_pr[i,k])*(1-hiv_init[i,k])*b0[i,k];
   
  // higher risk 
   B[i,2,k,2,1] = risk_pr[i,k]*hiv_init[i,k]*b0[i,k];
   
   S[i,1,k,2,1] = risk_pr[i,k]*s0[i,k];
   B[i,1,k,2,1] = risk_pr[i,k]*(1-hiv_init[i,k])*b0[i,k];
   
   test_nums_ts[i,k,1,1] = 0.0;
   test_nums_ts[i,k,2,1] = 0.0;
   test_numspos_ts[i,k,1,1] = 0.0;
   test_numspos_ts[i,k,2,1] = 0.0;
   test_rec_nums_ts[i,k,1,1] = 0.0;
   test_rec_nums_ts[i,k,2,1] = 0.0;
  
   // test_reas_f_ts[i,4,1,1] = 0.0; // calculated for both risk groups
   // test_reas_f_ts[i,4,2,1] = 0.0; 
   // test_reas_mw_ts[i,3,1,1] = 0.0; 
   // test_reas_mw_ts[i,3,2,1] = 0.0; 
   // test_reas_mm_ts[i,3,1,1] = 0.0;
   // test_reas_mm_ts[i,3,2,1] = 0.0;
   pr_tested[i,k,1] = 0.0;
   pr_tested_r1[i,k,1] = 0.0;
   pr_tested_r2[i,k,1] = 0.0;
   pr_rec_tested[i,k,1] =0.0;
   pr_rec_tested_r1[i,k,1] =0.0;
   pr_rec_tested_r2[i,k,1] =0.0;
  }
 }
  
 for (ii in 1:5){
    for (k in 1:s){
       for (c in 1:r){
   hiv_diag_ts[ii,k,c,1]  = 0.0;
   known_hiv_prev_ts[ii,k,c,1]  = 0.0;
    }
   }
   Ntot5cat[ii,1,1]  = 0.0;
   Ntot5cat[ii,2,1]  = 0.0;
  }

    ageout_temp[1,1] = 0;
    ageout_temp[2,1] = 0;
    ageout_temp[3,1] = 0;
    test_ts[1] = 0;

 for (k in 1:s){
    for (y in 2:timestep){

          ageout_temp[k,y-1] =   popgr* (mort[1,k]*(S[4,1,k,1,y-1] + B[4,1,k,1,y-1] + B[4,2,k,1,y-1]) +
                                         mort[2,k]*(S[5,1,k,1,y-1] + B[5,1,k,1,y-1] + B[5,2,k,1,y-1]) +
                                         mort[3,k]*(S[6,1,k,1,y-1] + B[6,1,k,1,y-1] + B[6,2,k,1,y-1]) +
                                         mort[4,k]*(S[7,1,k,1,y-1] + B[7,1,k,1,y-1] + B[7,2,k,1,y-1]) +
                                         mort[1,k]*(S[4,1,k,2,y-1] + B[4,1,k,2,y-1] + B[4,2,k,2,y-1]) +
                                         mort[2,k]*(S[5,1,k,2,y-1] + B[5,1,k,2,y-1] + B[5,2,k,2,y-1]) +
                                         mort[3,k]*(S[6,1,k,2,y-1] + B[6,1,k,2,y-1] + B[6,2,k,2,y-1]) +
                                         mort[4,k]*(S[7,1,k,2,y-1] + B[7,1,k,2,y-1] + B[7,2,k,2,y-1])  ) ;

// HIV negative population    
// lower risk
          S[1,1,k,1,y] = S[1,1,k,1,y-1] + ( - beta[1,k,1,y-1]*S[1,1,k,1,y-1]                                                                      - S[1,1,k,1,y-1]*aging[1] + (1-risk_pr[1,k])*ageout_temp[k,y-1] )* tstep;
          B[1,1,k,1,y] = B[1,1,k,1,y-1] + ( (1-p[k,y-1])*beta[1,k,1,y-1]*S[1,1,k,1,y-1] -p[k,y-1]*beta2[1,1,k,1,y-1]*B[1,1,k,1,y-1] -B[1,1,k,1,y-1]*aging[1] )* tstep  ;
          
          S[2,1,k,1,y] = S[2,1,k,1,y-1] + ( - beta[2,k,1,y-1]*S[2,1,k,1,y-1]                                                                      + S[1,1,k,1,y-1]*aging[1] +risk_ageout[1,k]*S[1,1,k,2,y-1]*aging[1]-S[2,1,k,1,y-1]*aging[2])* tstep ;
          B[2,1,k,1,y] = B[2,1,k,1,y-1] + ( (1-p[k,y-1])*beta[2,k,1,y-1]*S[2,1,k,1,y-1] -p[k,y-1]*beta2[2,1,k,1,y-1]*B[2,1,k,1,y-1] +B[1,1,k,1,y-1]*aging[1] +risk_ageout[1,k]*B[1,1,k,2,y-1]*aging[1]-B[2,1,k,1,y-1]*aging[2] )* tstep ;
         
          S[3,1,k,1,y] = S[3,1,k,1,y-1] + ( - beta[3,k,1,y-1]*S[3,1,k,1,y-1]                                                                      + S[2,1,k,1,y-1]*aging[2]+risk_ageout[2,k]*S[2,1,k,2,y-1]*aging[2]-S[3,1,k,1,y-1]*aging[3] )* tstep ;
          B[3,1,k,1,y] = B[3,1,k,1,y-1] + ( (1-p[k,y-1])*beta[3,k,1,y-1]*S[3,1,k,1,y-1]  -p[k,y-1]*beta2[3,1,k,1,y-1]*B[3,1,k,1,y-1] + B[2,1,k,1,y-1]*aging[2] +risk_ageout[2,k]*B[2,1,k,2,y-1]*aging[2]-B[3,1,k,1,y-1]*aging[3] )* tstep ;
          
          S[4,1,k,1,y] = S[4,1,k,1,y-1] + ( - beta[4,k,1,y-1]*S[4,1,k,1,y-1]                                                                      +S[3,1,k,1,y-1]*aging[3] +risk_ageout[3,k]*S[3,1,k,2,y-1]*aging[3]-S[4,1,k,1,y-1]*aging[4] - S[4,1,k,1,y-1]*mort[1,k])* tstep ;
          B[4,1,k,1,y] = B[4,1,k,1,y-1] + ( (1-p[k,y-1])*beta[4,k,1,y-1]*S[4,1,k,1,y-1] -p[k,y-1]*beta2[4,1,k,1,y-1]*B[4,1,k,1,y-1] +B[3,1,k,1,y-1]*aging[3] +risk_ageout[3,k]*B[3,1,k,2,y-1]*aging[3]-B[4,1,k,1,y-1]*aging[4] - B[4,1,k,1,y-1]*mort[1,k])* tstep ;
          
          S[5,1,k,1,y] = S[5,1,k,1,y-1] + ( - beta[5,k,1,y-1]*S[5,1,k,1,y-1]                                                                      +S[4,1,k,1,y-1]*aging[4] +risk_ageout[4,k]*S[4,1,k,2,y-1]*aging[4]-S[5,1,k,1,y-1]*aging[5] - S[5,1,k,1,y-1]*mort[2,k])* tstep ;
          B[5,1,k,1,y] = B[5,1,k,1,y-1] + ( (1-p[k,y-1])*beta[5,k,1,y-1]*S[5,1,k,1,y-1] -p[k,y-1]*beta2[5,1,k,1,y-1]*B[5,1,k,1,y-1] +B[4,1,k,1,y-1]*aging[4] +risk_ageout[4,k]*B[4,1,k,2,y-1]*aging[4]-B[5,1,k,1,y-1]*aging[5] - B[5,1,k,1,y-1]*mort[2,k])* tstep ;
     
          S[6,1,k,1,y] = S[6,1,k,1,y-1] + ( - beta[6,k,1,y-1]*S[6,1,k,1,y-1]                                                                      +S[5,1,k,1,y-1]*aging[5] +risk_ageout[5,k]*S[5,1,k,2,y-1]*aging[5]-S[6,1,k,1,y-1]*aging[6] - S[6,1,k,1,y-1]*mort[3,k])* tstep ;
          B[6,1,k,1,y] = B[6,1,k,1,y-1] + ( (1-p[k,y-1])*beta[6,k,1,y-1]*S[6,1,k,1,y-1] -p[k,y-1]*beta2[6,1,k,1,y-1]*B[6,1,k,1,y-1] +B[5,1,k,1,y-1]*aging[5] +risk_ageout[5,k]*B[5,1,k,2,y-1]*aging[5]-B[6,1,k,1,y-1]*aging[6] - B[6,1,k,1,y-1]*mort[3,k])* tstep ;
 
          S[7,1,k,1,y] = S[7,1,k,1,y-1] + ( - beta[7,k,1,y-1]*S[7,1,k,1,y-1]                                                                      +S[6,1,k,1,y-1]*aging[6] +risk_ageout[6,k]*S[6,1,k,2,y-1]*aging[6]- S[7,1,k,1,y-1]*mort[4,k])* tstep ;
          B[7,1,k,1,y] = B[7,1,k,1,y-1] + ( (1-p[k,y-1])*beta[7,k,1,y-1]*S[7,1,k,1,y-1] -p[k,y-1]*beta2[7,1,k,1,y-1]*B[7,1,k,1,y-1] +B[6,1,k,1,y-1]*aging[6] +risk_ageout[6,k]*B[6,1,k,2,y-1]*aging[6]- B[7,1,k,1,y-1]*mort[4,k])* tstep ;
 
// higher risk
          S[1,1,k,2,y] = S[1,1,k,2,y-1] + ( - beta[1,k,2,y-1]*S[1,1,k,2,y-1]                                                                      -S[1,1,k,2,y-1]*aging[1] + risk_pr[1,k]*ageout_temp[k,y-1] )* tstep;
          B[1,1,k,2,y] = B[1,1,k,2,y-1] + ( (1-p[k,y-1])*beta[1,k,2,y-1]*S[1,1,k,2,y-1] -p[k,y-1]*beta2[1,1,k,2,y-1]*B[1,1,k,2,y-1] -B[1,1,k,2,y-1]*aging[1] )* tstep  ;
          
          S[2,1,k,2,y] = S[2,1,k,2,y-1] + ( - beta[2,k,2,y-1]*S[2,1,k,2,y-1]                                                                      +(1-risk_ageout[1,k])*S[1,1,k,2,y-1]*aging[1] -S[2,1,k,2,y-1]*aging[2])* tstep ;
          B[2,1,k,2,y] = B[2,1,k,2,y-1] + ( (1-p[k,y-1])*beta[2,k,2,y-1]*S[2,1,k,2,y-1] -p[k,y-1]*beta2[2,1,k,2,y-1]*B[2,1,k,2,y-1]  +(1-risk_ageout[1,k])*B[1,1,k,2,y-1]*aging[1] -B[2,1,k,2,y-1]*aging[2] )* tstep ;
         
          S[3,1,k,2,y] = S[3,1,k,2,y-1] + ( - beta[3,k,2,y-1]*S[3,1,k,2,y-1]                                                                      +(1-risk_ageout[2,k])*S[2,1,k,2,y-1]*aging[2] -S[3,1,k,2,y-1]*aging[3] )* tstep ;
          B[3,1,k,2,y] = B[3,1,k,2,y-1] + ( (1-p[k,y-1])*beta[3,k,2,y-1]*S[3,1,k,2,y-1] -p[k,y-1]*beta2[3,1,k,2,y-1]*B[3,1,k,2,y-1]  +(1-risk_ageout[2,k])*B[2,1,k,2,y-1]*aging[2] -B[3,1,k,2,y-1]*aging[3] )* tstep ;
          
          S[4,1,k,2,y] = S[4,1,k,2,y-1] + ( - beta[4,k,2,y-1]*S[4,1,k,2,y-1]                                                                       +(1-risk_ageout[3,k])*S[3,1,k,2,y-1]*aging[3] -S[4,1,k,2,y-1]*aging[4] - S[4,1,k,2,y-1]*mort[1,k])* tstep ;
          B[4,1,k,2,y] = B[4,1,k,2,y-1] + ( (1-p[k,y-1])*beta[4,k,2,y-1]*S[4,1,k,2,y-1] -p[k,y-1]*beta2[4,1,k,2,y-1]*B[4,1,k,2,y-1]  +(1-risk_ageout[3,k])*B[3,1,k,2,y-1]*aging[3] -B[4,1,k,2,y-1]*aging[4] - B[4,1,k,2,y-1]*mort[1,k])* tstep ;
          
          S[5,1,k,2,y] = S[5,1,k,2,y-1] + ( - beta[5,k,2,y-1]*S[5,1,k,2,y-1]                                                                       +(1-risk_ageout[4,k])*S[4,1,k,2,y-1]*aging[4] -S[5,1,k,2,y-1]*aging[5] - S[5,1,k,2,y-1]*mort[2,k])* tstep ;
          B[5,1,k,2,y] = B[5,1,k,2,y-1] + ( (1-p[k,y-1])*beta[5,k,2,y-1]*S[5,1,k,2,y-1] -p[k,y-1]*beta2[5,1,k,2,y-1]*B[5,1,k,2,y-1]   +(1-risk_ageout[4,k])*B[4,1,k,2,y-1]*aging[4] -B[5,1,k,2,y-1]*aging[5] - B[5,1,k,2,y-1]*mort[2,k])* tstep ;
     
          S[6,1,k,2,y] = S[6,1,k,2,y-1] + ( - beta[6,k,2,y-1]*S[6,1,k,2,y-1]                                                                       +(1-risk_ageout[5,k])*S[5,1,k,2,y-1]*aging[5] -S[6,1,k,2,y-1]*aging[6] - S[6,1,k,2,y-1]*mort[3,k])* tstep ;
          B[6,1,k,2,y] = B[6,1,k,2,y-1] + (  (1-p[k,y-1])*beta[6,k,2,y-1]*S[6,1,k,2,y-1]  -p[k,y-1]*beta2[6,1,k,2,y-1]*B[6,1,k,2,y-1]  +(1-risk_ageout[5,k])*B[5,1,k,2,y-1]*aging[5] -B[6,1,k,2,y-1]*aging[6] - B[6,1,k,2,y-1]*mort[3,k])* tstep ;
 
          S[7,1,k,2,y] = S[7,1,k,2,y-1] + ( - beta[7,k,2,y-1]*S[7,1,k,2,y-1]                                                                       +(1-risk_ageout[6,k])*S[6,1,k,2,y-1]*aging[6] -S[7,1,k,2,y-1]*mort[4,k])* tstep ;
          B[7,1,k,2,y] = B[7,1,k,2,y-1] + (  (1-p[k,y-1])*beta[7,k,2,y-1]*S[7,1,k,2,y-1] -p[k,y-1]*beta2[7,1,k,2,y-1]*B[7,1,k,2,y-1]  +(1-risk_ageout[6,k])*B[6,1,k,2,y-1]*aging[6] -B[7,1,k,2,y-1]*mort[4,k])* tstep ;
          
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////            
// HIV diagnosed population    
// lower risk
          B[1,2,k,1,y] = B[1,2,k,1,y-1] + (p[k,y-1]*beta[1,k,1,y-1]*S[1,1,k,1,y-1] +p[k,y-1]*beta2[1,1,k,1,y-1]*B[1,1,k,1,y-1]-B[1,2,k,1,y-1]*aging[1] )* tstep  ;
          B[2,2,k,1,y] = B[2,2,k,1,y-1] + (p[k,y-1]*beta[2,k,1,y-1]*S[2,1,k,1,y-1] +p[k,y-1]*beta2[2,1,k,1,y-1]*B[2,1,k,1,y-1]+B[1,2,k,1,y-1]*aging[1] +risk_ageout[1,k]*B[1,2,k,2,y-1]*aging[1]-B[2,2,k,1,y-1]*aging[2] )* tstep ;
          B[3,2,k,1,y] = B[3,2,k,1,y-1] + (p[k,y-1]*beta[3,k,1,y-1]*S[3,1,k,1,y-1] +p[k,y-1]*beta2[3,1,k,1,y-1]*B[3,1,k,1,y-1] +B[2,2,k,1,y-1]*aging[2] +risk_ageout[2,k]*B[2,2,k,2,y-1]*aging[2]-B[3,2,k,1,y-1]*aging[3] )* tstep ;
          B[4,2,k,1,y] = B[4,2,k,1,y-1] + (p[k,y-1]*beta[4,k,1,y-1]*S[4,1,k,1,y-1] +p[k,y-1]*beta2[4,1,k,1,y-1]*B[4,1,k,1,y-1]+B[3,2,k,1,y-1]*aging[3] +risk_ageout[3,k]*B[3,2,k,2,y-1]*aging[3]-B[4,2,k,1,y-1]*aging[4] - B[4,2,k,1,y-1]*mort[1,k])* tstep ;
          B[5,2,k,1,y] = B[5,2,k,1,y-1] + (p[k,y-1]*beta[5,k,1,y-1]*S[5,1,k,1,y-1] +p[k,y-1]*beta2[5,1,k,1,y-1]*B[5,1,k,1,y-1]+B[4,2,k,1,y-1]*aging[4] +risk_ageout[4,k]*B[4,2,k,2,y-1]*aging[4]-B[5,2,k,1,y-1]*aging[5] - B[5,2,k,1,y-1]*mort[2,k])* tstep ;
          B[6,2,k,1,y] = B[6,2,k,1,y-1] + (p[k,y-1]*beta[6,k,1,y-1]*S[6,1,k,1,y-1] +p[k,y-1]*beta2[6,1,k,1,y-1]*B[6,1,k,1,y-1]+B[5,2,k,1,y-1]*aging[5] +risk_ageout[5,k]*B[5,2,k,2,y-1]*aging[5]-B[6,2,k,1,y-1]*aging[6] - B[6,2,k,1,y-1]*mort[3,k])* tstep ;
          B[7,2,k,1,y] = B[7,2,k,1,y-1] + (p[k,y-1]*beta[7,k,1,y-1]*S[7,1,k,1,y-1] +p[k,y-1]*beta2[7,1,k,1,y-1]*B[7,1,k,1,y-1]+B[6,2,k,1,y-1]*aging[6] +risk_ageout[6,k]*B[6,2,k,2,y-1]*aging[6]-B[7,2,k,1,y-1]*mort[4,k])* tstep ;

// higher risk
          B[1,2,k,2,y] = B[1,2,k,2,y-1] + (p[k,y-1]*beta[1,k,2,y-1]*S[1,1,k,2,y-1]+p[k,y-1]*beta2[1,1,k,2,y-1]*B[1,1,k,2,y-1] -B[1,2,k,2,y-1]*aging[1] )* tstep  ;
          B[2,2,k,2,y] = B[2,2,k,2,y-1] + (p[k,y-1]*beta[2,k,2,y-1]*S[2,1,k,2,y-1]+p[k,y-1]*beta2[2,1,k,2,y-1]*B[2,1,k,2,y-1] +(1-risk_ageout[1,k])*B[1,2,k,2,y-1]*aging[1] - B[2,2,k,2,y-1]*aging[2] )* tstep ;
          B[3,2,k,2,y] = B[3,2,k,2,y-1] + (p[k,y-1]*beta[3,k,2,y-1]*S[3,1,k,2,y-1]+p[k,y-1]*beta2[3,1,k,2,y-1]*B[3,1,k,2,y-1] +(1-risk_ageout[2,k])*B[2,2,k,2,y-1]*aging[2] - B[3,2,k,2,y-1]*aging[3] )* tstep ;
          B[4,2,k,2,y] = B[4,2,k,2,y-1] + (p[k,y-1]*beta[4,k,2,y-1]*S[4,1,k,2,y-1]+p[k,y-1]*beta2[4,1,k,2,y-1]*B[4,1,k,2,y-1] +(1-risk_ageout[3,k])*B[3,2,k,2,y-1]*aging[3] - B[4,2,k,2,y-1]*aging[4] - B[4,2,k,2,y-1]*mort[1,k])* tstep ;
          B[5,2,k,2,y] = B[5,2,k,2,y-1] + (p[k,y-1]*beta[5,k,2,y-1]*S[5,1,k,2,y-1]+p[k,y-1]*beta2[5,1,k,2,y-1]*B[5,1,k,2,y-1] +(1-risk_ageout[4,k])*B[4,2,k,2,y-1]*aging[4] - B[5,2,k,2,y-1]*aging[5] - B[5,2,k,2,y-1]*mort[2,k])* tstep ;
          B[6,2,k,2,y] = B[6,2,k,2,y-1] + (p[k,y-1]*beta[6,k,2,y-1]*S[6,1,k,2,y-1]+p[k,y-1]*beta2[6,1,k,2,y-1]*B[6,1,k,2,y-1]+(1-risk_ageout[5,k])*B[5,2,k,2,y-1]*aging[5] - B[6,2,k,2,y-1]*aging[6] - B[6,2,k,2,y-1]*mort[3,k])* tstep ;
          B[7,2,k,2,y] = B[7,2,k,2,y-1] + (p[k,y-1]*beta[7,k,2,y-1]*S[7,1,k,2,y-1]+p[k,y-1]*beta2[7,1,k,2,y-1]*B[7,1,k,2,y-1]+(1-risk_ageout[6,k])*B[6,2,k,2,y-1]*aging[6] - B[7,2,k,2,y-1]*mort[4,k])* tstep ;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////          
// summary estimates
 for (c in 1:r){
          test_rec_nums_ts[1,k,c,y] = test_rec_nums_ts[1,k,c,y-1] + beta2[1,1,k,c,y-1]*B[1,1,k,c,y-1]+ beta2[1,2,k,c,y-1]*B[1,2,k,c,y-1];
          test_rec_nums_ts[2,k,c,y] = test_rec_nums_ts[2,k,c,y-1] + beta2[2,1,k,c,y-1]*B[2,1,k,c,y-1]+ beta2[2,2,k,c,y-1]*B[2,2,k,c,y-1];
          test_rec_nums_ts[3,k,c,y] = test_rec_nums_ts[3,k,c,y-1] + beta2[3,1,k,c,y-1]*B[3,1,k,c,y-1]+ beta2[3,2,k,c,y-1]*B[3,2,k,c,y-1];
          test_rec_nums_ts[4,k,c,y] = test_rec_nums_ts[4,k,c,y-1] + beta2[4,1,k,c,y-1]*B[4,1,k,c,y-1]+ beta2[4,2,k,c,y-1]*B[4,2,k,c,y-1];
          test_rec_nums_ts[5,k,c,y] = test_rec_nums_ts[5,k,c,y-1] + beta2[5,1,k,c,y-1]*B[5,1,k,c,y-1]+ beta2[5,2,k,c,y-1]*B[5,2,k,c,y-1];
          test_rec_nums_ts[6,k,c,y] = test_rec_nums_ts[6,k,c,y-1] + beta2[6,1,k,c,y-1]*B[6,1,k,c,y-1]+ beta2[6,2,k,c,y-1]*B[6,2,k,c,y-1];
          test_rec_nums_ts[7,k,c,y] = test_rec_nums_ts[7,k,c,y-1] + beta2[7,1,k,c,y-1]*B[7,1,k,c,y-1]+ beta2[7,2,k,c,y-1]*B[7,2,k,c,y-1];
   
          test_nums_ts[1,k,c,y] = test_nums_ts[1,k,c,y-1] + beta[1,k,c,y-1]*S[1,1,k,c,y-1]  +  beta2[1,1,k,c,y-1]*B[1,1,k,c,y-1]+ beta2[1,2,k,c,y-1]*B[1,2,k,c,y-1];
          test_nums_ts[2,k,c,y] = test_nums_ts[2,k,c,y-1] + beta[2,k,c,y-1]*S[2,1,k,c,y-1]  +  beta2[2,1,k,c,y-1]*B[2,1,k,c,y-1]+ beta2[2,2,k,c,y-1]*B[2,2,k,c,y-1];
          test_nums_ts[3,k,c,y] = test_nums_ts[3,k,c,y-1] + beta[3,k,c,y-1]*S[3,1,k,c,y-1]  +  beta2[3,1,k,c,y-1]*B[3,1,k,c,y-1]+ beta2[3,2,k,c,y-1]*B[3,2,k,c,y-1];
          test_nums_ts[4,k,c,y] = test_nums_ts[4,k,c,y-1] + beta[4,k,c,y-1]*S[4,1,k,c,y-1]  +  beta2[4,1,k,c,y-1]*B[4,1,k,c,y-1]+ beta2[4,2,k,c,y-1]*B[4,2,k,c,y-1];
          test_nums_ts[5,k,c,y] = test_nums_ts[5,k,c,y-1] + beta[5,k,c,y-1]*S[5,1,k,c,y-1]  +  beta2[5,1,k,c,y-1]*B[5,1,k,c,y-1]+ beta2[5,2,k,c,y-1]*B[5,2,k,c,y-1];
          test_nums_ts[6,k,c,y] = test_nums_ts[6,k,c,y-1] + beta[6,k,c,y-1]*S[6,1,k,c,y-1]  +  beta2[6,1,k,c,y-1]*B[6,1,k,c,y-1]+ beta2[6,2,k,c,y-1]*B[6,2,k,c,y-1];
          test_nums_ts[7,k,c,y] = test_nums_ts[7,k,c,y-1] + beta[7,k,c,y-1]*S[7,1,k,c,y-1]  +  beta2[7,1,k,c,y-1]*B[7,1,k,c,y-1]+ beta2[7,2,k,c,y-1]*B[7,2,k,c,y-1];
          
          test_numspos_ts[1,k,c,y] = test_numspos_ts[1,k,c,y-1] + beta2[1,2,k,c,y-1]*B[1,2,k,c,y-1];
          test_numspos_ts[2,k,c,y] = test_numspos_ts[2,k,c,y-1] + beta2[2,2,k,c,y-1]*B[2,2,k,c,y-1];
          test_numspos_ts[3,k,c,y] = test_numspos_ts[3,k,c,y-1] + beta2[3,2,k,c,y-1]*B[3,2,k,c,y-1];
          test_numspos_ts[4,k,c,y] = test_numspos_ts[4,k,c,y-1] + beta2[4,2,k,c,y-1]*B[4,2,k,c,y-1];
          test_numspos_ts[5,k,c,y] = test_numspos_ts[5,k,c,y-1] + beta2[5,2,k,c,y-1]*B[5,2,k,c,y-1];
          test_numspos_ts[6,k,c,y] = test_numspos_ts[6,k,c,y-1] + beta2[6,2,k,c,y-1]*B[6,2,k,c,y-1];
          test_numspos_ts[7,k,c,y] = test_numspos_ts[7,k,c,y-1] + beta2[7,2,k,c,y-1]*B[7,2,k,c,y-1];
      }
    }
  }
  
  
  for (i in 1:ag){
// women
    test_reas_f[i,1,1] =  beta_scr[i,1,1]*S[i,1,1,1,timestep]+beta2_scr[i,1,1,1]*B[i,1,1,1,timestep] ; // screening
    test_reas_f[i,2,1] =                                      beta2_scr[i,2,1,1]*B[i,2,1,1,timestep] ; // PLHIV testing
    test_reas_f[i,3,1] =  0; // no prep for LR
    test_reas_f[i,4,1] =  tst_prg*beta_prg[i,timestep]*S[i,1,1,1,timestep]+tst_prg*beta_prg[i,timestep]*B[i,1,1,1,timestep] ; // pregnancy  
    test_reas_f[i,5,1] =  0 ; // (pwid - no LR)
    
    test_reas_f[i,1,2] =  beta_scr[i,1,2]*S[i,1,1,2,timestep]+beta2_scr[i,1,1,2]*B[i,1,1,2,timestep] ; // screening
    test_reas_f[i,2,2] =                                      beta2_scr[i,2,1,2]*B[i,2,1,2,timestep] ; // PLHIV testing
    test_reas_f[i,3,2] =                        tst_prep*beta_prep[i,1,timestep]*B[i,1,1,2,timestep] ; // PrEP
    test_reas_f[i,4,2] =  tst_prg*beta_prg[i,timestep]*S[i,1,1,2,timestep]+tst_prg*beta_prg[i,timestep]*B[i,1,1,2,timestep] ; // pregnancy   
    test_reas_f[i,5,2] =  tst_pwid*beta_pwid_hr[i,1]*S[i,1,1,2,timestep]+tst_pwid*beta_pwid_hr[i,1]*B[i,1,1,2,timestep] ; // PWID
// msw    
    test_reas_mw[i,1,1] = beta_scr[i,2,1]*S[i,1,2,1,timestep]+beta2_scr[i,1,2,1]*B[i,1,2,1,timestep]; // screening
    test_reas_mw[i,2,1] =                                     beta2_scr[i,2,2,1]*B[i,2,2,1,timestep] ; // PLHIV testing
    test_reas_mw[i,3,1] =  0; // no prep for LR
    test_reas_mw[i,4,1] =  0; // no pwid in LR
    
    test_reas_mw[i,1,2] = beta_scr[i,2,2]*S[i,1,2,2,timestep]+beta2_scr[i,1,2,2]*B[i,1,2,2,timestep]; // screening
    test_reas_mw[i,2,2] =                                     beta2_scr[i,2,2,2]*B[i,2,2,2,timestep] ; // PLHIV testing
    test_reas_mw[i,3,2] =                       tst_prep*beta_prep[i,2,timestep]*B[i,1,2,2,timestep] ; // PrEP
    test_reas_mw[i,4,2] =  tst_pwid*beta_pwid_hr[i,2]*S[i,1,2,2,timestep]+tst_pwid*beta_pwid_hr[i,2]*B[i,1,2,2,timestep] ; // PWID
// msm    
    test_reas_mm[i,1,1] = beta_scr[i,3,1]*S[i,1,3,1,timestep]+beta2_scr[i,1,3,1]*B[i,1,3,1,timestep]; // screening
    test_reas_mm[i,2,1] =                                     beta2_scr[i,2,3,1]*B[i,2,3,1,timestep] ; // PLHIV testing 
    test_reas_mm[i,3,1] =  0; // no prep for LR
    test_reas_mm[i,4,1] =  0; // no pwid in lr
    
    test_reas_mm[i,1,2] = beta_scr[i,3,2]*S[i,1,3,2,timestep]+beta2_scr[i,1,3,2]*B[i,1,3,2,timestep]; // screening
    test_reas_mm[i,2,2] =                                     beta2_scr[i,2,3,2]*B[i,2,3,2,timestep] ; // PLHIV testing 
    test_reas_mm[i,3,2] =                       tst_prep*beta_prep[i,3,timestep]*B[i,1,3,2,timestep] ; // PrEP
    test_reas_mm[i,4,2] =  tst_pwid*beta_pwid_hr[i,3]*S[i,1,3,2,timestep]+tst_pwid*beta_pwid_hr[i,3]*B[i,1,3,2,timestep] ; // PWID
    
  }
      
//print(test_nums_ts);
    for (y in 2:timestep){
            for (k in 1:s){
                for (c in 1:r){
                   for (i in 1:4){
   
         hiv_diag_ts[i,k,c,y] =  hiv_diag_ts[i,k,c,y-1] + beta[i,k,c,y-1]*S[i,2,k,c,y-1] + beta2[i,1,k,c,y-1]*B[i,2,k,c,y-1];// 
         known_hiv_prev_ts[i,k,c,y] = known_hiv_prev_ts[i,k,c,y-1] + (B[i,2,k,c,y]);// 
        }
        hiv_diag_ts[5,k,c,y] = hiv_diag_ts[5,k,c,y-1] + beta[5,k,c,y-1]*S[5,2,k,c,y-1] + beta2[5,1,k,c,y-1]*B[5,2,k,c,y-1]+ beta[6,k,c,y-1]*S[6,2,k,c,y-1] + beta2[6,1,k,c,y-1]*B[6,2,k,c,y-1]+beta[7,k,c,y-1]*S[7,2,k,c,y-1] + beta2[7,1,k,c,y-1]*B[7,2,k,c,y-1];
      
        known_hiv_prev_ts[5,k,c,y] =  known_hiv_prev_ts[5,k,c,y-1] + (B[5,2,k,c,y]+B[6,2,k,c,y]+B[7,2,k,c,y]); // 
      }
     }
     test_ts[y] = test_ts[y-1] + 1; 
    }
  
  // loop over cumulative estimates to get yearly estimates

  for (y0 in 2:(timespan+1)){
       for (i in 1:5){
            for (k in 1:s){
              
    hiv_diag[i,k,y0-1] = (hiv_diag_ts[i,k,1,t1[y0]]-hiv_diag_ts[i,k,1,t1[y0-1]])+(hiv_diag_ts[i,k,2,t1[y0]]-hiv_diag_ts[i,k,2,t1[y0-1]]);
    known_hiv_prev[i,k,y0-1] = (known_hiv_prev_ts[i,k,1,t1[y0]]-known_hiv_prev_ts[i,k,1,t1[y0-1]])+(known_hiv_prev_ts[i,k,2,t1[y0]]-known_hiv_prev_ts[i,k,2,t1[y0-1]]);
           }
       }
    }
 
     for (y0 in 2:(timespan+1)){
         for (i in 1:ag){
            for (k in 1:s){

    test_nums[i,k,1,y0-1]     = (test_nums_ts[i,k,1,t1[y0]]-test_nums_ts[i,k,1,t1[y0-1]]);
    test_nums[i,k,2,y0-1]     = (test_nums_ts[i,k,2,t1[y0]]-test_nums_ts[i,k,2,t1[y0-1]]);
    test_rec_nums[i,k,1,y0-1] = (test_rec_nums_ts[i,k,1,t1[y0]]-test_rec_nums_ts[i,k,1,t1[y0-1]]);
    test_rec_nums[i,k,2,y0-1] = (test_rec_nums_ts[i,k,2,t1[y0]]-test_rec_nums_ts[i,k,2,t1[y0-1]]);
    test_numspos[i,k,1,y0-1] = (test_numspos_ts[i,k,1,t1[y0]]-test_numspos_ts[i,k,1,t1[y0-1]]);
    test_numspos[i,k,2,y0-1] = (test_numspos_ts[i,k,2,t1[y0]]-test_numspos_ts[i,k,2,t1[y0-1]]);
          }
        }
    test[y0-1] = test_ts[t1[y0]]-test_ts[t1[y0-1]];    
      }

  testvol_est[1] = sum(test_nums[,1,1,21])+sum(test_nums[,1,2,21])+sum(test_nums[,2,1,21])+sum(test_nums[,2,2,21])+sum(test_nums[,3,1,21])+sum(test_nums[,3,2,21]);
  testvol_est[2] = sum(test_nums[,1,1,22])+sum(test_nums[,1,2,22])+sum(test_nums[,2,1,22])+sum(test_nums[,2,2,22])+sum(test_nums[,3,1,22])+sum(test_nums[,3,2,22]);
  testvol_est[3] = sum(test_nums[,1,1,23])+sum(test_nums[,1,2,23])+sum(test_nums[,2,1,23])+sum(test_nums[,2,2,23])+sum(test_nums[,3,1,23])+sum(test_nums[,3,2,23]);
  
  //print(testvol_est);
 
  // this selects samples every 1 year --- IS THE INDEXING OK (y0-1), doublecheck

   for (y0 in 1:timespan){
     for (i in 1:ag){    
          for (k in 1:s){
     pr_tested[i,k,y0] = (B[i,1,k,1,t1[y0]]+B[i,2,k,1,t1[y0]]+ B[i,1,k,2,t1[y0]]+B[i,2,k,2,t1[y0]])/
                           (B[i,1,k,1,t1[y0]]+B[i,2,k,1,t1[y0]]+S[i,1,k,1,t1[y0]]+B[i,1,k,2,t1[y0]]+B[i,2,k,2,t1[y0]]+S[i,1,k,2,t1[y0]]);
                            
    pr_tested_r1[i,k,y0] = (B[i,1,k,1,t1[y0]]+B[i,2,k,1,t1[y0]])/ (B[i,1,k,1,t1[y0]]+B[i,2,k,1,t1[y0]]+S[i,1,k,1,t1[y0]] );     
    
    pr_tested_r2[i,k,y0] = (B[i,1,k,2,t1[y0]]+B[i,2,k,2,t1[y0]])/ ( B[i,1,k,2,t1[y0]]+B[i,2,k,2,t1[y0]]+S[i,1,k,2,t1[y0]]);

     
       // [ag,d,s,r,timestep]
       ////////////////////////////////
       //////////////////////////////// FIX THIS WITH QUEST DATA
       ////////////////////////////////
    pr_rec_tested[i,k,y0] = 0.5*(test_nums[i,k,1,y0]+test_nums[i,k,2,y0])/ (B[i,1,k,1,t1[y0]]+B[i,2,k,1,t1[y0]]+B[i,1,k,2,t1[y0]]+B[i,2,k,2,t1[y0]]+S[i,1,k,1,t1[y0]]+S[i,1,k,2,t1[y0]]);
    pr_rec_tested_r1[i,k,y0] = 0.5*(test_nums[i,k,1,y0])/ (B[i,1,k,1,t1[y0]]+B[i,2,k,1,t1[y0]]+S[i,1,k,1,t1[y0]]);
    pr_rec_tested_r2[i,k,y0] = 0.5*(test_nums[i,k,2,y0])/ (B[i,1,k,2,t1[y0]]+B[i,2,k,2,t1[y0]]+S[i,1,k,2,t1[y0]]);
       }
     }
   }
    
  // print(pr_tested);
  
   for (y0 in 2:(timespan+1)){
        for (i in 1:4){
     // by sex      
     Ntot5cat[i,1,y0-1] =  S[i,1,1,1,t1[y0]]+B[i,1,1,1,t1[y0]]+S[i,2,1,1,t1[y0]]+B[i,2,1,1,t1[y0]] +S[i,1,1,2,t1[y0]]+B[i,1,1,2,t1[y0]]+S[i,2,1,2,t1[y0]]+B[i,2,1,2,t1[y0]];
     Ntot5cat[i,2,y0-1] =  S[i,1,2,1,t1[y0]]+B[i,1,2,1,t1[y0]]+S[i,2,2,1,t1[y0]]+B[i,2,2,1,t1[y0]] +  
                           S[i,1,3,1,t1[y0]]+B[i,1,3,1,t1[y0]]+S[i,2,3,1,t1[y0]]+B[i,2,3,1,t1[y0]] +
                           S[i,1,2,2,t1[y0]]+B[i,1,2,2,t1[y0]]+S[i,2,2,2,t1[y0]]+B[i,2,2,2,t1[y0]] +  
                           S[i,1,3,2,t1[y0]]+B[i,1,3,2,t1[y0]]+S[i,2,3,2,t1[y0]]+B[i,2,3,2,t1[y0]]; 
        }
     Ntot5cat[5,1,y0-1] =   S[5,1,1,1,t1[y0]]+B[5,1,1,1,t1[y0]]+S[5,2,1,1,t1[y0]]+B[5,2,1,1,t1[y0]]+ S[5,1,1,2,t1[y0]]+B[5,1,1,2,t1[y0]]+S[5,2,1,2,t1[y0]]+B[5,2,1,2,t1[y0]]+
                            S[6,1,1,1,t1[y0]]+B[6,1,1,1,t1[y0]]+S[6,2,1,1,t1[y0]]+B[6,2,1,1,t1[y0]]+ S[6,1,1,2,t1[y0]]+B[6,1,1,2,t1[y0]]+S[6,2,1,2,t1[y0]]+B[6,2,1,2,t1[y0]]+
                            S[7,1,1,1,t1[y0]]+B[7,1,1,1,t1[y0]]+S[7,2,1,1,t1[y0]]+B[7,2,1,1,t1[y0]]+ S[7,1,1,2,t1[y0]]+B[7,1,1,2,t1[y0]]+S[7,2,1,2,t1[y0]]+B[7,2,1,2,t1[y0]]; 
                        
     Ntot5cat[5,2,y0-1] =   S[5,1,2,1,t1[y0]]+B[5,1,2,1,t1[y0]]+S[5,2,2,1,t1[y0]]+B[5,2,2,1,t1[y0]]+ S[5,1,2,2,t1[y0]]+B[5,1,2,2,t1[y0]]+S[5,2,2,2,t1[y0]]+B[5,2,2,2,t1[y0]]+
                            S[6,1,2,1,t1[y0]]+B[6,1,2,1,t1[y0]]+S[6,2,2,1,t1[y0]]+B[6,2,2,1,t1[y0]]+ S[6,1,2,2,t1[y0]]+B[6,1,2,2,t1[y0]]+S[6,2,2,2,t1[y0]]+B[6,2,2,2,t1[y0]]+
                            S[7,1,2,1,t1[y0]]+B[7,1,2,1,t1[y0]]+S[7,2,2,1,t1[y0]]+B[7,2,2,1,t1[y0]]+ S[7,1,2,2,t1[y0]]+B[7,1,2,2,t1[y0]]+S[7,2,2,2,t1[y0]]+B[7,2,2,2,t1[y0]]+
                            S[5,1,3,1,t1[y0]]+B[5,1,3,1,t1[y0]]+S[5,2,3,1,t1[y0]]+B[5,2,3,1,t1[y0]]+ S[5,1,3,2,t1[y0]]+B[5,1,3,2,t1[y0]]+S[5,2,3,2,t1[y0]]+B[5,2,3,2,t1[y0]]+
                            S[6,1,3,1,t1[y0]]+B[6,1,3,1,t1[y0]]+S[6,2,3,1,t1[y0]]+B[6,2,3,1,t1[y0]]+ S[6,1,3,2,t1[y0]]+B[6,1,3,2,t1[y0]]+S[6,2,3,2,t1[y0]]+B[6,2,3,2,t1[y0]]+
                            S[7,1,3,1,t1[y0]]+B[7,1,3,1,t1[y0]]+S[7,2,3,1,t1[y0]]+B[7,2,3,1,t1[y0]]+ S[7,1,3,2,t1[y0]]+B[7,1,3,2,t1[y0]]+S[7,2,3,2,t1[y0]]+B[7,2,3,2,t1[y0]]; 
   }
    
    
  //  print(hiv_diag_ts);
  // end of transformed parameters
}

model {
  int id_temp;
  int id_temp2;
  int id_temp3;
  //priors
  // tp[1] ~ beta(1, 50);
  // tp[2] ~ beta(1, 50);
  // tp[3] ~ beta(1, 50);
  
  p0[1] ~beta(hiv_test_posit_wm[1], hiv_test_posit_wm[2]);
  p0[2] ~beta(hiv_test_posit_wm[1], hiv_test_posit_wm[2]);
  p0[3] ~beta(hiv_test_posit_msm[1], hiv_test_posit_msm[2]);
  p1[1] ~  beta(1,10000);
  p1[2] ~  beta(1,10000);
  p1[3] ~  beta(1,800);

  bp0[1,1] ~ beta(8,8);
  bp0[2,1] ~ beta(8,8);
  bp0[3,1] ~ beta(8,8);
  bp0[4,1] ~ beta(8,8);
  bp0[5,1] ~ beta(8,8);
  bp0[6,1] ~ beta(8,8);
  bp0[7,1] ~ beta(8,8);
  
  bp0[1,2] ~ beta(8,8);
  bp0[2,2] ~ beta(8,8);
  bp0[3,2] ~ beta(8,8);
  bp0[4,2] ~ beta(8,8);
  bp0[5,2] ~ beta(8,8);
  bp0[6,2] ~ beta(8,8);
  bp0[7,2] ~ beta(8,8);
  
  bp0[1,3] ~ beta(40,2);
  bp0[2,3] ~ beta(40,2);
  bp0[3,3] ~ beta(21.292, 128.041);
  bp0[4,3] ~ beta(21.292, 128.041);
  bp0[5,3] ~ beta(40,2);
  bp0[6,3] ~ beta(40,2);
  bp0[7,3] ~ beta(40,2);

// beta2[ag,d,s,r],  beta[ag,s,r]
 for (i in 1:ag){ 
   for (k in 1:s){ 
      for (rr in 1:r){ 
          beta_scr[i,k, rr] ~ beta(1, 10);
          beta2_scr[i,1,k,rr] ~ beta(1, 10);
          beta2_scr[i,2,k,rr] ~ beta(1, 100);
      }
    }
  }
    
  beta_prg_pr[1,1]~ beta(beta_prg_alpha[1], beta_prg_beta[1]);
  beta_prg_pr[1,2] ~ beta(beta_prg_alpha[2], beta_prg_beta[2]);
  
  beta_prg_pr[2,1] ~ beta(beta_prg_alpha[3], beta_prg_beta[3]);
  beta_prg_pr[2,2] ~ beta(beta_prg_alpha[4], beta_prg_beta[4]);
  
  beta_prg_pr[3,1] ~ beta(beta_prg_alpha[5], beta_prg_beta[5]);
  beta_prg_pr[3,2] ~ beta(beta_prg_alpha[6], beta_prg_beta[6]);
  
  beta_prg_pr[4,1] ~ beta(beta_prg_alpha[7], beta_prg_beta[7]);
  beta_prg_pr[4,2] ~ beta(beta_prg_alpha[8], beta_prg_beta[8]);

  // 2017
  beta_prep_w[1,1] ~ beta(beta_prep_w_rr_alpha[1], beta_prep_w_rr_beta[1]);
  beta_prep_w[1,2] ~ beta(beta_prep_w_alpha[1], beta_prep_w_beta[1]);

  beta_prep_w[2,1]  ~ beta(beta_prep_w_rr_alpha[2], beta_prep_w_rr_beta[2]);
  beta_prep_w[2,2]  ~ beta(beta_prep_w_alpha[2], beta_prep_w_beta[2]);
  
  beta_prep_w[3,1] ~ beta(beta_prep_w_rr_alpha[3], beta_prep_w_rr_beta[3]);
  beta_prep_w[3,2]  ~ beta(beta_prep_w_alpha[3], beta_prep_w_beta[3]);

  beta_prep_w[4,1]  ~ beta(beta_prep_w_rr_alpha[4], beta_prep_w_rr_beta[4]);
  beta_prep_w[4,2]  ~ beta(beta_prep_w_alpha[4], beta_prep_w_beta[4]);  
  
  beta_prep_w[5,1]  ~ beta(beta_prep_w_rr_alpha[5], beta_prep_w_rr_beta[5]);
  beta_prep_w[5,2]  ~ beta(beta_prep_w_alpha[5], beta_prep_w_beta[5]);  
  
  beta_prep_msw[1,1] ~ beta(beta_prep_msw_rr_alpha[1], beta_prep_msw_rr_beta[1]);
  beta_prep_msw[1,2] ~ beta(beta_prep_msw_alpha[1], beta_prep_msw_beta[1]);

  beta_prep_msw[2,1]  ~ beta(beta_prep_msw_rr_alpha[2], beta_prep_msw_rr_beta[2]);
  beta_prep_msw[2,2]  ~ beta(beta_prep_msw_alpha[2], beta_prep_msw_beta[2]);
  
  beta_prep_msw[3,1]  ~ beta(beta_prep_msw_rr_alpha[3], beta_prep_msw_rr_beta[3]);
  beta_prep_msw[3,2]  ~ beta(beta_prep_msw_alpha[3], beta_prep_msw_beta[3]);

  beta_prep_msw[4,1]  ~ beta(beta_prep_msw_rr_alpha[4], beta_prep_msw_rr_beta[4]);
  beta_prep_msw[4,2]  ~ beta(beta_prep_msw_alpha[4], beta_prep_msw_beta[4]);  
  
  beta_prep_msw[5,1]  ~ beta(beta_prep_msw_rr_alpha[5], beta_prep_msw_rr_beta[5]);
  beta_prep_msw[5,2]  ~ beta(beta_prep_msw_alpha[5], beta_prep_msw_beta[5]);  
  
  beta_prep_msm[1,1] ~ beta(beta_prep_msm_rr_alpha[1], beta_prep_msm_rr_beta[1]);
  beta_prep_msm[1,2] ~ beta(beta_prep_msm_alpha[1], beta_prep_msm_beta[1]);

  beta_prep_msm[2,1]  ~ beta(beta_prep_msm_rr_alpha[2], beta_prep_msm_rr_beta[2]);
  beta_prep_msm[2,2]  ~ beta(beta_prep_msm_alpha[2], beta_prep_msm_beta[2]);
  
  beta_prep_msm[3,1]  ~ beta(beta_prep_msm_rr_alpha[3], beta_prep_msm_rr_beta[3]);
  beta_prep_msm[3,2]  ~ beta(beta_prep_msm_alpha[3], beta_prep_msm_beta[3]);

  beta_prep_msm[4,1]  ~ beta(beta_prep_msm_rr_alpha[4], beta_prep_msm_rr_beta[4]);
  beta_prep_msm[4,2]  ~ beta(beta_prep_msm_alpha[4], beta_prep_msm_beta[4]);  
  
  beta_prep_msm[5,1]  ~ beta(beta_prep_msm_rr_alpha[5], beta_prep_msm_rr_beta[5]);
  beta_prep_msm[5,2]  ~ beta(beta_prep_msm_alpha[5], beta_prep_msm_beta[5]);  
  
  beta_pwid ~beta(beta_pwid_shape[1], beta_pwid_shape[2]);
  
  tst_pwid ~beta(test_pr_pwid[1], test_pr_pwid[2]);
  tst_prg ~gamma(test_pr_prg[1], test_pr_prg[2]);
  tst_prep ~gamma(test_pr_prep[1], test_pr_prep[2]);

  hiv_init[1,1] ~ beta(7.326, 10000);
  hiv_init[2,1] ~ beta(11.413, 8502.631);
  hiv_init[3,1] ~ beta(11.413, 8502.631);
  hiv_init[4,1] ~ beta(11.413, 8502.631);
  hiv_init[5,1] ~ beta(11.413, 8502.631);
  hiv_init[6,1] ~ beta(11.413, 8502.631);
  hiv_init[7,1] ~ beta(11.413, 8502.631);
  
  hiv_init[1,2] ~ beta(7.326, 10000);
  hiv_init[2,2] ~ beta(11.413, 8502.631);
  hiv_init[3,2] ~ beta(11.413, 8502.631);
  hiv_init[4,2] ~ beta(11.413, 8502.631);
  hiv_init[5,2] ~ beta(11.413, 8502.631);
  hiv_init[6,2] ~ beta(11.413, 8502.631);
  hiv_init[7,2] ~ beta(11.413, 8502.631);
  
  hiv_init[1,3] ~ beta(16.861, 170.543 );
  hiv_init[2,3] ~ beta(16.861, 170.543 );
  hiv_init[3,3] ~ beta(16.861, 170.543 );
  hiv_init[4,3] ~ beta(16.861, 170.543 );
  hiv_init[5,3] ~ beta(16.861, 170.543 );
  hiv_init[6,3] ~ beta(16.861, 170.543 );
  hiv_init[7,3] ~ beta(16.861, 170.543 );
  // 

  mu_test ~normal(d_testvol[1]*0.05, d_testvol[1]*0.01);
  coverage ~beta(beta_testcov[1], beta_testcov[2]);
  
  for (i in 1:5){
  mu_hd[1,i] ~ normal(d_hiv_diag[i,14,1]*0.05, d_hiv_diag[i,14,1]*0.01);
  mu_hd[2,i] ~ normal(d_hiv_diag[i,14,2]*0.05, d_hiv_diag[i,14,2]*0.01);
  mu_hd[3,i] ~ normal(d_hiv_diag[i,14,3]*0.05, d_hiv_diag[i,14,3]*0.01);
  
  mu_kh[1,i] ~ normal(d_hiv_known[i,14,1]*0.05, d_hiv_known[i,14,1]*0.05);
  mu_kh[2,i] ~ normal(d_hiv_known[i,14,2]*0.05, d_hiv_known[i,14,2]*0.05);
  mu_kh[3,i] ~ normal(d_hiv_known[i,14,3]*0.05, d_hiv_known[i,14,3]*0.01);
  }

// testing patterns
// msw women
for (i in 1:ag){
  id_temp = 1;
  for (y in dv){ // 2008-2015
    d_tested_f[i,id_temp] ~ binomial(d_denom, pr_tested[i,1,y]);
    d_tested_m[i,id_temp] ~ binomial(d_denom, pr_tested[i,2,y]);
    id_temp = id_temp+1;
  }
}

for (i in 1:5){
    d_tested_f_r1[i] ~ binomial(d_denom, pr_tested_r1[i,1,15]);
    d_tested_m_r1[i] ~ binomial(d_denom, pr_tested_r1[i,2,15]);
    
    d_tested_f_r2[i] ~ binomial(d_denom, pr_tested_r2[i,1,15]);
    d_tested_m_r2[i] ~ binomial(d_denom, pr_tested_r2[i,2,15]);
}
// msm
 for (i in 1:5){
   id_temp2 = 1;
  for (y in dmsmv){
    d_tested_msm[i,id_temp2] ~ binomial(d_denom, pr_tested[i,3,y]);
    d_tested_recent_msm[i,id_temp2] ~ binomial(d_denom, pr_rec_tested[i,3,y]);
    id_temp2 = id_temp2+1;
  }
    // d_tested_msm_r1[i] ~ binomial(1000, pr_tested_r1[i,3,15]);
    // d_tested_msm_r2[i] ~ binomial(1000, pr_tested_r2[i,3,15]);
}

// NSFG data
for (i in 1:3){
  for (y in dm){
    id_temp2 = 1;
    d_tested_recent_m2[i,id_temp2] ~ binomial(d_denom, pr_rec_tested[i,2,y]);
    d_tested_recent_f2[i,id_temp2] ~ binomial(d_denom, pr_rec_tested[i,1,y]);
    
   // d_tested_msm2[i,id_temp2] ~ binomial(d_denom, pr_tested[i,3,y]);
    d_tested_m2[i,id_temp2] ~ binomial(d_denom, pr_tested[i,2,y]);
    d_tested_f2[i,id_temp2] ~ binomial(d_denom, pr_tested[i,1,y]);

    id_temp2 = id_temp2+1;
  }
}

// testing volume
  d_testvol[1] ~ normal(testvol_est[1]*coverage, mu_test);
  d_testvol[2] ~ normal(testvol_est[2]*coverage, mu_test);
  d_testvol[3] ~ normal(testvol_est[3]*coverage, mu_test);

// CURRENTLY NOT CALIBRATING TO DIAGNOSES OR PLHIV
//  // hiv diagnoses and current known PHIV, HIV deaths
for (i in 1:5){
  for (y in 10:23){ //2008-2021
   for (k in 1:s){
   d_hiv_diag[i,y-9,k] ~ normal(hiv_diag[i,k,y], mu_hd[k,i]);
     }
   }
  }
  
for (i in 1:5){
  for (y in 10:23){ //2008-2021
   for (k in 1:s){
      id_temp3 = 1;
   d_hiv_known[i,id_temp3,k]~ normal(known_hiv_prev[i,k,y], mu_kh[k,i]);
   id_temp3 =id_temp3+1;
     }
   }
  }

}

generated quantities {
 // real time_to_retest[ag,d,s,timespan];
  real Ntot[ag,s,timespan]; 
  real  Ntot_risk[ag,s,r,timespan];
  real  Nrisk_prop[ag,s,r,timespan];
  real test_totals[5];
 // real<lower = 0> ever_tested[4,10];
//  real rec_tested[ag,s,timespan];
 // real test_out;
 // int v_out[3];
  
 // test_out = test;
 // v_out = dmsmv;
 
 test_totals[1] = sum(test_reas_f[,1,1])+sum(test_reas_f[,1,2])+sum(test_reas_mw[,1,1])+sum(test_reas_mw[,1,2])+sum(test_reas_mm[,1,1])+sum(test_reas_mm[,1,2]) ; // screening  
 test_totals[2] = sum(test_reas_f[,2,1])+sum(test_reas_f[,2,2])+sum(test_reas_mw[,2,1])+sum(test_reas_mw[,2,2])+sum(test_reas_mm[,2,1])+sum(test_reas_mm[,2,2]) ; // plhiv 
 test_totals[3] = sum(test_reas_f[,3,1])+sum(test_reas_f[,3,2])+sum(test_reas_mw[,3,1])+sum(test_reas_mw[,3,2])+sum(test_reas_mm[,3,1])+sum(test_reas_mm[,3,2]) ; // prep 
 test_totals[4] = sum(test_reas_f[,4,1])+sum(test_reas_f[,4,2]) ; // pregnancy 
 test_totals[5] = sum(test_reas_f[,5,1])+sum(test_reas_f[,5,2])+sum(test_reas_mw[,4,1])+sum(test_reas_mw[,4,2])+sum(test_reas_mm[,4,1])+sum(test_reas_mm[,4,2]) ; // pwid  

  
  for (y0 in 2:(timespan+1)){
    for (i in 1:ag){
//  time_to_retest[i,y] = 1 / beta2[i,1,k]; // this is a weird place to have this in absence of time-varying change
  for (k in 1:s){
 // rec_tested[i,k,y0-1] =  (R[i,1,k,1,t1[y0]]+R[i,1,k,2,t1[y0]])/(R[i,1,k,1,t1[y0]]+B[i,1,k,1,t1[y0]]+S[i,1,k,1,t1[y0]]+R[i,1,k,2,t1[y0]]+B[i,1,k,2,t1[y0]]+S[i,1,k,2,t1[y0]]) ; ;
  Ntot[i,k,y0-1] =  S[i,1,k,1,t1[y0]]+B[i,1,k,1,t1[y0]]+S[i,2,k,1,t1[y0]]+B[i,2,k,1,t1[y0]] +
                    S[i,1,k,2,t1[y0]]+B[i,1,k,2,t1[y0]]+S[i,2,k,2,t1[y0]]+B[i,2,k,2,t1[y0]];
  for (c in 1:r){
      Ntot_risk[i,k,c,y0-1] =  S[i,1,k,c,t1[y0]]+B[i,1,k,c,t1[y0]]+S[i,2,k,c,t1[y0]]+B[i,2,k,c,t1[y0]];
      Nrisk_prop[i,k,c,y0-1] = Ntot_risk[i,k,c,y0-1] /Ntot[i,k,y0-1];
  }


    }
   }
  }

  //print(col(to_matrix(y), 2)+ col(to_matrix(y), 3))
  
}
