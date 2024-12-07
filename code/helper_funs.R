

betavars <- function(mu1, var1){
  alpha1 = ((1 - mu1) / var1 - 1 / mu1) * mu1 ^ 2;
  beta1 = alpha1 * (1 / mu1 - 1);
  
  return(list(alpha=alpha1, beta=beta1))
}
