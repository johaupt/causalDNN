expControl <- function(n_var, mode="regression", tau_zero=NULL){
  beta = rnorm(n_var, mean = 0, sd = 0.2)
  beta_x2 = rnorm(n_var, mean = 0, sd = 0.2)
  beta_tau = rnorm(n_var, mean = 0, sd = 0.2)
  if(is.null(tau_zero)){
      tau_zero = rnorm(1, mean=-0.1, sd=0.01)
  }
  
  return(list("beta"=beta, "beta_x2"=beta_x2, 
              "beta_tau"=beta_tau, "tau_zero" = tau_zero))
  
}

experiment(X, expControl, g=NULL, prop_score=NULL){
  n_obs = nrow(X)
  beta = expControl$beta
  beta_x2 = expControl$beta_x2
  beta_tau = expControl$beta_tau
  tau_zero = expControl$tau_zero
  mode = expControl$mode
  
  tau = tau_zero + X%*%beta_tau + rnorm(n_obs, 0, 0.001)
  y = X%*%beta + g*tau + rnorm(n_obs, 0, 0.1)
  
  if(mode == "classification"):
    y = 1/1+exp(-y)
    y = as.numeric(y>=0.5)
  
}



experiment(expControl(10))

