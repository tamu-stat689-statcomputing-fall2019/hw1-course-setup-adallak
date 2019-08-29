# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  ### sanity check
  if(sigma <= 0){
    stop("Sigma should be Positive")
  }
  n <- dim(X)[1] ## data dimension 
  error = rnorm(n, 0, sigma)
  Y = X%*%beta + error
  # Return Y
  return(Y)
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y -response
calculateBeta <- function(X, Y){
  # Calculate beta_LS
  Xty = crossprod(X, y)
  XtX = crossprod(X, X)
  beta_LS = solve(XtX,Xty)
  # Return beta
  return(beta_LS)
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
  MSE = as.numeric(crossprod(beta-beta_LS))
  # Return MSE - error ||beta - beta_LS||_2^2
  return(MSE)
}