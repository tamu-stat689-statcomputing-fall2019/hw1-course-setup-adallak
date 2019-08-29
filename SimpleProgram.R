# Generate data from linear regression model and calculate the least squares vector of coefficients
#####################################################################################################
source("FunctionsLM.R") ## Calling the file which stores the functions

# Model parameters
n = 100 # sample size
p = 10 # number of covariates
sigma = 2 # noise standard deviation
beta = rep(2,p) # true vector of coefficients
X = matrix(rnorm(n*p), n, p) # n by p matrix of predictors

# [ToDo] Use generateY function to generate Y
y = generateY(X,beta,sigma)
# [ToDo] Use calculateBeta function to calculate beta_LS
beta_LS= calculateBeta(X,y)
# [ToDo] Use calculateMSE to assess the estimation error measured by squared eucledian distance - ||beta - beta_LS||_2^2
calculateMSE(beta,beta_LS)

### Checking that linear regression function works properly
m1<-lm(y~0+X)
beta_lm = coef(m1)

eps = 10e-10 ## Threshold two compare two results
if(as.numeric(crossprod(beta_lm-beta_LS))<eps)
{
  print("identical")
}else{
  print("Not-identical")
}
