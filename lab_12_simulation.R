# 36-350
generate_data = function(n,p){
  covariates = matrix(rnorm(n*p,0,1))
  responses = rnorm(n,0,1)
  return(list(covariates = covariates, responses = responses))
}