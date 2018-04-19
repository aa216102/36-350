# 36-350
generate_data = function(n,p){
  covariates = matrix(rnorm(n*p,0,1), nrow = n, ncol = p)
  responses = rnorm(n,0,1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff){
  regression = lm(responses~covariates)
  cutoff.indices = summary(regression)$coefficients[2:length(summary(regression)$coefficients[,4]),4]<=cutoff
  cutoff.regression = lm(responses ~ covariates[,cutoff.indices])
  return(cutoff.regression)
}

run_simulation = function(n_trials, n, p, cutoff){
  p.values = c()
  for(i in 1:n_trials){
    covariates = generate_data(n,p)$covariates
    responses = generate_data(n,p)$responses
    cutoff.regression = model_select(covariates, responses, cutoff)
    p.values = c(p.values, 
                 summary(cutoff.regression)$coefficients[2:length(summary(cutoff.regression)$coefficients[,4]),4])
  }
  return(hist(p.values))
}