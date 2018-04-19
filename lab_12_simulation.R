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
    print(19)
    responses = generate_data(n,p)$responses
    print(21)
    cutoff.regression = model_select(covariates, responses, cutoff)
    print(23)
    p.values = c(p.values, 
                 summary(cutoff.regression)$coefficients[2:length(summary(cutoff.regression)$coefficients[,4]),4])
    print(26)
    print(p.values)
  }
  return(hist(p.values))
}
par(mfrow=c(3,3))
n.vals = c(100,1000,10000)
p.vals = c(10,20,50)
for(i in 1:3){
  for(j in 1:3){
    run_simulation(20, n.vals[i], p.vals[j], .05)
  }
}