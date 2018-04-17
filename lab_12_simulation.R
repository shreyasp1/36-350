generate_data = function(n,p){
  m = matrix(rnorm(n*p), nrow = n, ncol = p)
  resp = rnorm(n)
  return(list("matrix" = m, "responses" = resp))
}

model_select = function(cov, resp, cutoff){
  regr.lm = lm(resp ~ cov)
  coef.p_vals = (summary(regr.lm)$coefficients[,4])[-1]
  sig.ceof = which(coef.p_vals <= cutoff)
  regr.coef.lm = lm(resp ~ cov[,sig.ceof])
  return((summary(regr.coef.lm)$coefficients[,4])[-1])
  
}

run_simulation = function(n_trials, n, p ,cutoff){
  all.p.vals = numeric(0)
  for(i in 1:n_trials){
    data = generate_data(n[i],p[i])
    p_val = model_select(data$matrix, data$responses, cutoff)
    all.p.vals = c(all.p.vals, p_val)
  }
  hist(all.p.vals)
}

run_simulation(3, c(100,1000,10000), c(10,20,50), 0.05)


