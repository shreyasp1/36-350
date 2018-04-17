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

