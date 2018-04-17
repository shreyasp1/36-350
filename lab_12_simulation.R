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
    data = generate_data(n,p)
    p_val = model_select(data$matrix, data$responses, cutoff)
    all.p.vals = c(all.p.vals, p_val)
  }
  write(all.p.vals, file = "p_vals.txt")
}

make_plot = function(datapath){
  s = readLines(datapath)
  p.vals = as.numeric(strsplit(s, split = " ")[[1]])
  hist(p.vals)
}