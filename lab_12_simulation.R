generate_data = function(n,p){
  m = matrix(rnorm(n*p), nrow = n, ncol = p)
  resp = rnorm(n)
  list("matrix" = m, "responses" = resp)
}

