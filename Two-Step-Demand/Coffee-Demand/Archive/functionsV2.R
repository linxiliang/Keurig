# Functions for logit estimation
ubar <- function(a, b){
  uimplicit <- function(x)  return(x - exp(a-b*x))
  jacu <- function(x) as.matrix(1 + b*exp(a-b*x))
  xstart = 50
  return(nleqslv(xstart, uimplicit, jacu, method="Broyden", global="none", control=list(btol=.0001))$x)
}

ll_linear<- function(b, i=0, dt=dt){
  if (i == 0){
    istart = 1 
    iend = nrow(dt)
  } else{
    istart = dt[1, start]
    iend = dt[1, end]
  }
  ixmat = cbind(XMat[istart:iend, ], log(b[np+1] - dt[, price]))
  dt[, `:=`(a = ixmat %*% b[1:(np-1)] + size1_amount)]
  dt[, `:=`(u = log(ubar(a, b[np]))), by = "row_i"]
  dtprod = dt[, .(mu = max(u)), by = c("hh", "t", "brand_descr", "keurig", "purchased")]
  dtprod[, `:=`(mu = mu - mean(mu)), by = c("hh", "t")]
  dtprod[, prob := exp(mu)/sum(exp(mu)), by = c("hh", "t")]
  llv = -(dtprod[, sum(purchased*log(prob))])
  return(llv)
}

# Code up posterior proportions 
pif_linear = function(h, b0, b1, bhat, sig){
  dft = hh_market_prod[.(h), ]
  csig = backsolve(chol(sig),diag(np))
  b1x = b1
  b0x = b0 
  b1x[np-1] = log(b1x[np-1])
  b0x[np-1] = log(b0x[np-1])
  return(exp(-ll_linear(b1, i=h, dft) + lndMvn(b1x[1:np], bhat, csig) + log(dnorm(b1[np+1], a, b)) + 
               ll_linear(b0, i=h, dft) - lndMvn(b0x[1:np], bhat, csig) - log(dnorm(b0[np+1], a, b))))
}

# Function for the Metropolis RW draws
rwmhd_linear <- function(i){
  nti = hh_market_prod[.(i), nt_i][1]
  cov_i = s2 * sig
  b0 = beta0[i,]
  b0[np] = exp(b0[np-1])
  b1 = beta0[i,] + mvrnorm(1, rep(0, np), cov_i/nti)
  b1[np-1] = exp(b1[np-1])
  T1 = T0[i] + rnorm(1, 0, sqrt(Tv/nh))
  if (T1 <= max(hh_market_prod[.(i), price])){
    T1 = max(hh_market_prod[.(i), price])+0.0001
  }
  alpha = min(1, pif_linear(i, c(b0, T0[i]), c(b1, T1), bhat, sig))
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  b1[np-1] = log(b1[np-1])
  if (runif(1) <= alpha) return(c(b1, T1)) else return(c(beta0[i,], T0[i]))
}
