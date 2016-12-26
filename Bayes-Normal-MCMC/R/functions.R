# Functions for logit estimation

ll<- function(b){
  hh_panel[, expu:=exp(cbind(a1, a3, a4, a6, a7, a8, a9, a10, a11, 
                                 a13, a14, a15, a17, price, size1_amount, 
                                 brand_lag, brand_cum) %*% c(0,b))]
  hh_panel[, prob:=expu/sum(expu), by="t"]
  llv = -(hh_panel[, sum(purchased*log(prob))])
  return(llv)
}


llfrac<- function(b, dt){
  dt[, expu:=exp(cbind(a1, a3, a4, a6, a7, a8, a9, a10, a11, 
                       a13, a14, a15, a17, price, size1_amount, 
                       brand_lag, brand_cum) %*% c(0,b))]
  dt[, prob:=expu/sum(expu), by="t"]
  llv = -(dt[, sum(purchased*log(prob))])
  return(llv)
}

llg<-function(b){
  #hh_panel = copy(hh_panel)
  hh_panel[, expu:=exp(cbind(a1, a3, a4, a6, a7, a8, a9, a10, a11, 
                                 a13, a14, a15, a17, price, size1_amount, 
                                 brand_lag, brand_cum) %*% c(0,b))]
  hh_panel[, prob:=expu/sum(expu), by="t"]
  
  grd = rep(0, np+1)
  t_list = unique(hh_panel[, t])
  for (v in t_list){
    dft = hh_panel[t==v, ]
    X = as.matrix(dft[, .(a1, a3, a4, a6, a7, a8, a9, a10, a11, 
                          a13, a14, a15, a17, price, size1_amount, 
                          brand_lag, brand_cum)])
    pvx = dft[, prob] * X
    grd = grd + t(X - matrix(rep(colSums(pvx), nrow(dft)), nrow=nrow(dft), byrow=TRUE)) %*% dft[, purchased]
  }
  grd = grd
  return(-grd[-1])
}

llgn<-function(b) grad(ll, b)


# Negative Hessian function
Hess = function(h){
  dft = hh_panel[hh==h, ]
  ntx = length(unique(dft[,t]))
  rho = ntx/nt
  w = 0.1
  
  llfx = function(b) (1-w)*llfrac(b, dft) + w*rho*ll(b)
  llfng = function(b) grad(llfx, b)
  opt = optim(opt2$par, llfx, llfng, method = c("BFGS"))
  
  return(hessian(llfx, opt$par))
}

# Code up posterior proportions 
pif = function(h, b0, b1, bhat, sig){
  dft = hh_panel[hh==h, ]
  csig = chol(sig)
  return(exp(-llfrac(b1, dft) + lndMvn(b1, bhat, csig) + 
               llfrac(b0, dft) - lndMvn(b0, bhat, csig)))
}

# Function for the Metropolis RW draws
rwmhd <- function(i){
  cov_i = s2 * solve(Hess_list[[i]] + solve(sig))
  beta1 = beta0[i,] + mvrnorm(1, rep(0, np), cov_i)
  alpha = min(1, pif(i,beta0[i,],beta1, bhat, sig))
  if (runif(1) <= alpha) return(beta1) else return(beta0[i,])
}
