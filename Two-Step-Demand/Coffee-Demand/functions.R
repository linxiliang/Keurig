# Functions for logit estimation
ubar <- function(a, b){
  uimplicit <- function(x)  return(x - exp(a-b*x))
  jacu <- function(x) as.matrix(1 + b*exp(a-b*x))
  xstart = 50
  return(nleqslv(xstart, uimplicit, jacu, method="Broyden", global="none", control=list(btol=.0001))$x)
}

ll_homo<- function(b, dt=hh_market_prod){
  dt[, `:=`(u = cbind(XMat, price_mod) %*% b + size1_amount)]
  dtprod = dt[, .(mu = max(u)), by = c("hh", "t", "brand_descr", "keurig", "roast", "purchased")]
  dtprod[, `:=`(mu = mu - mean(mu)), by = c("hh", "t")]
  dtprod[, prob := exp(mu)/sum(exp(mu)), by = c("hh", "t")]
  llv = -(dtprod[, sum(purchased*log(prob))])
  return(llv)
}

ihessfun <- function (i){
  nti = hh_market_prod[.(i), nt_i][1]
  hh_ind_mod = hh_market_prod[.(i), ]
  XMat_indv =  as.matrix(hh_ind_mod[, c(xnames, "price_mod"), with=FALSE])
  ll_indv <- function(b, dt=hh_ind_mod){
    dt[, `:=`(u = XMat_indv %*% b + size1_amount)]
    dtprod = dt[, .(mu = max(u)), by = c("hh", "t", "brand_descr", "keurig", "roast", "purchased")]
    dtprod[, `:=`(mu = mu - mean(mu)), by = c("hh", "t")]
    dtprod[, prob := exp(mu)/sum(exp(mu)), by = c("hh", "t")]
    llv = -(dtprod[, sum(purchased*log(prob))])
    return(llv)
  }
  ihess = hessian(ll_indv, opt0$par)
  if (any(eigen(ihess)[[1]]<0)){
    return(nti/nt * hess)
  } else{
    return(0.9*ihess + 0.1* nti/nt * hess)
  }
}

ll_linear<- function(b, i=0, dt=dt){
  if (i == 0){
    istart = 1 
    iend = nrow(dt)
  } else{
    istart = dt[1, start]
    iend = dt[1, end]
  }
  ixmat = cbind(XMat[istart:iend, ], log(b[np] - dt[, price]))
  dt[, `:=`(u = ixmat %*% b[1:(np-1)] + size1_amount)]
  dtprod = dt[!is.na(u) & !is.infinite(u), ]
  dtprod = dtprod[, .(mu = max(u)), by = c("hh", "t", "brand_descr", "keurig", 
                                           "flavor", "roast", "purchased")]
  dtprod[, `:=`(mu = mu - mean(mu)), by = c("hh", "t")]
  dtprod[, prob := exp(mu)/sum(exp(mu)), by = c("hh", "t")]
  llv = -(dtprod[, sum(purchased*log(prob))])
  return(llv)
}

ll_simple <- function(b, i=0, dt=dt){
  if (i == 0){
    istart = 1 
    iend = nrow(dt)
  } else{
    istart = dt[1, start]
    iend = dt[1, end]
  }
  dt[, `:=`(u = XMat[istart:iend, ] %*% b + size1_amount)]
  dtprod = dt[!is.na(u) & !is.infinite(u), ]
  dtprod = dtprod[, .(mu = max(u)), by = c("hh", "t", "brand_descr", "keurig", 
                                           "roast", "purchased")]
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
  return(exp(-ll_linear(b1, i=h, dft) + lndMvn(b1x, bhat, csig) + 
               ll_linear(b0, i=h, dft) - lndMvn(b0x, bhat, csig)))
}

pif_simple = function(h, b0, b1, bhat, sig){
  dft = hh_market_prod[.(h), ]
  csig = backsolve(chol(sig),diag(np))
  return(exp(-ll_simple(b1, i=h, dft) + lndMvn(b1, bhat, csig) + 
               ll_simple(b0, i=h, dft) - lndMvn(b0, bhat, csig)))
}


# Function for the Metropolis RW draws
rwmhd_linear <- function(i){
  bhat_i = bhat_all[i, ]
  nti = hh_market_prod[.(i), nt_i][1]
  cov_i = s2 * solve(hess_list[[i]] + solve(sig[1:(np-1), 1:(np-1)]))
  b0 = beta0[i,]
  b1 = c(b0[1:(np-1)] + mvrnorm(1, rep(0, np-1), cov_i), b0[np] + rnorm(1, 0, sqrt(sig[np,np]/nti)))
  b0[np-1] = exp(b0[np-1])
  b1[np-1] = exp(b1[np-1])
  #T1 = T0[i] + rnorm(1, 0, sqrt(Tv/nti))
  #T1 = T0[i]
  if (b1[np] <= max(hh_market_prod[.(i), min_budget])){
    b1[np] = max(hh_market_prod[.(i), min_budget])
  }
  alpha = min(1, pif_linear(i, b0, b1, bhat_i, sig))
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  b1[np-1] = log(b1[np-1])
  b0[np-1] = log(b0[np-1])
  if (runif(1) <= alpha) return(b1) else return(b0)
}

rwmhd_simple <- function(i){
  bhat_i = bhat_all[i, ]
  nti = hh_market_prod[.(i), nt_i][1]
  cov_i = s2 * solve(hess_list[[i]] + solve(sig))
  b0 = beta0[i,]
  b1 = c(b0 + mvrnorm(1, rep(0, np), cov_i))
  alpha = min(1, pif_simple(i, b0, b1, bhat_i, sig))
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  if (runif(1) <= alpha) return(b1) else return(b0)
}

