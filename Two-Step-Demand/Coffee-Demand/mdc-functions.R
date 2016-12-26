
ll <- function(b1, b2, sig=1, K=KMat, X=XMat, idt=hh_market_prod){
  # Compute the constant c for each product
  idt[, cons := (1 - K %*% b2)/(expenditure + price)]
  
  # First compute V_k using sparse matrix multiplication
  idt[, eVk := exp((X%*%b1 + (K%*%b2-1) * lexpend - lprice)/sig)]

  # Compute sum of the exponentials
  idt[, esum := sum(eVk), by = c("hh", "t")]
  
  # Collapse trip level
  idt_agg = idt[purchased>=1, .(prob = sum(log(cons))+log(sum(1/cons))+sum(log(eVk))-sum(log(esum))), 
                by = c("hh", "t", "M")]
  idt_agg[, prob := prob - (M-1)*log(sig) + log(factorial(M-1))]
  
  # Individual likelihood
  llv = idt_agg[, sum(prob)]
  return(-llv)
}

ll_homo <- function(b){
  b1 = b[1:nx]
  b2 = b[(nx+1):(nx+nk)]
  b2 = exp(b2)/(1+exp(b2))
  #sigma = exp(b[length(b)])
  return(ll(b1, b2))
}


ihessfun <- function (i){
  nti = hh_market_prod[.(i), nt_i][1]
  hh_indv_mod = hh_market_prod[.(i), ]
  XIndv = as.matrix(hh_indv_mod[, xnames, with=FALSE])
  KIndv = hh_indv_mod[, cbind(1-keurig, keurig)]
  
  fll_indv <- function(b, xdt=hh_indv_mod){
    b1 = b[1:nx]
    b2 = b[(nx+1):(nx+nk)]
    b2 = exp(b2)/(1+exp(b2))
    fll = 0.9*ll(b1, b2, X=XIndv, K=KIndv, idt = xdt) + 0.1* nti/nt *ll(b1, b2)
    #print(fll)
    return(fll)
  }
  
  ll_indv <- function(b, xdt=hh_indv_mod){
    b1 = b[1:nx]
    b2 = b[(nx+1):(nx+nk)]
    b2 = exp(b2)/(1+exp(b2))
    return(ll(b1, b2, X=XIndv, K=KIndv, idt = xdt))
  }
  
  iopt = optim(opt0$par, fll_indv, method = c("Nelder-Mead"), 
               control = list(maxit = 100, reltol=1e-4))
  ihess = hessian(ll_indv, iopt$par)
  xhess = 0.9*ihess + 0.1* nti/nt * hess
  #print(paste("Finished Working on Individual ", i, ";", sep=""))
  if (any(eigen(xhess)[[1]]<0)){
    return(nti/nt * hess)
  } else{
    return(xhess)
  }
}

ihessfun <- function (i){
  nti = hh_market_prod[.(i), nt_i][1]
  return(nti/nt * hess)
}

i_ll <- function(b, i=1){
  b1 = b[1:nx]
  b2 = b[(nx+1):(nx+nk)]
  b2 = exp(b2)/(1+exp(b2))
  hh_indv_mod = hh_market_prod[.(i), ]
  XIndv = as.matrix(hh_indv_mod[, xnames, with=FALSE])
  KIndv = hh_indv_mod[, cbind(1-keurig, keurig)]
  return(-ll(b1, b2, X=XIndv, K=KIndv, idt = hh_indv_mod))
}


rwmhd_iid <- function(i){
  # Obtain root of population variance matrix
  csig = backsolve(chol(sig), diag(np))
  
  # Proposals
  #cov_i = s2 * solve(hess_list[[i]] + solve(sig))
  cov_i = s2 * sig
  orig = beta0[i,]
  prop = c(orig + mvrnorm(1, rep(0, np), cov_i))
  
  # Walking step
  ratio = exp(i_ll(prop, i=i)+lndMvn(prop, bhat, csig)-i_ll(orig, i=i)-lndMvn(orig, bhat, csig))
  alpha = min(1, ratio)
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  if (runif(1) <= alpha) return(prop) else return(orig)
}

rwmhd <- function(i){
  # Obtain root of population variance matrix
  csig = backsolve(chol(sig), diag(np))
  
  # Proposals
  cov_i = s2 * solve(hess_list[[i]] + solve(sig))
  
  #cov_i = s2 * sig
  orig = beta0[i,]
  prop = c(orig + mvrnorm(1, rep(0, np), cov_i))
  
  # Walking step
  ratio = exp(i_ll(prop, i=i)+lndMvn(prop, bhat, csig)-i_ll(orig, i=i)-lndMvn(orig, bhat, csig))
  alpha = min(1, ratio)
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  if (runif(1) <= alpha) return(prop) else return(orig)
}

# objective function
eval_f0 <- function(ex, alpha, zb, p, eps, E){
  uu = -1/alpha * exp(zb + eps) * ((ex/p + 1)^alpha-1)
  return(sum(uu))
}

# constraint function
eval_g0 <- function(ex, alpha, zb, p, eps, E){
  return(sum(ex)-E)
}

# gradient of objective function
eval_grad_f0 <- function(ex, alpha, zb, p, eps, E) {
  return(- 1/p * exp(zb + eps)*(ex/p+1)^(alpha-1))
}

# jacobian of constraint
eval_jac_g0 <- function(ex, alpha, zb, p, eps, E) {
  return(rep(1, length(ex)))
}

# log transformation
eval_log_f0 <- function(ex, alpha, zb, p, eps, E){
  uu = 1/alpha * exp(zb + eps) * ((ex/p + 1)^alpha-1)
  return(log(sum(uu)))
}

# Obtain the optimal utility
# Solve using NLOPT_LD_MMA with gradient information supplied in separate function
eu <- function(alpha, zb, p, eps, E){
  # Bounds 
  e_lb = rep(0, length(zb))
  e_ub = rep(E, length(zb))
  
  res0 <- nloptr( x0=rep(0, length(eps)), 
                  eval_f=eval_f0, 
                  eval_grad_f=eval_grad_f0,
                  lb = e_lb, 
                  ub = e_ub, 
                  eval_g_ineq = eval_g0,
                  eval_jac_g_ineq = eval_jac_g0,                
                  opts = list("algorithm"="NLOPT_LD_MMA", xtol_rel = 1e-16, ftol_abs = 0),
                  alpha = alpha,
                  zb = zb, 
                  p = p,
                  eps = esp,
                  E = E)
  return(log(-res0$objective))
} 

