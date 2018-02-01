
ll <- function(b1, b2, sig=1, K=KMat, X=XMat, idt=hh_samp){
  # Compute the constant c for each product
  idt[, cons := (1 - K %*% b2)/(expenditure + price)]
  
  # First compute V_k using sparse matrix multiplication
  idt[, eVk := exp((X%*%b1 + (K%*%b2-1) * lexpend - lprice)/sig)]

  # Compute sum of the exponentials
  idt[, esum := 1+sum(eVk), by = c("hh", "t")]
  
  # Collapse trip level
  idt_agg = idt[purchased>=1, .(prob = sum(log(cons))+sum(log(eVk))-sum(log(esum))-log(esum[1])), 
                by = c("hh", "t", "M")]
  idt_agg[, prob := prob - (M)*log(sig) + log(factorial(M))]
  
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


ihessfun <- function(i){
  nti = hh_market_prod[.(i), nt_i][1]
  hh_indv_mod = hh_market_prod[.(i), ]
  XIndv = as.matrix(hh_indv_mod[, xnames, with=FALSE])
  KIndv = hh_indv_mod[, cbind(1-keurig, keurig)]
  
  fll_indv <- function(b, xdt=hh_indv_mod){
    b1 = b[1:nx]
    b2 = b[(nx+1):(nx+nk)]
    b2 = exp(b2)/(1+exp(b2))
    fll = 0.9*ll(b1, b2, X=XIndv, K=KIndv, idt = xdt) + 0.1* nti/nsamp *ll(b1, b2)
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
  xhess = 0.9*ihess + 0.1* nti/nsamp * hess
  #print(paste("Finished Working on Individual ", i, ";", sep=""))
  if (any(eigen(xhess)[[1]]<0)){
    return(nti/nsamp * hess)
  } else{
    return(xhess)
  }
}

# ihessfun <- function (i){
#   nti = hh_market_prod[.(i), nt_i][1]
#   return(nti/nt * hess)
# }

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
  bhat_i = bhat_chunk[as.character(i), ]
  # Obtain root of population variance matrix
  csig = backsolve(chol(sig), diag(np))
  
  # Proposals
  #cov_i = s2 * solve(hess_list[[i]] + solve(sig))
  cov_i = s2 * sig
  orig = unlist(beta_dt[, as.character(i), with=F], recursive = T, use.names = F)
  prop = c(orig + mvrnorm(1, rep(0, np), cov_i))
  
  # Walking step
  ratio = exp(i_ll(prop, i=i)+lndMvn(prop, bhat_i, csig)-i_ll(orig, i=i)-lndMvn(orig, bhat_i, csig))
  alpha = min(1, ratio)
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  if (runif(1) <= alpha) return(prop) else return(orig)
}

rwmhd <- function(i){
  bhat_i = bhat_chunk[as.character(i), ]
  # Obtain root of population variance matrix
  csig = backsolve(chol(sig), diag(np))
  
  # Proposals
  cov_i = 9 * s2 * solve(hess_list[[i]] + solve(sig))
  
  #cov_i = s2 * sig
  orig = unlist(beta_dt[, as.character(i), with=F], recursive = T, use.names = F)
  prop = c(orig + mvrnorm(1, rep(0, np), cov_i))
  
  # Walking step
  ratio = exp(i_ll(prop, i=i)+lndMvn(prop, bhat_i, csig)-i_ll(orig, i=i)-lndMvn(orig, bhat_i, csig))
  alpha = min(1, ratio)
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  if (runif(1) <= alpha) return(prop) else return(orig)
}

# Wrapper for preference estimation
prefrun <- function(){
  .GlobalEnv$bhat_chunk = Z_i %*% Delta
  row.names(.GlobalEnv$bhat_chunk) = hh_list
  if (d>40000){
    dtx = lapply(hh_list, rwmhd)
    beta_dt[, c(as.character(hh_list)) := dtx]
    return(beta_dt)
  } else{
    # Sampl 10% of households 
    hh_list_samp = sort(sample(hh_list, ceiling(length(hh_list)*0.10)))
    dtx = lapply(hh_list_samp, rwmhd)
    beta_dt[, c(as.character(hh_list_samp)) := dtx]
    return(beta_dt[, as.character(hh_list_samp), with = F])
  }
}
