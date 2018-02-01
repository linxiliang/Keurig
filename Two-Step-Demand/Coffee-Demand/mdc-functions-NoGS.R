detfun<-function(v, d){
  nv = length(v)
  if (nv==1){
    return(v-d)
  } else{
    mat = matrix(rep(v, nv), nrow=nv, byrow=T)
    diag(mat) = diag(mat) - d
    return(det(mat))
  }
}

ll <- function(b1, b2, b3, sig=1, K=KMat, X=XMat, idt=hh_samp){
  # Get the correct alpha, bhat
  idt[, `:=`(bz = X%*%b1, alpha = K%*%c(0.98, b2))]
  
  # First compute V_k using sparse matrix multiplication
  idt[, Vk := (bz + (alpha-1) * lexpend - lprice)/sig]
  
  # Compute the constant c for each product
  idt[, cons := (1/alpha)*exp(bz - Vk)*(alpha/price * (size+1)^(alpha-1) -
                                          (alpha-1)/(expenditure+price) * ((size+1)^alpha - 1))]
  idt[, `:=`(d_c = (alpha-1)/(expenditure + price),
             o_i = (1/alpha)*exp(bz - Vk)*((size+1)^(alpha)-1))]
  
  # Compute sum of the exponentials
  idt[, `:=`(esum=exp((1-b3)/b3*log(sum(o_i*purchased)) - log(b3)/b3) + sum(exp(Vk)),
             cons=(1-b3)/(b3*sum(o_i*purchased)) * cons), by = c("hh", "t")]
  
  # Collapse trip level
  idt_agg = idt[purchased>=1, .(prob=log(abs(detfun(cons, d_c)))+(1-b3)/b3*log(sum(o_i))-log(b3)/b3 +  
                                sum(Vk)-sum(log(esum))-log(esum[1])), 
                by = c("hh", "t", "M")]
  idt_agg[, prob := prob - (M)*log(sig) + log(factorial(M))]
  
  # Individual likelihood
  llv = idt_agg[, sum(prob)]
  return(-llv) 
}

ll_homo <- function(b){
  b1 = b[1:nx]
  b2 = b[(nx+1):(nx+nk-1)]
  b3 = b[(nx+nk)]
  b2 = exp(b2)/(1+exp(b2))
  b3 = exp(b3)/(1+exp(b3))
  #sigma = exp(b[length(b)])
  return(ll(b1, b2, b3))
}


ihessfun <- function(i){
  nti = hh_market_prod[.(i), nt_i][1]
  hh_indv_mod = hh_market_prod[.(i), ]
  XIndv = as.matrix(hh_indv_mod[, xnames, with=FALSE])
  KIndv = hh_indv_mod[, cbind(1-keurig, keurig)]
  
  fll_indv <- function(b, xdt=hh_indv_mod){
    b1 = b[1:nx]
    b2 = b[(nx+1):(nx+nk-1)]
    b3 = b[(nx+nk)]
    b2 = exp(b2)/(1+exp(b2))
    b3 = exp(b3)/(1+exp(b3))
    fll = 0.9*ll(b1, b2, b3, K=KIndv, X=XIndv, idt = xdt) + 0.1* nti/nsamp *ll(b1, b2, b3)
    #print(fll)
    return(fll)
  }
  
  ll_indv <- function(b, xdt=hh_indv_mod){
    b1 = b[1:nx]
    b2 = b[(nx+1):(nx+nk-1)]
    b3 = b[(nx+nk)]
    b2 = exp(b2)/(1+exp(b2))
    b3 = exp(b3)/(1+exp(b3))
    return(ll(b1, b2, b3, K=KIndv, X=XIndv, idt = xdt))
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
  b2 = b[(nx+1):(nx+nk-1)]
  b3 = b[(nx+nk)]
  b2 = exp(b2)/(1+exp(b2))
  b3 = exp(b3)/(1+exp(b3))
  hh_indv_mod = hh_market_prod[.(i), ]
  XIndv = as.matrix(hh_indv_mod[, xnames, with=FALSE])
  KIndv = hh_indv_mod[, cbind(1-keurig, keurig)]
  return(-ll(b1, b2, b3, K=KIndv, X=XIndv, idt = hh_indv_mod))
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
  cov_i = s2 * solve(hess_list[[i]] + solve(sig))
  
  #cov_i = s2 * sig
  orig = unlist(beta_dt[, as.character(i), with=F], recursive = T, use.names = F)
  evals = eigen(cov_i)
  if (any(evals$values<0)){
    prop = c(orig + mvrnorm(1, rep(0, np), sig))
  } else{
    prop = c(orig + mvrnorm(1, rep(0, np), cov_i))
  }
  
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
  if (d>tunein){
    hh_list_samp = sort(sample(hh_list, ceiling(length(hh_list)*0.25)))
    dtx = lapply(hh_list_samp, rwmhd)
    beta_dt[, c(as.character(hh_list_samp)) := dtx]
  } else{
    # Sampl 10% of households 
    hh_list_samp = sort(sample(hh_list, ceiling(length(hh_list)*0.10)))
    dtx = lapply(hh_list_samp, rwmhd)
    beta_dt[, c(as.character(hh_list_samp)) := dtx]
  }
  return(beta_dt)
}
