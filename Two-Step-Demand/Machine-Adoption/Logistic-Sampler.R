# Bayesian Estimation
# Obtain Hess list
hessfun <- function(){
  .GlobalEnv$hess_list = NULL
  for (i in id_list){
    nid = length(Data_Chunk[[as.character(i)]]$y)
    .GlobalEnv$hess_list[[as.character(i)]] = - nid/nsamp * hess
  }
}

# MH Sampler
rwmhd <- function(i){
  i_c = as.character(i)
  bhat_i = bhat_chunk[i_c, ]
  # Obtain root of population variance matrix
  csig = backsolve(chol(sig), diag(np))

  # Tune lambda
  ac_dt[.(i), acrate:=nac/nd]
  if (ac_dt[.(i), acrate]>0.45&ac_dt[.(i), nd]>=50) ac_dt[.(i), `:=`(lambda=1.10*lambda, nd=0, nac=0)]
  if (ac_dt[.(i), acrate]<0.15&ac_dt[.(i), nd]>=50) ac_dt[.(i), `:=`(lambda=0.90*lambda, nd=0, nac=0)]
  
  # Proposals
  cov_i = s2 * chol2inv(chol(hess_list[[i_c]] + chol2inv(chol(sig))))
  orig = unlist(beta_dt[, i_c, with=F], recursive = T, use.names = F)
  evals = eigen(cov_i)
  if (any(evals$values<0)){
    prop = orig + ac_dt[.(i), lambda]*mvrnorm(1, rep(0, np), sig)
  } else{
    prop = orig + ac_dt[.(i), lambda]*mvrnorm(1, rep(0, np), cov_i)
  }
  
  # Walking step
  ratio = exp(loglike(prop, Data_Chunk[[i_c]]$y, Data_Chunk[[i_c]]$X)+lndMvn(prop, bhat_i, csig)-
                loglike(orig, Data_Chunk[[i_c]]$y, Data_Chunk[[i_c]]$X)-lndMvn(orig, bhat_i, csig))
  alpha = min(1, ratio)
  alpha = ifelse(is.na(alpha), 0, alpha) # Outlier should have no chance of being accepted.
  ac_dt[.(i), nd:=nd+1]
  if (runif(1) <= alpha) {
    ac_dt[.(i), nac:=nac+1]
    return(prop) 
  } else {
    return(orig)
  }
}

# Wrapper for preference estimation
prefrun <- function(){
  .GlobalEnv$bhat_chunk = Z_Chunk %*% Delta
  row.names(.GlobalEnv$bhat_chunk) = id_list
  dtx = lapply(id_list, rwmhd)
  beta_dt[, c(as.character(id_list)) := dtx]
  return(beta_dt)
}

BinaryLogitSampler = function(Data, ZD=NULL, Names, MCMC, Par){
  # Data store data
  # ZV stores the relevant demographics info
  # Names store the labels of X, Y, and Groups.
  # MCMC stores the settings on the number of draws, thins etc. 
  # Par stores information on parallel computation
  clusterExport(Par$cl, c("hessfun", "rwmhd", "prefrun"))
  ncl = length(Par$cl)
  setkeyv(Data, Names$id)
  setkeyv(ZD, Names$id)
  full_id_list = unique(Data[, Names$id, with=FALSE])[[1]]
  nhsize = ceiling(length(full_id_list)/ncl)
  nh = length(full_id_list)
  np = length(Names$xnames) + 1
  nz = length(Names$znames) + 1
  nobs = nrow(Data)
  
  # Put each chunk of data to the data.table
  for (i in 1:ncl){
    if (i==ncl){
      id_list = sort(full_id_list)
    } else{
      id_list = full_id_list[1:nhsize]
    }
    full_id_list = setdiff(full_id_list, id_list)
    Data_Chunk = NULL
    for (j in id_list){
      Data_Chunk[[as.character(j)]] = list(X = cbind(1, as.matrix(Data[.(j), Names$xnames, with = F])), 
                                           y =  Data[.(j), Names$yname, with = F][[1]])
    }
    Z_Chunk = cbind(1, as.matrix(ZD[.(id_list), Names$znames, with = F]))
    clusterExport(Par$cl[i], c('id_list', 'Data_Chunk', 'Z_Chunk'))
  }
  
  # Demographics Matrix
  Z = cbind(1, as.matrix(ZD[, Names$znames, with=FALSE]))
  
  # Prior settings
  nu0 = max(c(4, 0.01*nh, np))
  V0 = nu0 * diag(np);
  sig0 = rwishart(nu0, solve(V0))$IW;
  A = nu0 * diag(diag(var(Z)));
  Dbar = matrix(rep(0, nz*np), nrow=nz)
  s2 = 2.93^2/np;
  
  # Tuning
  nsamp = ceiling(MCMC$sample_ratio * nobs)
  sample_obs = sort(sample(1:nobs, nsamp))
  sample_data = Data[sample_obs, ]
  rform = formula(paste(Names$yname, paste0(Names$xnames, collapse="+"), sep = "~"))
  breg = glm(rform, family = binomial(link='logit'), data = sample_data)
  par0 = breg$coefficients
  hess = -solve(vcov(breg))
  clusterExport(Par$cl, c("par0", "hess", "nsamp", 'np', 's2'))
  invisible(clusterEvalQ(Par$cl, hessfun()))
  
  #
  invisible(clusterEvalQ(Par$cl, (beta_dt=data.table(par0 + matrix(c(rnorm(length(id_list)*(np-1), mean=0, sd=0.1),
                                                                          rnorm(length(id_list), mean=0, sd=0.001)),
                                                                        ncol=length(id_list), byrow=T)))))
  invisible(clusterEvalQ(Par$cl, setnames(beta_dt, names(beta_dt), as.character(id_list))))
  beta0 = clusterEvalQ(Par$cl, beta_dt)
  bhnames = as.integer(unlist(lapply(beta0, names)))
  beta0 = matrix(t(unlist(beta0)), ncol=np, byrow=T)
  beta0 = beta0[order(bhnames), ]
  
  # Intialize the tuning dataset
  invisible(clusterEvalQ(Par$cl, (ac_dt=data.table(hh=id_list))))
  invisible(clusterEvalQ(Par$cl, ac_dt[, `:=`(nd=0,nac=0,lambda=1)]))
  invisible(clusterEvalQ(Par$cl, setkey(ac_dt, hh)))
  
  #Initialize storage of MCMC draws
  bhatd = matrix(rep(0, MCMC$R*np), nrow=MCMC$R)
  deltad = matrix(rep(0, MCMC$R*(np*nz)), nrow=MCMC$R)
  sigd = array(0, dim=c(MCMC$R, np, np))
  bindv = array(0, dim=c(MCMC$R, nh, np))
  gc()
  
  # Draws
  ## Gibbs Sampling
  start_time = proc.time()[3]
  for (d in 1:(MCMC$R*MCMC$thin)){
    Dhat  = solve(t(Z) %*% Z) %*% t(Z) %*% beta0
    Dtild = solve(t(Z) %*% Z + A) %*% (t(Z) %*% Z %*% Dhat + A %*% Dbar)     
    S     = t(beta0 - Z %*% Dtild) %*% (beta0 - Z %*% Dtild) + t(Dtild - Dbar) %*% A %*% (Dtild - Dbar)
    
    # Draw from the posterior
    sig  = rwishart(nu0+nh, solve(V0+S))$IW
    delta = mvrnorm(1, as.vector(Dtild), kronecker(sig, solve(t(Z)%*%Z + A)))
    Delta = matrix(delta, nrow=nz)
    bhat = colMeans(Z %*% Delta)
    print(bhat)
    
    # Store the posterior draws of mean betas and sigmas
    if (d %% MCMC$thin == 0){
      indx = d/MCMC$thin
      bhatd[indx,] = bhat; 
      deltad[indx,] = delta;
      sigd[indx,,] = sig;
      bnorder = order(bhnames)
      bindv[indx, ,] = beta0[bnorder, ]
    }
    
    # Parallel version
    clusterExport(Par$cl, c('sig', 'Delta', 'd'))
    beta_list = clusterEvalQ(Par$cl, prefrun())
    bhnames = as.integer(unlist(lapply(beta_list, names)))
    beta0 = matrix(unlist(beta_list), ncol=np, byrow=T)
    beta0 = beta0[order(bhnames), ]
    
    cat("Finished drawing", d, "out of", MCMC$R*MCMC$thin, ", and total time elapsed:", 
        proc.time()[3] - start_time, "\n\n")
  }
  return(list(bhatd=bhatd, deltad=deltad, sigd=sigd, bindv=bindv))
}
