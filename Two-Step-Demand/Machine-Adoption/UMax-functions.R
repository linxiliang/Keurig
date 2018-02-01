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
  if (length(p)==1){
    return(log(1/alpha * exp(zb + eps) * ((E/p + 1)^alpha-1)))
  } else{
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
                    eps = eps,
                    E = E)
    return(log(-res0$objective))
  }
} 

# Solve Lambda
BC<-function(lambda, alpha, zb, p, eps, E) return(sum(p*(exp((log(lambda)+log(p)-zb-eps)/(alpha-1))-1))-E)

# Obtain the mariginal utility at each product
EXPEND<-function(lambda, alpha, zb, p, eps, E) return(p*(exp((log(lambda)+log(p)-zb-eps)/(alpha-1))-1))

# Solve the optimization problem using iterative elimination 
eu2 <- function(umin, umax, alpha, zb, p, eps, E){
  if (length(p)==1){
    return(log(1/alpha * exp(zb + eps) * ((E/p + 1)^alpha-1)))
  } else{
    n0 = length(p)
    ind0 = seq(1, n0)
    dfx = 10
    while(dfx>0){
      kr = uniroot(BC, c(umin, umax), alpha = alpha[ind0], 
                   zb = zb[ind0], p = p[ind0], eps = eps[ind0], E = E, tol=1e-16)
      Spent = EXPEND(kr$root, alpha, zb, p, eps, E)
      ind0 = which(Spent>0)
      n1 = length(ind0)
      if (n1==1){
        dfx=0
      } else{
        dfx=n0-n1
        n0=n1
      }
    }
    if (n1==1){
      return(log(1/alpha[ind0] * exp(zb[ind0] + eps[ind0]) * ((E/p[ind0] + 1)^alpha[ind0]-1)))
    } else{
      return(log(sum(1/alpha[ind0] * exp(zb[ind0] + eps[ind0]) * 
                       ((Spent[ind0]/p[ind0] + 1)^alpha[ind0]-1))))
    }
  }
}


# Solve the optimization problem using iterative elimination
eu2 <- function(umin, umax, alpha, zb, p, eps, E){
  if (length(p)==1){
    return((1/alpha * exp(zb + eps) * ((E/p + 1)^alpha-1)))
  } else{
    n0 = length(p)
    ind0 = seq(1, n0)
    dfx = 10
    while(dfx>0){
      kr = uniroot(BC, c(umin, umax), alpha = alpha[ind0],
                   zb = zb[ind0], p = p[ind0], eps = eps[ind0], E = E, tol=1e-16)
      Spent = EXPEND(kr$root, alpha, zb, p, eps, E)
      ind0 = which(Spent>0)
      n1 = length(ind0)
      if (n1==1){
        dfx=0
      } else{
        dfx=n0-n1
        n0=n1
      }
    }
    if (n1==1){
      return(1/alpha[ind0] * exp(zb[ind0] + eps[ind0]) * ((E/p[ind0] + 1)^alpha[ind0]-1))
    } else{
      return(sum(1/alpha[ind0] * exp(zb[ind0] + eps[ind0]) *
                   ((Spent[ind0]/p[ind0] + 1)^alpha[ind0]-1)))
    }
  }
}

# Solve the optimization using my algorithm
Xifun <- function(e, alpha, psi, price) sum(psi/alpha * ((e/price+1)^(alpha)-1))

ovfun <- function(e1, m0, rho, alpha, u0, price){
  n_c = length(alpha)
  psi = exp(u0 + log(price))
  if (n_c<=1){
    e = e1
  } else{
    e = rep(0, n_c)
    e[1] = e1
    v1 = u0[1]+(alpha[1]-1)*log(e[1]/price[1]+1)
    for (i in 2:n_c){
      e[i] = price[i]*(exp((v1-u0[i])/(alpha[i]-1))-1)
    }
  }
  #print("eval")
  #print(e)
  return(log(rho) + (rho-1)*log(Xifun(e, alpha, psi, price)) + u0[1] + 
           (alpha[1]-1)*log(e1/price[1] + 1) - m0)
}

ufun <- function(e1, m0, rho, alpha, u0, price){
  n_c = length(alpha)
  psi = exp(u0 + log(price))
  if (n_c<=1){
    e = e1
  } else{
    e = rep(e1, n_c)
    v1 = u0[1]+(alpha[1]-1)*log(e[1]/price[1]+1)
    for (i in 2:n_c){
      e[i] =  price[i]*(exp((v1-u0[i])/(alpha[i]-1))-1)
    }
  }
  return((Xifun(e, alpha, psi, price))^rho-exp(m0)*sum(e))
}

vfun <- function(m0, rho, alpha, u0, price){
  if (length(u0)==1){
    cmin = 1e-8
    cmax = 1e12
    cminv = cvfun(cmin, m0=m0, rho=rho, alpha=alpha, u0=u0, price=price)
    cmaxv = cvfun(cmax, m0=m0, rho=rho, alpha=alpha, u0=u0, price=price)
    #print(cbind(cmin, cmax))
    #print(cbind(cminv, cmaxv))
    if (cminv<0) {
      return(0)
    } else if (cmaxv>0){
      return(0)
    }
    # solve for e1
    e1v = uniroot(cvfun, c(cmin, cmax), m0=m0, rho=rho, alpha=alpha, 
                  u0=u0, price=price, tol=1e-16)
    return(cufun(e1v$root, m0, rho, alpha, u0, price))
  } else{
    norder = order(u0, decreasing = T)
    nK = length(norder)
    alpha = alpha[norder]
    price = price[norder]
    u0 = u0[norder]
    psi = exp(u0 + log(price))
    zeta = m0 + 1
    e = rep(0, nK)
    k = 1
    while (zeta>m0 & k<nK){
      k = k+1
      cmin = e[1]
      for (j in 1:(k-1)){
        e[j] = price[j] * (exp((u0[k]-u0[j])/(alpha[j]-1))-1) 
      }
      cmax = e[1]
      zeta = log(rho) + (rho-1)*log(Xifun(e, alpha, psi, price)) + u0[1] + (alpha[1]-1)*log(e[1]/price[1] + 1)
    }
    # solve for e1
    cmin = cmin+1e-8
    if (cmax>1e12) {cmax=1e12}
    
    if (k==nK & zeta>m0) {
      cmin=e[1] + 1e-8
      cmax=1e12
      j = j+1
    }
    # Outliers as 0
    cminv = cvfun(cmin, m0=m0, rho=rho, alpha=alpha[1:j], u0=u0[1:j], price=price[1:j])
    cmaxv = cvfun(cmax, m0=m0, rho=rho, alpha=alpha[1:j], u0=u0[1:j], price=price[1:j])
    #print(cbind(cmin, cmax))
    #print(cbind(cminv, cmaxv))
    if (cminv<0) {
      return(0)
    } else if (cmaxv>0){
      return(0)
    }
    e1v = uniroot(cvfun, c(cmin, cmax), m0=m0, rho=rho, alpha=alpha[1:j], 
                  u0=u0[1:j], price=price[1:j], tol=1e-18)
    # Given e1 compute utility
    return(cufun(e1v$root, m0, rho, alpha[1:j], u0[1:j], price[1:j]))
  }
}

# MC ufun calculation
mc_vfun<-function(zb, alpha, rho, price, keurig, n){
  vfun_i <- function(d, rho, zb, alpha, price, keurig){
    u0 = zb - log(-log(runif(length(zb))))
    m0 = rho * (-log(-log(runif(length(1)))))
    v1 = vfun(m0, rho, alpha, u0, price)
    if (sum(1-keurig)==0){
     v2 = 0 
    } else{
     gindx = which(keurig==0)
     v2 = vfun(m0, rho, alpha[gindx], u0[gindx], price[gindx])
    }
    return(c(v1,v2))
  }
  v_mc = sapply(1:n, vfun_i, rho=rho[1], zb=zb, alpha=alpha, price=price, keurig=keurig)
  return(rowSums(v_mc)/n)
}

# # Simulation to make sure the algorithm works
# n = 40
# u0 = rnorm(n)
# price = runif(n)
# psi = exp(u0 + log(price))
# alpha = runif(n, min = 0.5, max = 0.95)
# rho = 0.6
# m0 = 0.6*0.2
# vfun(m0, rho, alpha, u0, price)
# 
# # optimization approach
# Uf <- function(e) -(sum(psi/alpha * ((e/price+1)^(alpha)-1)))^rho + exp(m0)*sum(e)
# 
# kopt = optim(rep(0.1, 100), Uf, method = "L-BFGS-B", lower=rep(0,100))
# kopt$value
# 
# system.time(replicate(10, optim(rep(0.1, 100), Uf, method = "L-BFGS-B", lower=rep(0,100))))
# system.time(replicate(10, vfun(m0, rho, alpha, u0, price)))