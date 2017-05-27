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
      e[i] =  price[i]*(exp((v1-u0[i])/(alpha[i]-1))-1)
    }
  }
  return(log(rho) + (rho-1)*log(Xifun(e, alpha, psi, price)) + u0[1] + (alpha[1]-1)*log(e1/price[1] + 1) - m0)
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
    # solve for e1
    e1v = uniroot(ovfun, c(0.0001, 10000), m0=m0, rho=rho, alpha=alpha, 
                  u0=u0, price=price, tol=1e-16)
    ufun(e1v$root, m0, rho, alpha, u0, price)
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
    e1v = uniroot(ovfun, c(cmin+0.0000001, cmax), m0=m0, rho=rho, alpha=alpha[1:j], 
                  u0=u0[1:j], price=price[1:j], tol=1e-16)
    # Given e1 compute utility
    ufun(e1v$root, m0, rho, alpha[1:j], u0[1:j], price[1:j])
  }
}

# Simulation to make sure the algorithm works
# n = 100
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