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

