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


# Solve the optimization problem using iterative elimination
eu2rev <- function(umin, umax, alpha, zb, p, eps, E){
  if (length(p)==1){
    return(E)
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
    Spent[-(ind0)] = 0
    return(Spent)
  }
}

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
mc_vfun<-function(zb1, zb2, zb3, alpha, rho, price, keurig, owned, licensed, thirdp, n){
  vfun_i <- function(d, rho, zb1, zb2, zb3, alpha, price, keurig, owned, licensed, thirdp){
    epsx=-log(-log(runif(length(zb1))))
    u1 = zb1 + epsx
    u2 = zb2 + epsx
    u3 = zb3 + epsx
    m0 = rho * (-log(-log(runif(length(1)))))
    v1 = vfun(m0, rho, alpha, u1, price)
    if (sum(owned+(1-keurig))==0){
      v2 = 0
    } else{
      gindx = which(((keurig==0)|(owned==1)))
      v2 = vfun(m0, rho, alpha[gindx], u2[gindx], price[gindx])
    }
    if (sum(owned+licensed+(1-keurig))==0){
      v3 = 0
    } else{
      gindx = which(((keurig==0)|(owned==1)|(licensed==1)))
      v3 = vfun(m0, rho, alpha[gindx], u3[gindx], price[gindx])
    }
    if (sum(1-keurig)==0){
      v4 = 0 
    } else{
      gindx = which(keurig==0)
      v4 = vfun(m0, rho, alpha[gindx], u1[gindx], price[gindx])
    }
    return(c(v1,v2,v3,v4))
  }
  v_mc = sapply(1:n, vfun_i, rho=rho[1], zb1=zb1, zb2=zb2, zb3=zb3, alpha=alpha, 
                price=price, keurig=keurig, owned=owned, licensed=licensed, thirdp=thirdp)
  return(rowSums(v_mc)/n)
}

ufun_ev <- function(e1, m0, rho, alpha, u0, price){
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
  return(e)
}

vfun_ev <- function(m0, rho, alpha, u0, price){
  if (length(u0)==1){
    # solve for e1
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
    return(e1v$root)
  } else{
    norder = order(u0, decreasing = T)
    oorder = order(norder)
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
      return(rep(0, nK))
    } else if (cmaxv>0){
      return(rep(0, nK))
    }
    e1v = uniroot(cvfun, c(cmin, cmax), m0=m0, rho=rho, alpha=alpha[1:j], 
                  u0=u0[1:j], price=price[1:j], tol=1e-18)
    # Given e1 compute all expenditure
    e_v = ufun_ev(e1v$root, m0, rho, alpha[1:j], u0[1:j], price[1:j])
    if (nK!=j) e_v = c(e_v, rep(0, (nK-j)))
    return(e_v[oorder])
  }
}

# MC revenue fun calculation
mc_rfun<-function(zb1, zb2, zb3, alpha, rho, price, keurig, owned, licensed, thirdp, n){
  rfun_i <- function(d, rho, zb1, zb2, zb3, alpha, price, keurig, owned, licensed, thirdp){
    epsx=-log(-log(runif(length(zb1))))
    u1 = zb1 + epsx
    u2 = zb2 + epsx
    u3 = zb3 + epsx
    m0 = rho * (-log(-log(runif(length(1)))))
    v1 = vfun_ev(m0, rho, alpha, u1, price)
    v2 = rep(0, length(zb1))
    v3 = rep(0, length(zb1))
    v4 = rep(0, length(zb1))
    if (sum(owned+(1-keurig))!=0){
      gindx = which(((keurig==0)|(owned==1)))
      v2[gindx] = vfun_ev(m0, rho, alpha[gindx], u2[gindx], price[gindx])
    }
    if (sum(owned+licensed+(1-keurig))!=0){
      gindx = which(((keurig==0)|(owned==1)|(licensed==1)))
      v3[gindx] = vfun_ev(m0, rho, alpha[gindx], u3[gindx], price[gindx])
    }
    if (sum(1-keurig)!=0){
      gindx = which(keurig==0)
      v4[gindx] = vfun_ev(m0, rho, alpha[gindx], u1[gindx], price[gindx])
    }
    return(c(v1,v2,v3,v4))
  }
  v_mc = sapply(1:n, rfun_i, rho=rho[1], zb1=zb1, zb2=zb2, zb3=zb3, alpha=alpha, 
                price=price, keurig=keurig, owned=owned, licensed=licensed, thirdp=thirdp)
  v_mc = rowSums(v_mc)/n
  v_mc = matrix(v_mc, nrow=length(zb1))
  v_mc = lapply(seq_len(ncol(v_mc)), function(i) v_mc[,i])
  return(v_mc)
}

# Welfare Functions
cppcvfun <- function(m0, rho, alpha, u0, price){
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
    return(ccvfun(e1v$root, m0, rho, alpha, u0, price))
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
    return(ccvfun(e1v$root, m0, rho, alpha[1:j], u0[1:j], price[1:j]))
  }
}

# MC CV calculation
mc_cvfun<-function(zb1, zb2, zb3, alpha, rho, price, keurig, owned, licensed, thirdp, n){
  cppcvfun_i <- function(d, rho, zb1, zb2, zb3, alpha, price, keurig, owned, licensed, thirdp){
    epsx=-log(-log(runif(length(zb1))))
    u1 = zb1 + epsx
    u2 = zb2 + epsx
    u3 = zb3 + epsx
    m0 = rho * (-log(-log(runif(length(1)))))
    v1 = cppcvfun(m0, rho, alpha, u1, price)
    if (sum(owned+(1-keurig))==0){
      v2 = 0
    } else{
      gindx = which(((keurig==0)|(owned==1)))
      v2 = cppcvfun(m0, rho, alpha[gindx], u2[gindx], price[gindx])
    }
    if (sum(owned+licensed+(1-keurig))==0){
      v3 = 0
    } else{
      gindx = which(((keurig==0)|(owned==1)|(licensed==1)))
      v3 = cppcvfun(m0, rho, alpha[gindx], u3[gindx], price[gindx])
    }
    if (sum(1-keurig)==0){
      v4 = 0 
    } else{
      gindx = which(keurig==0)
      v4 = cppcvfun(m0, rho, alpha[gindx], u1[gindx], price[gindx])
    }
    return(c(v1,v2,v3,v4))
  }
  v_mc = sapply(1:n, cppcvfun_i, rho=rho[1], zb1=zb1, zb2=zb2, zb3=zb3, alpha=alpha, 
                price=price, keurig=keurig, owned=owned, licensed=licensed, thirdp=thirdp)
  return(rowSums(v_mc)/n)
}
