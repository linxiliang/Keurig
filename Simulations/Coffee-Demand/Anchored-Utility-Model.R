# 
library(data.table)
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

ll <- function(b1, b2, b3, sig=1, K=KMat, X=XMat, idt=panel){
  # Get the correct alpha, bhat
  idt[, `:=`(bz = X%*%b1, alpha = K%*%b2)]
  
  # First compute V_k using sparse matrix multiplication
  idt[, Vk := (bz + (alpha-1) * lexpend - lprice)/sig]
  
  # Compute the constant c for each product
  idt[, cons := (1/alpha)*exp(bz - Vk)*(alpha/price * (size+1)^(alpha-1) -
                                          (alpha-1)/(expenditure+price) * ((size+1)^alpha - 1))]
  idt[, `:=`(d_c = (alpha-1)/(expenditure + price),
             o_i = (1/alpha)*exp(bz - Vk)*((size+1)^(alpha)-1))]
  
  # Compute sum of the exponentials
  idt[, `:=`(esum=exp((1-b3)/b3*log(sum(o_i*purchased)) - log(b3)/b3) + sum(exp(Vk)),
             cons=(1-b3)/(b3*sum(o_i*purchased)) * cons), by = c("t")]
  
  # Collapse trip level
  idt_agg = idt[purchased>=1, .(prob=log(abs(detfun(cons, d_c)))+(1-b3)/b3*log(sum(o_i))-log(b3)/b3 +  
                                  sum(Vk)-sum(log(esum))-log(esum[1])), 
                by = c("t", "M")]
  idt_agg[, prob := prob - (M)*log(sig) + log(factorial(M))]
  
  # Individual likelihood
  llv = idt_agg[, sum(prob)]
  return(-llv) 
}

ll_homo <- function(b){
  b1 = b[1:nx]
  b2 = b[(nx+1):(nx+nk)]
  b3 = b[(nx+nk+1)]
  b2 = exp(b2)/(1+exp(b2))
  b3 = exp(b3)/(1+exp(b3))
  #sigma = exp(b[length(b)])
  return(ll(b1, b2, b3))
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
    return(ufun(e1v$root, m0, rho, alpha[1:j], u0[1:j], price[1:j]))
  }
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
    e1v = uniroot(ovfun, c(0.0001, 10000), m0=m0, rho=rho, alpha=alpha, 
                  u0=u0, price=price, tol=1e-16)
    return(e1v)
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
      if (e[1]>1e20) cmax=1e20
      zeta = log(rho) + (rho-1)*log(Xifun(e, alpha, psi, price)) + u0[1] + (alpha[1]-1)*log(e[1]/price[1] + 1)
    }
    # solve for e1
    print(c(cmin,cmax))
    e1v = uniroot(ovfun, c(cmin+0.0000001, cmax), m0=m0, rho=rho, alpha=alpha[1:j], 
                  u0=u0[1:j], price=price[1:j], tol=1e-16)
    # Given e1 compute all expenditure
    e_v = ufun_ev(e1v$root, m0, rho, alpha[1:j], u0[1:j], price[1:j])
    e_v = c(e_v, rep(0, (nK-j)))
    return(e_v[oorder])
  }
}

# optimization approach
Uf <- function(e) -(sum(psi/alpha * ((e/price+1)^(alpha)-1)))^rho + exp(m0)*sum(e)

# Replications and simulations
nprod = 5
nt = 2000
n = nprod*nt
zx = rnorm(nprod)
keurig = c(rep(1, ceiling(nprod/2)), rep(0, (nprod-ceiling(nprod/2))))
alpha = c(2, 1.7)
alpha = cbind(1-keurig, keurig)%*%alpha
rho = -0.2

# Names
bnames = paste0("a", c(1:nprod))
xnames = c(bnames)

# Estimate dimensions
nx = length(xnames)
nk = 2
np = nx + nk + 1

nsim = 100
EB = matrix(rep(0, np*nsim), ncol=np)
for (k in 1:nsim){
  
  # Create the dataset
  panel = data.table(expand.grid(t = 1:nt, brand = 1:nprod))
  setkey(panel, t, brand)
  
  # Create brand dummies and associated variables
  for (i in 1:nprod){
    panel[, dvar := as.integer(brand==i)]
    setnames(panel, "dvar", paste0("a", i))
  }
  panel[, `:=`(keurig=rep(keurig, nt), price=runif(n), eps=-log(-log(runif(n))), zv=rep(zx, nt), 
               alpha=rep(alpha, nt), rho=exp(rho)/(1+exp(rho)))]
  panel[, `:=`(alpha=exp(alpha)/(1+exp(alpha)), lprice=log(price))]
  panel[, eps0 := rep(-log(-log(runif(1))), .N), by = c("t")]
  panel[, eps0 := rho*eps0]
  
  # Simulate the expenditure
  panel[, u0:=zv-log(price)+eps]
  for (t in 1:nt){
    panel[.(1), vfun(eps0[1], rho[1], alpha, u0, price)]
  }
  panel[, expenditure:=vfun_ev(eps0[1], rho[1], alpha, u0, price), by = c("t")]
  panel[, `:=`(size=expenditure/price, purchased=as.integer(expenditure>0), lexpend=log(expenditure/price+1))]
  panel[, `:=`(M = sum(purchased)), by = c("t")]
  
  XMat = as.matrix(panel[, xnames, with=FALSE])
  KMat = panel[, cbind(keurig, 1-keurig)]
  
  b0 = rnorm(np)
  opt0 = optim(b0, ll_homo, gr = NULL, method = c("BFGS"), control = list(reltol=1e-16))
  
  EB[k,] = opt0$par
}
