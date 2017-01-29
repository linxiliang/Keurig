 T(i, x) = cos((i-1) .* acos(x));

 # Functions
 function capprox(w, x::Real)
     length(w) == nodes ? length(w) : error("function values not the same length as interpolation nodes")
     Wmat = repmat(w, 1, (order+1)); # Step 3 - function value at transformed node
     ccoef = diag(Tmat * Wmat) ./ sum(Tmat.^2,2)[:,1]; #Step 4 Compute coefficients
     if (a<=x<=b)
         return sum(Float64[T(i, 2 * (x-a)/(b-a) - 1) * ccoef[i] for i = 1:(order+1)]);
     elseif (x<a)
         abase = capprox(w, a)
         aslope = (capprox(w, a+0.0001) - abase)/0.0001
         return abase + aslope * (x-a) # Linear Extrapolation
     else
         bbase = capprox(w, b)
         bslope = (bbase - capprox(w, b - 0.0001))/0.0001
         return bbase + bslope * (x-b) # Linear Extrapolation
     end
 end

 function getcoef(w)
     Wmat = repmat(w, 1, (order+1)); # Step 3 - function value at transformed node
     ccoef = diag(Tmat * Wmat) ./ sum(Tmat.^2,2)[:,1]; #Step 4 Compute coefficients
     return ccoef
 end

 (hermitenodes, hermitewts)=gausshermite(9);
 function hermiteint(f::Function, μ::Float64, σ::Float64)
     return sqrt(1./pi) * sum(map(f, sqrt(2.)*σ*hermitenodes + μ) .* hermitewts);
 end

 function W1V(W)
     wappx(x::Real) = capprox(W, x);
     function wfun(ν::Real)
         return ν + β * hermiteint(wappx, α0+α1*ν, σ0)
     end
     return map(wfun, tcnode)
 end

 function coefun(ccoef, x::Real)
     if (a<=x<=b)
         return sum(Float64[T(i, 2. * (x-a)/(b-a) - 1.) * ccoef[i] for i = 1:(order+1)]);
     elseif (x<a)
         abase = coefun(ccoef, a)
         aslope = (coefun(ccoef, a+0.0001) - abase)/0.0001
         return abase + aslope * (x-a) # Linear Extrapolation
     else
         bbase = ccoef(ccoef, b)
         bslope = (bbase - coefun(ccoef, b - 0.0001))/0.0001
         return bbase + bslope * (x-b) # Linear Extrapolation
     end
 end


 function WApprox(θ1::Vector, θ0::Matrix, H::Matrix, s1::Vector, s0::Matrix, sH::Matrix, wvec::Vector)
     cdist = MvNormal(θ1, H)
     wts0 = pdf(cdist, θ0)
     sdist = MvNormal(s1, sH)
     wts1 = pdf(sdist, s0)
     wts = wts0 .* wts1
     wts = wts ./ sum(wts)
     return (sum(wts .* wvec))
 end

 function W0V(θ1::Vector, x::Vector, w::Float64)
     return 100*log(exp(β*w/100) + exp((θ1[1] + θ1[2] * x[1] + coefun(W1coef, x[3]))/100))
 end

 # Compute the value of holding the machine for given state
function W1fun(n::Int64)
     W1vec = Array(Float64, n)
     for i in 1:n
         W1vec[i] = coefun(W1coef, SMat[3, i])
     end
     return W1vec
end

# Compute the likelihood in each data.
function vupdate!()
   #Update weights and ww_value for old theta
   snew = pdf(snew_dist, SMat)
   wts_old[:] = wts_old - tpdf[1] * spdf[:,1] + tnew * snew;
   ww_old[:] = ww_old - tpdf[1] * wtild[1] * spdf[:,1] + wnew * tnew * snew;

   # Update densities for states
   spdf[:,:] = hcat(spdf[:, 2:end], snew);
   shift!(tpdf);
   push!(tpdf, tnew);
   shift!(wtild);
   push!(wtild, wnew);
   shift!(sdist)
   push!(sdist, snew_dist)

   # Update pdf, weights, and ww_value for new theta
   tpdf1[:] =  pdf(tdist1, thtild);
   wts[:] = spdf * tpdf1;
   ww[:] = spdf * (tpdf1 .* wtild);

   # Update expoential value of adoption for new theta
   if controls
     ex1_new[:] = exp((theta1[1] + theta1[2] * XMat[:,1] + W1vec)/100 + ZMat*kappa1)
   else
     ex1_new[:] = exp((theta1[1] + theta1[2] * XMat[:,1] + W1vec)/100)
   end
end

function ll_fun()
     # Approximation
     Wa_old = ww_old./wts_old;
     Wa_new = ww./wts;

     # Compute exponential of value of not adopting
     ex0_old = exp(β * Wa_old/100)
     ex0_new = exp(β * Wa_new/100)

     # Compute the probability of waiting
     pval_old = ex0_old./(ex0_old+ex1_old)
     pval_new = ex0_new./(ex0_new+ex1_new)

     # Compute likelihood for old and new
     ll_old = sum(log(1-pval_old).*purch_vec + log(pval_old).*(1-purch_vec))
     ll_new = sum(log(1-pval_new).*purch_vec + log(pval_new).*(1-purch_vec))

     return hcat(ll_old, ll_new)
 end

 # Update only if accepted
function supdate!()
     tpdf[:] = tpdf1;
     ex1_old[:] = ex1_new;
     wts_old[:] = wts;
     ww_old[:] = ww;
 end

val = rand(length(np), 2)
function ll!(f::Function, np::Array{Int64, 1})
     @sync begin
         for p in np
             @async val[p-1, :] = fetch(@spawnat p eval(:(ll_fun())))
         end
     end
     return(sum(val,1))
 end

 function postd(l1::Real, l0::Real, theta1::Vector, theta0::Vector, kappa1::Vector, kappa0::Vector, bhat::Vector, sigb::Matrix)
     return exp(l1 + log(pdf(MvNormal(bhat, sigb), vcat(theta1, kappa1))) - l0 - log(pdf(MvNormal(bhat, sigb), vcat(theta0, kappa0))))
 end

 function postd(l1::Real, l0::Real, theta1::Vector, theta0::Vector, bhat::Vector, sigb::Matrix)
     return exp(l1 + log(pdf(MvNormal(bhat, sigb), theta1)) - l0 - log(pdf(MvNormal(bhat, sigb), theta0)))
 end
