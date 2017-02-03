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

 # Compute the value of holding the machine for given state
function W1fun(n::Int64)
     W1vec = Array(Float64, n)
     for i in 1:n
         W1vec[i] = coefun(W1coef, XMat[i, 3])
     end
     return W1vec
end
