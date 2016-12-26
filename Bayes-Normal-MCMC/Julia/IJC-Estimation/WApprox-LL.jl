# Kernal Density Function
function kden(x0::Matrix, x1::Vector, sig::Matrix)
  cdist = MvNormal(x1, sig) # Sig governs the bandwidth
  denv = pdf(cdist, x0)
  return denv./sum(denv)
end

# W approximation
function WApprox(theta::Vector, sigtheta::Matrix, thetav::Matrix, wmat::Array{Float64,3})
  wt = reshape(repmat(kden(thetav, theta, sigtheta)', nr*nc), nc, nr, N)
  return (sum(wt .* wmat, 3)[:,:,1])'
end

# Iterate the Bellman equation once and get the ccp
function cppf!(w0::Matrix, x::Vector, prob::Vector, coef::Vector)
  w0[:] = w(w0, x, prob)
  (nr, nc) = size(w0)
  zvec = zeros(Float64, nc*nr)
  cont_value = hcat(zeros(Float64, nr), discountfactor .* sum(w0 .* repmat(prob', nr), 2))
  cont_value = reshape(repmat(cont_value', nc), 2, Int64(length(cont_value) * nc/2))'
  current_util = hcat(hcat(repmat(x, nr), delta) * coef, zvec)
  v =  current_util + cont_value
  ccp = exp(v)./sum(exp(v),2)
  return ccp
end

# Loglikelihood function
pvx = reshape(repmat(pvec', length(prices)), T*length(prices))
pmat = repmat(prices, T)
pfilter = pvx .==pmat
afilter = hcat(dt[:adoption].==1, dt[:adoption].==0)
sfilter = dt[:astate].==0
afilter[:, 1] = sfilter .* afilter[:, 1]
function LLF(ccp::Matrix)
  (nr, nc) = size(ccp)
  return sum(log(repmat(ccp[pfilter,:], msize)[afilter]))
end
