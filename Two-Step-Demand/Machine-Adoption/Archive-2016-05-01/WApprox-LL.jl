# Kernal Density Function
function kden(x0::Matrix, x1::Vector, h::Real)
    cdist = MvNormal(x1, eye(length(x1))
    denv = pdf(cdist, broadcast(-, x0, x1)/h))
    return denv ./ sum(denv)
end

# W0 approximation
function WApprox(theta::Vector, sigtheta::Matrix, thetav::Matrix, wmat::Array{Float64,3})
  wt = reshape(repmat(kden(thetav, theta, 0.01)', nr*nc), nc, nr, N)
  return (sum(wt .* wmat, 3)[:,:,1])'
end

# CCP function
function ccp(EW0::Function, price::Vector, ν::Real)
  βEW0 = exp(β * hermiteint(EW0Fun, ν^(γ)*(b^(1-γ)), 1/(1+abs(ν))))
  util = exp((ϑ + β1*price + w1appx(ν))/scaleconstant)
  val = [βEW0; util]
  return vec(val ./ sum(val))
end

# Loglikelihood function
function LLF(ccp::Matrix)
  (nr, nc) = size(ccp)
  return sum(log(repmat(ccp[pfilter,:], msize)[afilter]))
end
