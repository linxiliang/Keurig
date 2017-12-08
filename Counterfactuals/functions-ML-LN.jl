# Functions
(hermitenodes, hermitewts)=gausshermite(10);
function hermiteint(f::Function, μ::Float64, σ::Float64)
 return sqrt(1./pi) * sum(map(f, exp(sqrt(2.)*σ*hermitenodes + μ).- 1.) .* hermitewts);
end

# Numerical Integration for independent normals.
function hermiteint2d(w_tensor::Array{Float64,3}, μ::Array{Float64,1}, σ::Array{Float64,1}, pbar::Real)
  nx = length(μ);
  nd = length(hermitenodes);
  xn = exp(sqrt(2.)*σ*hermitenodes' .+ μ);
  xn[2,:] = xn[2,:] .- 1.0;
  EV = 0;
  for i in 1:nd
    for j in 1:nd
      EV += hermitewts[i] * hermitewts[j] * chebyshev_evaluate(w_tensor, [xn[1, i], pbar, xn[2,j]], order_tensor, range);
    end
  end
  return EV*(sqrt(1./pi))^nx
end

function W1V(cweights::Array{Float64, 1})
   wappx(x::Real) = chebyshev_evaluate(cweights,[x],delta_order,delta_range)
   function wfun(ν::Real)
       return ν + β * hermiteint(wappx, α0+α1*log(ν+1), σ0)
   end
   return map(wfun, delta_nodes)
end
