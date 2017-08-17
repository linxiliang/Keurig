# Functions
function hermiteint(f::Function, μ::Float64, σ::Float64)
 return sqrt(1./pi) * sum(map(f, sqrt(2.)*σ*hermitenodes + μ) .* hermitewts);
end

# Numerical Integration for independent normals.
(hermitenodes, hermitewts)=gausshermite(10);
function hermiteint2d(w_tensor::Array{Float64,3}, μ::Array{Float64,1}, σ::Array{Float64,1}, pbar::Real)
  nx = length(μ);
  nd = length(hermitenodes)
  xn = sqrt(2.)*σ*hermitenodes' .+ μ;
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
       return ν + β * hermiteint(wappx, α0+α1*ν, σ0)
   end
   return map(wfun, delta_nodes)
end

function ll_fun!(Θ_a::Vector, w_tensor::Array{Float64, 3})
  κ = Θ_a[4:end]
  w1_b[:] = (Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] +  W1Vec)/(Θ_a[3]^2) + ZMat*κ;
  #w1_b[:] = (Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] +  W1Vec)/(Θ_a[3]^2) + ZMat*[0.13671037841663608026, 0.79119170457287690823, 0.35703443779432908478, 0.42949855017590554684, 0.01820777905828507501, -0.13282396850696606694];
  for h in 1:nobs
    w0_b[h] = β*hermiteint2d(w_tensor, [SMat[1,h], SMat[3,h]], sigma, SMat[2,h]);
  end
  pvec = exp(w1_b)./(exp(w0_b/(Θ_a[3]^2))+exp(w1_b));

  # Compute likelihood for old and new
  ll = -sum(log(pvec).*purch_vec + log(1-pvec).*(1-purch_vec));
  return ll
end

llvec = zeros(Float64, length(np))
function ll!(Θ_a::Vector)
  println("The parameter is $(Θ_a)")
  err = 1;
  nx = 0;
  broad_mpi(:(Θ_a = $Θ_a))
  wgrid_new = SharedArray(Float64, n1, n2, n3);
  if isnan(wgrid[1,1,1])
    wgrid[:,:,:] = wgrid_new
  end
  while (err > tol && !isnan(err))
      nx = nx+1;
      w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);
      broad_mpi(:(w_tensor = $w_tensor))
      @sync begin
        @parallel for (i,j,k) in mgrid
        pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
        mu = [ρ0 + ρ1 * pbar_n2, α0 + α1 * nodes_3[k]];
        EW_v0 = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2)/(Θ_a[3]^2);
        EW_v1 = (Θ_a[1] - (Θ_a[2]^2) * nodes_1[i] + EW1x[k])/(Θ_a[3]^2);
        wgrid_new[i, j, k] = Θ_a[3]^2 * log(exp(EW_v0) + exp(EW_v1))
        # wgrid_new[i, j, k] = Θ_a[3]^2 * log(exp(β*EW_v/(Θ_a[3]^2)) + exp((Θ_a[1] + Θ_a[2] * nodes_1[i] + EW1x[k])/(Θ_a[3]^2)))
       end
      end
      err = sum(abs(wgrid_new-wgrid))
      #println("error is $(err) with nx $(nx)")
      wgrid[:,:,:] = wgrid_new;
  end
  if isnan(err)
    println("The parameter vector is not feasible")
    return 1.0e9
  end
  w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);
  broad_mpi(:(w_tensor = $w_tensor))
  @sync begin
      for p in np
          @async llvec[p-1] = fetch(@spawnat p eval(:(ll_fun!(Θ_a, w_tensor))))
      end
  end
  ll = sum(llvec)
  println("The likelihood is $(ll)")
  return ll
end
