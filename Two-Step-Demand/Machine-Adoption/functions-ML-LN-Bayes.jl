# Functions
(hermitenodes, hermitewts)=gausshermite(15);
function hermiteint(f::Function, μ::Float64, σ::Float64)
  return sqrt(1./pi) * sum(map(f, exp(sqrt(2.)*σ*hermitenodes + μ).- 1.) .* hermitewts);
end

# Numerical Integration for independent normals.
function cheb_eval(w_tensor, x, order_tensor, range)
  x_new = x .* ((range[1,:].>=x) .* (range[2,:].<=x)) .+ range[1,:] .* (range[1,:].<=x) .+  range[2,:] .* (range[2,:].>=x)
  w = chebyshev_evaluate(w_tensor, x_new, order_tensor, range);
  return w
end

function hermiteint2d(w_tensor::Array{Float64,3}, μ::Array{Float64,1}, σ::Array{Float64,1}, pbar::Real)
  nx = length(μ);
  nd = length(hermitenodes);
  xn = exp(sqrt(2.)*σ*hermitenodes' .+ μ);
  xn[2,:] = xn[2,:] .- 1.0;
  EV = 0;
  for i in 1:nd
    for j in 1:nd
      EV += hermitewts[i] * hermitewts[j] * cheb_eval(w_tensor, [xn[1, i], pbar, xn[2,j]], order_tensor, range);
    end
  end
  return EV*(sqrt(1./pi))^nx
end

function W1V(cweights::Array{Float64, 1})
   wappx(x::Real) = cheb_eval(cweights,[x],delta_order,delta_range)
   function wfun(ν::Real)
       return ν + β * hermiteint(wappx, α0+α1*log(ν+1), σ0)
   end
   return map(wfun, delta_nodes)
end

function wupdate!(θ_a::Vector, w_tensor::Array{Float64, 3})
  for h in 1:nobs
    w0_b[h] = cheb_eval(w_tensor, XMat[h,:], order_tensor, range);
  end
  return 0
end

function ll_fun!(Θ_a::Vector, κ::Vector)
  w1_b[:] = Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] + (Θ_a[3]^2) * W1Vec + ZMat*κ;

  #w1_b[:] = (Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] +  W1Vec)/(Θ_a[3]^2) + ZMat*[0.13671037841663608026, 0.79119170457287690823, 0.35703443779432908478, 0.42949855017590554684, 0.01820777905828507501, -0.13282396850696606694];
  pvec = exp(w1_b)./(exp(w0_b)+exp(w1_b));

  # Compute likelihood for old and new
  ll = -sum(log(pvec).*purch_vec + log(1-pvec).*(1-purch_vec));
  return ll
end

function ll_fun_grad!(Θ_a::Vector, κ::Vector)
  w1_b[:] = Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] + (Θ_a[3]^2) * W1Vec + ZMat*κ;
  pvec = exp(w1_b)./(exp(w0_b)+exp(w1_b));
  return -vec(sum((purch_vec - pvec).*ZMat, 1))
end

llvec = zeros(Float64, length(np))
function ll!(Θ_a::Vector)
  #Θ_a = [Θ_a[1], 2.50438, Θ_a[2]]
  println("The parameter is $(Θ_a)")
  err = 1;
  nx = 0;
  broad_mpi(:(Θ_a = $Θ_a))
  wgrid_new[:,:,:] = SharedArray(Float64, n1, n2, n3);
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
          # mu = [ρ0 + ρ1 * log(pbar_n2)+ρ2 * log(nodes_3[k]+1), α0 + α1 * log(nodes_3[k]+1)];
          mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
          EW_v0 = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2);
          EW_v1 = Θ_a[1] - (Θ_a[2]^2) * nodes_1[i] + (Θ_a[3]^2) * EW1x[k];
          wgrid_new[i, j, k] = log(exp(EW_v0) + exp(EW_v1))
          # println("values of v0 $(EW_v0), and value of v1 $(EW_v1), and value w is $(wgrid_new[i, j, k])")
        end
      end
      err = maximum(abs(wgrid_new-wgrid))
      println("error is $(err) with nx $(nx)")
      wgrid[:,:,:] = wgrid_new;
  end
  w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);

 # Directly approximate the expected value of the next period W.
  for (i,j,k) in mgrid
    pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
    # mu = [ρ0 + ρ1 * log(pbar_n2) + ρ2 * log(nodes_3[k]+1), α0 + α1 * log(nodes_3[k]+1)];
    mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
    ExpectedW[i, j, k] = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2);
  end

  w_tensor = chebyshev_weights(ExpectedW, nodes_1, nodes_2, nodes_3, order_tensor, range);
  broad_mpi(:(w_tensor = $w_tensor))

  if isnan(err)
    println("The parameter vector is not feasible")
    return 1.0e9
  end

  # Update the W0 value
  @sync begin
      for p in np
          @async @spawnat p eval(:(wupdate!(Θ_a, w_tensor))) # Too many channels may be used here!
      end
  end

  # Embed the kappa search here!
  function ll_int!(κ::Vector)
    broad_mpi(:(κ[:] = $κ))
    @sync begin
        for p in np
            @async llvec[p-1] = fetch(@spawnat p eval(:(ll_fun!(Θ_a, κ))))
        end
    end
    return sum(llvec)
  end
  function ll_grad!(storage, κ)
    broad_mpi(:(κ[:] = $κ))
    @sync begin
        for p in np
            @async ll_w_grad[p-1, :] = fetch(@spawnat p eval(:(ll_fun_grad!(Θ_a, κ))))
        end
    end
    storage[:] = vec(sum(ll_w_grad, 1))
  end

  llopt = optimize(ll_int!, κ, NelderMead())
  κ[:] = llopt.minimizer
  println("The likelihood is $(llopt.minimum)")
  return llopt.minimum
end

function postd(l1::Real, l0::Real, theta1::Vector, theta0::Vector, beta_prior::Distributions.MvNormal)
    return exp(l1 + log(pdf(beta_prior, theta1)) - l0 - log(pdf(beta_prior, theta0)))
end

# Fast logistic regression
ll
