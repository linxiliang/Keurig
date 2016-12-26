#########################################################################
#
# Simple Discrete Choice Simulation 
# with Normal Consumer Heterogeneithy
# and State Dependence
# 
# Xiliang Lin
# Nov, 2015
#########################################################################
workspace() 
#Install missing packages
function PkgCheck(pkg::AbstractString)
    try
      eval(parse("using "*pkg))
    catch
      Pkg.add(pkg)
      eval(parse("using "*pkg))
    end
    return 0
end
#Load required packages
PkgCheck("Distributions")
PkgCheck("DataFrames")
PkgCheck("GLM")
PkgCheck("ForwardDiff")
PkgCheck("Optim")

#------------------------------------------------------------------------#
#Simulate the data
srand(123)
#Set Preference Distributions
sigma = [0.9 0.01 0.0 0.01
         0.01 0.9 0.0 0.01
         0.0 0.0 0.09 0.0 
         0.01 0.01 0.0 0.04];
mu = [0, 0, -4, 0.5];
trueparam = MvNormal(mu, sigma);

#Simulate preference parameters
nhh = 10000
preferences=rand(trueparam, nhh)'
#No heterogeneity - to test the codes
#preferences = transpose(repmat(mu, 1, nhh))
preferences=convert(SharedArray, preferences)

#Let the time period be a binomial draw 
TBinom = Binomial(10, 0.5)
periods = rand(TBinom, nhh)
periods = convert(SharedArray, periods)

#Let the price of the three products be draw from uniform distribution
price = rand(sum(periods), 2)
price = hcat(rep(0, sum(periods)), price)
price = reshape(price', (3*sum(periods), 1))
price = convert(SharedArray, price)

#Simulate the latent utility draw
eps = rand(Gumbel(), sum(periods)*3, 1)
eps = convert(SharedArray, eps)
	    
#Now simulate the purchasing sequences
#Using shared array to store the positions 
hh_pos = SharedArray(Int64, nhh, 2)
#Using shared array for purchase sequences
utils = SharedArray(Float64, 3*sum(periods))
purchases = SharedArray(Float64, 3*sum(periods))
state = SharedArray(Float64, 3*sum(periods))
@parallel for i=1:nhh
    if (i==1)
        hh_pos[i,1] = 1
    else 
        hh_pos[i,1] = 3*sum(periods[1:(i-1)])+1
    end 
    hh_pos[i, 2] = 3*sum(periods[1:i])
end

function util(j::Int64, prefer::Matrix, prices::Vector, state::Vector, noise::Vector)
    if (j == 1)
        return(noise[1])
    else
        #println("equation = $(prefer[j-1]) + $(prefer[3]) * $(prices[j]) + $(prefer[4]) * $(state[j]) + $(noise[j])\n")
        return(prefer[j-1] + prefer[3] * prices[j] + prefer[4] * state[j] + noise[j])
    end
end

@parallel for i=1:nhh
    for t = hh_pos[i, 1]:3:hh_pos[i, 2]
        utils[t:(t+2)] = [util(1, preferences[i,:]', price[t:(t+2)], state[t:(t+2)], eps[t:(t+2)]),
		      util(2, preferences[i,:]', price[t:(t+2)], state[t:(t+2)], eps[t:(t+2)]),
			   util(3, preferences[i,:]', price[t:(t+2)], state[t:(t+2)], eps[t:(t+2)])]
        purchases[t:(t+2)]= utils[t:(t+2)].==maximum(utils[t:(t+2)])
	if  (t != (hh_pos[i,2]-2))
         if (purchases[t]==true)
	   state[(t+3):(t+5)] = state[t:(t+2)]
	 else
	   state[(t+3):(t+5)] = purchases[t:(t+2)]
	 end
       end
    end
end


#-----------------------------------------------------------------------------------------------------#

#Estimation using simple discrete choice, and let's see the estimates for the parameters
prod_intercepts = [1.0 0.0 0.0
                   0.0 1.0 0.0
                   0.0 0.0 1.0]
prod_intercepts = repmat(prod_intercepts, sum(periods), 1)
X = hcat(prod_intercepts, price, state)

function LL(param::Vector)
    param = [0.0; param]
    pmat = (repmat(param, 1, sum(periods)*3))'
    #println("The pmat size is $(size(pmat))")
    #println("The X size is $(size(X))")
    expu = exp(sum(pmat .* X, 2))

    ll = sum([purchases[i]*log(expu[i]/sum(expu[i:(i+2)])) + 
              purchases[i+1]*log(expu[i+1]/sum(expu[i:(i+2)])) + 
              purchases[i+2]*log(expu[i+2]/sum(expu[i:(i+2)])) for i = 1:3:(3*sum(periods))])

    #println("The type of ll is $(typeof(ll)), and value is $(-ll)")
    return -ll;
end

g_ll = ForwardDiff.gradient(LL)
h_ll = ForwardDiff.hessian(LL)
opt = optimize(LL, [0.0,0.0,0.0,0.0], autodiff=true)
opt.minimum
sqrt(diag(inv(h_ll(opt.minimum))))