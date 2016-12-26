# Simulate Adoption Value
epsdistr = Gumbel()
replications = 1000
periods = 100
X = hcat(eye(nprod), randn(nprod)*0.01+0.5, 16*ones(Float64, nprod), zeros(Float64, nprod));
RX = repmat(X, replications);
eps = rand(epsdistr, nprod*replications, periods);
function deltaf(avail::Vector)
   mmaxu = 0.0
   choice0 = zeros(replications*nprod)
   for t = 0:(periods-1)
      util = broadcast(.*, avail, reshape(RX * tbetas + eps[:,(t+1)], nprod, replications))
      maxu = maximum(util, 1)
      choice1 = reshape(bitunpack(repmat(maxu, nprod) .== util), replications*nprod)
      mmaxu += discountfactor^t * maxu
      RX[:,end] = log((choice1 + choice0 .== 2) .* (exp(RX[:, end])-1) + choice1 .* RX[:, (end-1)] + 1)
      choice0 = choice1
  end
  return mean(mmaxu)
end 
