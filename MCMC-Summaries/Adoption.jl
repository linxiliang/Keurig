# MCMC Summaries of Adoption Parameters
# Xiliang Lin
# Jan 2017

#Set Directory
@everywhere cd("$(homedir())/Keurig");

# Load Packages
using Distributions, Gadfly

# Read the mcmcdraws
draws_no_mu = readdlm("Data/Bayes-MCMC/Adoption-Coef-Large-Step.csv", '\t');
(nd, np) = size(draws_no_mu);

# Thin the draws by 20
thin_by = 20
thin_draws = Int64(nd/thin_by)
draws_thin = [draws_no_mu[i*20, j] for i=1:thin_draws, j=1:np]

# Plot the draws of coefficients
plot(x=collect(1:thin_draws), y=draws_thin[:, 1], Geom.line)
sigs = 1/1000 * (draws_thin[:, 1:(end-1)])' * draws_thin[:, 1:(end-1)]
sigs = var(draws_thin[501:1000, 1:(end-1)], 1)
sigs'
writedlm("Data/Bayes-MCMC/ToCopy.csv", sigs')
