function model_w = Bellman_operator_precomputations(model_w)

%	Bellman_operator_precomputations
%	--------------------------------------------------------------------------
%	For the states in model_w.x_list, this algorithm computes:
%	- choice-specific utility (not value!) as a function of the state
%	- the states at which w needs to be evaluated at for integration

global param quad


L = size(model_w.x_list,1);						% Number of state points
M = length(quad.weights);

model_w.precomp_utility = zeros(2,L);
model_w.precomp_x_next  = zeros(M,2,L);

if model_w.method_code == 2
	model_w.precomp_ET = zeros((model_w.N+1)^2,L);
end


%	Loop over all states in x.list and evaluate utilities and states
for i = 1:L
	x = model_w.x_list(i,:);		% Price vector
	
	%	Utility from adopting
	model_w.precomp_utility(:,i) = param.delta + param.alpha*x;
	
	%	States at which expected value function needs to be evaluated at
	log_x	= log(x);
	log_x	= log_x(ones(M,1), :);
	x_next	= exp(log_x + quad.nodes);
	model_w.precomp_x_next(:,:,i) = x_next;
	
	%	Pre-computed expected basis function values
	if model_w.method_code == 2
		T = return_evaluation_T_Chebyshev_2D(x_next, model_w);
		model_w.precomp_ET(:,i) = (T'*quad.weights)/pi;
	end
end
