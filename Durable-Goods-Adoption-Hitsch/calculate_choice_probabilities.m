function Pr_list = calculate_choice_probabilities(Pr_list, model_w)

%	calculate_choice_probabilities
%	--------------------------------------------------------------------------
%	Pr_list is an array of structures (K = 3).  Each element is an interpolator
%	object representing the choice probability of option k.

global param quad


L	= size(Pr_list(1).Pr.x_list,1);					% Number of state points
Pr	= zeros(L,3);


%	Loop over all states in x.list and evaluate utilities and states
for i = 1:L
	x = Pr_list(1).Pr.x_list(i,:);		% Price vector
	
	%	Utility from adopting
	utility = param.delta + param.alpha*x;
	
	%	Calculate value of waiting ---------------------
	log_x	= log(x);
	log_x	= log_x(ones(length(quad.weights),1), :);
	x_next	= exp(log_x + quad.nodes);
		
	%	Evaluate w at quadrature nodes
	if model_w.method_code == 1;
		w_values = interpolate_2D(x_next, model_w);
	else
		w_values = evaluate_Chebyshev_2D(x_next, model_w);
	end
	
	Ew = (quad.weights'*w_values)/pi;		% Expected future value
	
	v	= [utility param.beta*Ew];			% Choice-specific values
	v_	= v - max(v);						% Subtract max of v to avoid exp(v) == InF
											% (infinity) problem
	
	% Choice probabilities
	exp_v_		= exp(v_);
	sum_exp_v_	= sum(exp_v_);
	
	for k = 1:3
		Pr(i,k) = exp_v_(k)/sum_exp_v_;
	end
end


for k = 1:3
	Pr_list(k).Pr.f = reshape(Pr(:,k),Pr_list(k).Pr.N(2),Pr_list(k).Pr.N(1))';
end
