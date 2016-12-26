function Tw = Bellman_operator(model_w)

%	Bellman_operator
%	--------------------------------------------------------------------------
%	Evaluates the right hand side of the "expected" Bellman equation
%
%	Works for:
%	model_w.method_code = 1	... Bilinear interpolation
%	model_w.method_code = 2	... Chebyshev approximation

global quad param


if model_w.method_code == 1
	
	L	= size(model_w.x_list,1);	% Number of points on which to evaluate function
	Tw	= zeros(L,1);
	
	%	Loop over all states in x.list and evaluate the Bellman equation
	for i = 1:L
		
		utility	= model_w.precomp_utility(:,i);
		x_next	= model_w.precomp_x_next(:,:,i);
		
		% Expected value
		w_values = interpolate_2D(x_next, model_w);
		Ew = (quad.weights'*w_values)/pi;
		
		v		= [utility; param.beta*Ew];		% Choice-specific values
		max_v	= max(v);						% Subtract max of v to avoid exp(v) == InF
												% (infinity) problem
		
		Tw(i) = log(sum(exp(v - max_v))) + max_v;
	end
	
else	
	%	Note: Using Chebyshev approximation we achive vast efficiency
	%	improvements because the expected basis functions can be pre-computed
	%	and the remaining calculations can be written in matrix form
	
	utility	= model_w.precomp_utility;
	Ew		= model_w.precomp_ET'*model_w.theta;
	
	v		= [utility' param.beta*Ew];		% Choice-specific values
	max_v	= max(max(v));					% Subtract max of v to avoid exp(v) == InF
											% (infinity) problem
	
	Tw = log(sum(exp(v - max_v), 2)) + max_v;
end
