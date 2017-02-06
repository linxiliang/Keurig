function w = value_function_iteration(w, settings)

%	value_function_iteration
%	--------------------------------------------------------------------------
%	(Expected) value function iteration.  Works for both discrete and
%	parametric approximation.



%	Pre-compute utilities and states for integration  -----------------------
w = Bellman_operator_precomputations(w);


%	Expected Value function iteration algorithms --------------------------------------
%	Update w -> Tw, calculate the respective norms, and stop if norm < tol

disp(' _________________________');
disp('    Iteration     sup-Norm');
norm		= settings.tol + 1;
iteration	= 1;
tic


while norm >= settings.tol && iteration <= settings.iter_max
	Tw = Bellman_operator(w);

	if w.method_code == 1					% Bilinear interpolation case
		Tw = reshape(Tw,w.N(2),w.N(1))';
		norm = max(max(abs(Tw - w.f)));
		w.f = Tw;
	else
		theta = w.theta;					% Chebyshev approximation case
		w = calculate_Chebyshev_coefficients_2D(Tw,w);
        disp(w.theta);
		norm = max(max(abs(w.theta - theta)));
	end

	if mod(iteration,settings.output_frequency) == 0
		disp([iteration norm]);
	end
	iteration = iteration + 1;
end


elapsed = toc;
disp(' _________________________');
disp([' Elapsed time: ' num2str(elapsed)]);
disp([' Iterations:   ' num2str(iteration-1)]);
disp(' _________________________');
disp(' ');
