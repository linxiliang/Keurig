function T = return_evaluation_T_Chebyshev_2D(x, cheb)

%	return_evaluation_T_Chebyshev_2D
%	--------------------------------------------------------------------------
%	Similar to evaluate_Chebyshev_2D.m, but instead evaluating the Chebyshev
%	polynomial at the points in x, it returns the underlying basis function
%	values

%	Adjust points in x
xi_1 = 2.0*((x(:,1)-cheb.lower_b(1))/cheb.delta(1)) - 1.0;
xi_2 = 2.0*((x(:,2)-cheb.lower_b(2))/cheb.delta(2)) - 1.0;


%	Corresponding polynomials
T1 = calculate_Chebyshev_polynomials(xi_1, cheb.N);
T2 = calculate_Chebyshev_polynomials(xi_2, cheb.N);


%	Calculate polynomial terms
K = size(x,1);
T = zeros(K,length(cheb.theta));

for k = 1:K
	T(k,:) = kron(T1(k,:), T2(k,:));
end
