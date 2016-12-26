function y = evaluate_Chebyshev_2D(x, cheb)

%	evaluate_Chebyshev_2D
%	--------------------------------------------------------------------------
%	Evaluate Chebyshev polynomial with coefficients cheb.theta at points x


%	Adjust points in x
xi_1 = 2.0*((x(:,1)-cheb.lower_b(1))/cheb.delta(1)) - 1.0;
xi_2 = 2.0*((x(:,2)-cheb.lower_b(2))/cheb.delta(2)) - 1.0;


%	Corresponding polynomials
T1 = calculate_Chebyshev_polynomials(xi_1, cheb.N);
T2 = calculate_Chebyshev_polynomials(xi_2, cheb.N);


%	Calculate approximated function values
K = size(x,1);
y = zeros(K,1);

for k = 1:K
	y(k) = T1(k,:)*reshape(cheb.theta,cheb.N+1,cheb.N+1)'*T2(k,:)';
end
