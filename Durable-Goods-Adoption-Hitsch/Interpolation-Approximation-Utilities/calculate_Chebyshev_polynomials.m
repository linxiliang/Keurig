function T = calculate_Chebyshev_polynomials(x, N)

%	calculate_Chebyshev_polynomials
%	--------------------------------------------------------------------------
%	Uses a recursive formula to evaluate the Chebyshev polynomials for the
%	points in the vector x.
%	N	... largest degree of Chebyshev polynomials

K = length(x);
if size(x,1) == 1				% Ensure x is a column vector
	x = x';
end
	
T = zeros(K,N+1);

T(:,1) = 1.0;

%	Evaluate polynomials
if N >= 1						% Only if degree >= 1
	T(:,2) = x;
	if N >= 2					% Only if degree >= 2
		for k = 2:N
			T(:,k+1) = 2.0*x.*T(:,k) - T(:,k-1);
		end
	end
end
