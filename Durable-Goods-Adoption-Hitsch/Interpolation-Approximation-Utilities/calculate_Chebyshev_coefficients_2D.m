function cheb = calculate_Chebyshev_coefficients_2D(y, cheb)

%	calculate_Chebyshev_coefficients_2D
%	--------------------------------------------------------------------------
%	Uses the function values y = f(cheb.x_list) to calculate/update the
%	Chebyshev coefficients


cheb.theta = cheb.A*y;
