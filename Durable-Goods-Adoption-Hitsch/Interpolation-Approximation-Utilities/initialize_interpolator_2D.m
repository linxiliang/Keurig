function interp = initialize_interpolator_2D(N, lower_b, upper_b)

%	initialize_interpolator_2D
%	--------------------------------------------------------------------------
%	Creates a structure 'interp' that contains all essential information
%	for bilinear interpolation in 2 dimensions
%
%	Inputs:
%	N		... number of grid points in each direction
%	lower_b	... lower bounds of rectangle
%	upper_b	... upper bounds of rectangle
%
%	Created fields:
%	method			... identify approximation method
%	method_code		... code "1" identifies bilinear interpolation
%	h				... step size between grid points
%	x_list			... list of all grid points
%	f				... storage of function values corresponding to x_list


if sum(lower_b >= upper_b) > 0
	disp('Error: Upper bounds need to be > lower bounds');
	interp = [];
	return
end


interp.method = 'bilinear-interpolation';
interp.method_code = 1;

interp.N = N;
interp.lower_b = lower_b;
interp.upper_b = upper_b;


%	Implied step size:
interp.h  = (upper_b - lower_b)./(N - 1);


%	List of points on which f lives
x_list = zeros(N(1)*N(2),2);
for k1 = 1:N(1)
	for k2 = 1:N(2)
		index = (k1-1)*N(2) + k2;
		x_list(index,1) = lower_b(1) + interp.h(1)*(k1-1);
		x_list(index,2) = lower_b(2) + interp.h(2)*(k2-1);
	end
end
interp.x_list = x_list;


%	Storage for function values
interp.f = zeros(N(1),N(2));
