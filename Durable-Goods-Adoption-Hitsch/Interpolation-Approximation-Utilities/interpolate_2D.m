function value = interpolate_2D(x, interp)

%	interpolate_2D
%	--------------------------------------------------------------------------
%	Evaluates a 2-dimensional function using bilinear interpolation
%
%	x		... K by 2 array of points at which function is to be interpolated
%	interp	... interpolator structure


N = interp.N;		% Dimensions
K = size(x,1);		% Number of points

j = zeros(K,2);		% Indices
u = zeros(K,2);		% Weights


%	Find weights in each dimension
for k = 1:2
	j(:,k) = find_index(x(:,k),interp.lower_b(k),interp.h(k),N(k));
	u(:,k) = (x(:,k) - (j(:,k)-1)*interp.h(k) - interp.lower_b(k))/interp.h(k);
end



%	Interpolate in each dimension	
index = j(:,1) + N(1)*(j(:,2)-1);
y0 = interp.f(index) + u(:,2).*(interp.f(index+N(1)) - interp.f(index));
index = index + 1;
y1 = interp.f(index) + u(:,2).*(interp.f(index+N(1)) - interp.f(index));


%	Combine results
value = y0 + u(:,1).*(y1 - y0);




function j = find_index(x, lb, h, N)

%	find_index
%	--------------------------------------------------------------------------
%	Finds the index j such that x(j) <= x <= x(j+1) in the grid
%	[lb, lb+h, ..., lb+(N-1)*h].
%	If x < lb, then j = 1, and if x > lb+(N-1)*h, then j = N-1

j = ceil( (x-lb)/h );

j(j < 1) = 1;
j(j >= N) = N-1;
