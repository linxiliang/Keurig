function cheb = initialize_Chebyshev_2D(N, M, lower_b, upper_b)

%	initialize_Chebyshev_2D
%	--------------------------------------------------------------------------
%	Creates a structure 'cheb' that contains all essential information
%	for Chebyshev regression in 2 dimensions
%
%	Inputs:
%	N		... degree of Chebyshev polynomial
%	M		... number of Chebyshev interpolation nodes
%	lower_b	... lower bounds of rectangle
%	upper_b	... upper bounds of rectangle
%
%	Created fields:
%	method		... identify approximation method
%	method_code	... code "2" identifies Chebyshev approximation
%	theta		... Chebyshev coefficients
%	x_list		... list of all approximation nodes on which the function 
%	delta		... difference between upper and lower bounds of rectangle
%					needs to be evaluated to calculate the coefficients
%	T, T2		... pre-computed Chebyshev polynomial values and sum-of-squares
%				... values on nodes; used in calculation of coefficients


if M < N+1
	disp('Error: Number of Chebyshev interpolation nodes < N + 1');
	cheb = [];
	return
end
if sum(lower_b >= upper_b) > 0
	disp('Error: Upper bounds need to be > lower bounds');
	cheb = [];
	return
end



%	Construct interpolation nodes -------------------------------------------
z_nodes = zeros(M,1);
for k = 1:M
	z_nodes(k) = -cos(pi*((2*k - 1)/(2*M)));
end


%	Adjust nodes to range of rectangle and create a list of all -------------
%	node vectors
x_nodes = zeros(M,2);
delta = upper_b - lower_b;
for i = 1:2
	x_nodes(:,i) = 0.5*delta(i)*(z_nodes+1) + lower_b(i);
end

x_list = zeros(M*M,2);
for k1 = 1:M
	for k2 = 1:M
		index = (k1-1)*M + k2;
		x_list(index,1) = x_nodes(k1,1);
		x_list(index,2) = x_nodes(k2,2);
	end
end


%	Calculate polynomial values for all nodes (M x N+1 matrix) --------------
T = calculate_Chebyshev_polynomials(z_nodes, N);


%	Precompute the denominator "sum of squares" terms used in the -----------
%	Chebyshev coefficients
T2 = zeros(N+1,1);
for i = 1:N+1
	T2(i) = T(:,i)'*T(:,i);
end


%	Calculate X, a regressor matrix of Chebyshev polynomials ------------------
%	and A = inv(X'*X)*X'
X = kron(T,T);
A = X'*X \ X';




%	Pack all information into the 'cheb' structure --------------------------
cheb.method = 'Chebyshev-approximation';
cheb.method_code = 2;
cheb.N = N;
cheb.M = M;
cheb.lower_b = lower_b;
cheb.upper_b = upper_b;
cheb.theta = zeros((N+1)^2,1);
cheb.x_list = x_list;
cheb.delta = delta;
cheb.T = T;
cheb.T2 = T2;
cheb.A = A;
