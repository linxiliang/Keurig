%	Main.m
%	--------------------------------------------------------------------------
%	Gnter J. Hitsch  --  April 2013
%
%	Durable goods adoption problem.
%	Compares the solution obtained bilinear interpolation with Chebyshev
%	approximation

clear
global param quad
format short g

addpath ./Interpolation-Approximation-Utilities




%	Model parameters ---------------------------------------------------------
param.beta		= 0.99;						% Discount factor
param.delta		= [6.0	5.0];				% Utility intercepts
param.alpha		= -1.00;					% Price coefficient

param.price_mu	= [-0.030  -0.030 ];		% Price process (normal)
param.price_cov	= [ 0.004   0.001 ; 
					0.001   0.004 ];



%	Initialize the integrator:  Choose number of quadrature nodes, -----------
%	and calculate nodes and weights
quad.N = 7;
[quad.nodes, quad.weights] = Gauss_Hermite_multidim(2,quad.N);

quad.nodes = sqrt(2)*quad.nodes*chol(param.price_cov) ...
		   + repmat(param.price_mu, size(quad.nodes,1),1);



%	Interpolation of expected value function using bilinear interpolation -----
%	---------------------------------------------------------------------------
N_grid	= [21 21];					% Number of grid points
lower_b = [ 0.5  0.5];				% Lower bound of grid
upper_b = [10.0 10.0];				% Upper bound of grid

%	Initialize bilinear interpolator
w_interp = initialize_interpolator_2D(N_grid,lower_b,upper_b);



%	Approximation of value function using Chebyshev polynomials ---------------
%	---------------------------------------------------------------------------
N_degree	= 3;					% Polynomial degree
N_nodes		= 7;					% Number of Chebyshev nodes
lower_b_cheb = [ 0.5  0.5];			% Lower bound of rectangle
upper_b_cheb = [10.0 10.0];			% Upper bound of rectangle

%	Initialize Chebyshev approximator
w_cheb = initialize_Chebyshev_2D(N_degree,N_nodes,lower_b_cheb,upper_b_cheb);




%	Convergence settings ------------------------------------------------------
settings.tol				= 1e-8;		% Convergence tolerance (this is set low only
										% for in-class demonstration!)
settings.iter_max			= 2000;		% Max no. of iterations
settings.output_frequency	= 10;		% How frequently to display status information



%	Solve model ---------------------------------------------------------------
w_interp_solution = value_function_iteration(w_interp, settings);
w_cheb_solution   = value_function_iteration(w_cheb, settings);



%	Calculate conditional choice probabilities --------------------------------
N_Pr		= [41   41];			% Number of grid points for Pr
lower_b		= [ 0.5  0.5];			% Lower bound of grid
upper_b		= [10.0 10.0];			% Upper bound of grid

for k = 1:3
	Pr_list_interp(k).Pr	= initialize_interpolator_2D(N_Pr,lower_b,upper_b);
	Pr_list_cheb(k).Pr		= initialize_interpolator_2D(N_Pr,lower_b,upper_b);
	Pr_list_diff(k).Pr		= initialize_interpolator_2D(N_Pr,lower_b,upper_b);
end

Pr_list_interp	= calculate_choice_probabilities(Pr_list_interp, w_interp_solution);
Pr_list_cheb	= calculate_choice_probabilities(Pr_list_cheb, w_cheb_solution);

for k = 1:3
	Pr_list_diff(k).Pr.f = Pr_list_interp(k).Pr.f - Pr_list_cheb(k).Pr.f;
end


%	Create grids for solution graphs ------------------------------------------
N_w			= [41  41];				% Number of grid points for w
lower_b		= [ 0.5  0.5];			% Lower bound of grid
upper_b		= [10.0 10.0];			% Upper bound of grid

graph_w	= initialize_interpolator_2D(N_w,lower_b,upper_b);


%	Transfer solutions to the grid for graphical display ----------------------
w_interp_graph	= transfer_representation(w_interp_solution, graph_w);
w_cheb_graph	= transfer_representation(w_cheb_solution,   graph_w);



%	Display output ------------------------------------------------------------
figure(1);
cm = (0.75:0.01:1)';
cm2 = 0.1 + 0.9*cm;
cm3 = 0.3 + 0.7*cm;
cm = [cm cm2 cm3];
colormap(cm);
	
subplot(1,2,1);
display_interpolator_2D(w_interp_graph,'Expected Value Function: Bilinear Interpolation','price_1','price_2');
axis square

subplot(1,2,2);
display_interpolator_2D(w_cheb_graph,'Expected Value Function: Chebyshev Approximation','price_1','price_2');
axis square


figure(2);
colormap(cm);
subplot(1,2,1);
display_interpolator_2D(Pr_list_interp(1).Pr,'CCP Product 1','price_1','price_2');
axis square

subplot(1,2,2);
display_interpolator_2D(Pr_list_interp(2).Pr,'CCP Product 2','price_1','price_2');
axis square


figure(3);
colormap(cm);
subplot(1,2,1);
display_interpolator_2D(Pr_list_diff(1).Pr,'CCP Product 1: Bilinear-Chebyshev Method','price_1','price_2');
axis square

subplot(1,2,2);
display_interpolator_2D(Pr_list_diff(2).Pr,'CCP Product 2: Bilinear-Chebyshev Method','price_1','price_2');
axis square
