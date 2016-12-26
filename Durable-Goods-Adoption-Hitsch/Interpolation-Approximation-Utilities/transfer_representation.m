function output_model = transfer_representation(phi, output_model)

%	transfer_representation
%	--------------------------------------------------------------------------
%	Takes a function phi, represented either by table values or as a 
%	Chebyshev polynomial, and creates a new representation using the format
%	in output_model



if phi.method_code == 1
	y = interpolate_2D(output_model.x_list,phi);
elseif phi.method_code == 2
	y = evaluate_Chebyshev_2D(output_model.x_list,phi);
else
	disp('Error: Unknown method'); return;
end


if output_model.method_code == 1
	output_model.f = reshape(y,output_model.N(2),output_model.N(1))';
elseif output_model.method_code == 2
	output_model = calculate_Chebyshev_coefficients_2D(y,output_model);
else
	disp('Error: Unknown method'); return;
end
