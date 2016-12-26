function display_interpolator_2D(interp, title_, xlabel_, ylabel_,is_contour)

%	display_interpolator_2D
%	-----------------------------------------------------------------------

if nargin < 5
	is_contour = 0;
end


axis_1 = interp.lower_b(1):interp.h(1):interp.upper_b(1);
axis_2 = interp.lower_b(2):interp.h(2):interp.upper_b(2);

if ~is_contour
	surf(axis_1, axis_2, interp.f');
else
	contourf(axis_1, axis_2, interp.f');
end
title(title_);
xlabel(xlabel_);
ylabel(ylabel_);
