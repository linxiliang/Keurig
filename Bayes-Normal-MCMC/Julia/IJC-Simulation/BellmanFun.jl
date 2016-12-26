# Bellman Operators
# Function used to compute next period w for a given vector delta (different brand existence)
function w(w0::Vector, x::Vector, prob::Vector)
  zvec = zeros(Float64, nc)
  # If purchase get lifetime utility, and if not get the future value.
  return log(sum(exp(broadcast(+, hcat(hcat(x, delta) * coef, zvec), hcat(0, discountfactor .* sum(w0 .* prob)))), 2))[:,1]
end

# Matrix Version for faster computation.
function w(w0::Matrix, x::Vector, prob::Vector)
  (nr, nc) = size(w0)
  zvec = zeros(Float64, nc*nr)
  cont_value = hcat(zeros(Float64, nr), discountfactor .* sum(w0 .* repmat(prob', nr), 2))
  cont_value = reshape(repmat(cont_value', nc), 2, Int64(length(cont_value) * nc/2))'
  current_util = hcat(hcat(repmat(x, nr), delta) * coef, zvec)
  # If purchase get lifetime utility, and if not get the future value.
  return reshape(log(sum(exp(current_util + cont_value), 2)), nc, nr)'
end

function w(w0::Matrix, x::Vector, prob::Vector, coef::Vector)
  (nr, nc) = size(w0)
  zvec = zeros(Float64, nc*nr)
  cont_value = hcat(zeros(Float64, nr), discountfactor .* sum(w0 .* repmat(prob', nr), 2))
  cont_value = reshape(repmat(cont_value', nc), 2, Int64(length(cont_value) * nc/2))'
  current_util = hcat(hcat(repmat(x, nr), delta) * coef, zvec)
  # If purchase get lifetime utility, and if not get the future value.
  return reshape(log(sum(exp(current_util + cont_value), 2)), nc, nr)'
end
