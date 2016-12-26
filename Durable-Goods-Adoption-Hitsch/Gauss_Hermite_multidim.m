function [X,weight] = Gauss_Hermite_multidim(dim,K)

% gauher_multidim(dim ... dimensions, K ... quadrature nodes)
% Creates an array X of quadrature node/vectors, and a vector of
% corresponding weights

[xi,w] = gauher_mdim(K);

% Create a list of nodes and weights
X = xi;
W = w;
for i = 2:dim
    X = expand_mdim(xi,X);
    W = expand_mdim(w,W);
end

weight = cumprod(W,2);
weight = weight(:,dim);



% Sub-function expand_mdim
function z = expand_mdim(x,y)

N_x = size(x,1);
N_y = size(y,1);
z = [];
for i = 1:N_x
    D = repmat(x(i,:),N_y,1);
    E = [D y];
    z = [z;E];
end



% Sub-function gauher_mdim
% (Exact copy of gauher)
function [x,w] = gauher_mdim(n)

EPS = 3.0e-14;
PIM4 = 0.7511255444649425;
MAXIT = 10;
x = zeros(n,1);
w = zeros(n,1);
m=(n+1)/2;
for i=1:m
    if (i == 1)
        z=sqrt((2*n+1))-1.85575*(2*n+1)^(-0.16667);
    elseif (i == 2)
        z = z - 1.14*(n^0.426)/z;
    elseif (i == 3)
        z=1.86*z-0.86*x(1);
    elseif (i == 4)
            z=1.91*z-0.91*x(2);
    else z=2.0*z-x(i-2); 
    end
    for its=1:MAXIT
        p1=PIM4;
        p2=0.0;
        for j=1:n
            p3=p2;
            p2=p1;
            p1=z*sqrt(2.0/j)*p2-sqrt(((j-1))/j)*p3;
        end 
        pp=sqrt(2*n)*p2;
        z1=z;
        z=z1-p1/pp;
        if (abs(z-z1) <= EPS) break; end
    end
    x(i)=z;
    x(n+1-i) = -z;
    w(i)=2.0/(pp*pp);
    w(n+1-i)=w(i);
end
