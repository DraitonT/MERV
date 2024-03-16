function angle = anglesOfStates(vector1, vector2)
% ANGLESOFSTATES produces the angles between a set of vectors
%
% Inputs:
%   vector1 - [3 x N]
%   vector2 - [3 x N]
% Outputs: 
%   angle - (deg) [1 x N]
%


angle = acosd(dot(vector1, vector2) ./ (vecnorm(vector1) .* vecnorm(vector2)));

end