function distance = computeDistance(p1,p2)
% This function computes the distance between two points 
% Inputs:
%   p1 - coordinates of the first point (x1, y1)
%   p2 - coordinates of the second point (x2, y2)
% Output:
%   distance - distance between the two points


% write your function here    
  distance = sqrt((p1(1) - p2(1))^2 + (p1(2) - p2(2))^2);
end