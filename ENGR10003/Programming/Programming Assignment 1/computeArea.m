function area = computeArea(p1, p2, p3)
% This function computes the area of a triangle given the coordinates of the three points that form the triangle 
% Inputs:
%    p1 - coordinates of the first point (x1, y1)
%    p2 - coordinates of the second point (x2, y2)
%    p3 - coordinates of the third point (x3, y3)
% Output:
%    area - area of the triangle points p1, p2, p3 as the corners given by
%    the equation  sqrt(s*(s-a)*(s-b)*(s-c)) where s = (a + b + c)/2;


% write your function here
    a = computeDistance(p1,p2);
    b = computeDistance(p2,p3);
    c = computeDistance(p1,p3);
    s = (a + b + c)/2;
    area = sqrt(s*(s-a)*(s-b)*(s-c));
end