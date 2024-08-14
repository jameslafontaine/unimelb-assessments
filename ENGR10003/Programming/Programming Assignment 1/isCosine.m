function t = isCosine(a, b, c, gamma) 
% This function checks if triangle of sides a, b and c obeys the Law of Cosines
% Inputs:
%   a, b, c and gamma are positive integers as per the problem specification c^2 = a^2 + b^2 - 2ab cos(gamma)
% Output: 
%   1 the sides and angle obeys the Law of Cosines
%   0 the sides and angle do not obey the Law of Cosines (inputs are valid)
%  -1 the inputs are not valid - they are not positive integers

%write your function here
    % check if the inputs are not positive
    if (floor(a)<=0 || floor(b)<=0 || floor(c)<=0 || floor(gamma)<=0)
        t = -1;
    % check if the inputs are not integers
    elseif (mod(a,1) ~= 0 || mod(b,1) ~= 0 || mod(c,1) ~= 0 || mod(gamma,1) ~= 0)
        t = -1;
    % check if the sides and angle obey the Law of Cosines
    elseif (c^2 == a^2 + b^2 - 2*a*b*cosd(gamma))
        t = 1;
    else
        t = 0;
    end
end