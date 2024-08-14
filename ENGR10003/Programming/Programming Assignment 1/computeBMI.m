function BMI_class = computeBMI(weight, height)
% This function copmputes the BMI to determine if a person is
% underweight, in the healthy weight range, overweight or obese. 
% Inputs:
%  weight: Weight of person (kg)
%  height: Height of person (m)
% Output: BMI_class
%  1   if a person underweight, for BMI less than 18.5
%  2   if a person is in the healthy weight range, for BMI between 18.5 (inclusive) to 24.9 (inclusive)
%  3   if a person is overweight, for BMI above 24.9 (not inclusive) to 29.9 (inclusive)
%  4   if a person is obese, for BMI above 29.9 (not inclusive)

% write your function here
    BMI = weight/height^2;
    if (BMI < 18.5)
        BMI_class = 1;
    elseif (BMI <= 24.9)
        BMI_class = 2;
    elseif (BMI <= 29.9)
        BMI_class = 3;
    else
        BMI_class = 4;
    end
end