function c=InsulinConc(C0, M, t)
% This function computes the insulin concentration at specified time points
% and produces a plot of the the insulin concentration
% Inputs:
%   C0 - initial insulin concentration
%   M - mass of the subject in kg
%   t - lapsed time since the of the initial concentration 
%       of insulin in minutes
% Outputs:
%   c - a vector of insulin concentration values at the times 
%       specified in t
%   a plot of c versus t, with proper axes labels and a title

%write your function here
    i = 1;

    % compute the concentration of insulin at each time (t)
    for x = t
        c(i) = C0*exp((-20*x)/M);
        i = i + 1;
    end
    
    % plot a graph of insulation concentration (c) versus the time vector (t)
    plot(t,c);
    xlabel('Time (min)');
    ylabel('Insulin Concentration');
    title('Insulin Concentration over Time');
end
    
