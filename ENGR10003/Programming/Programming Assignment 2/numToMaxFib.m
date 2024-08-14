function N = numToMaxFib(M)
% This function computes the number of elements (N) in the Finonacci series
% up to a specified maximum value M
% Input:
%    M - the specified maximum value for the series
% Output:
%    N - number of elements in the series less than or equal to M
%    If the user specifies an invalid value for M, such as
%     0, 1 or a negative number, the function must return -1

%write your function here
fibSequence = [0 1];
i = 3;

    % check if M is greater than 1
    if (M>1)
        % compute the Fibonacci sequence up until the maximum value will 
        % be passed 
        while ((fibSequence(end) + fibSequence(end-1)) <= M)
            fibSequence(i) = fibSequence(i-1) + fibSequence(i-2);
            i = i+1;
        end
        N = length(fibSequence);
    % otherwise the user must have specified an invalid value for M        
    else
        N = -1;
    end
end