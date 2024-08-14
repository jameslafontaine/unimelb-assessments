function B=my_reshape(A,m)
% This function returns a m x (N/m) matrix B, whose elements are taken columnwise 
% from row vector A of length N
% Input:
%  A - a row vector of length N
% Output:
%  B - a matrix of size m x (N/m) OR -1 if N is not divisible by m.


%write your function here
N = length(A);
elemCounter = 1;
col = 1;

    % check if N is divisible by m
    if (rem(N,m) == 0)
        % iterate across through the columns
        while col <= N/m
            row = 1;
            % iterate down through the rows and move on to the next 
            % element of A after each addition of an element
            while row <= m
                B(row,col) = A(elemCounter);
                elemCounter = elemCounter + 1;
                row = row+1;
            end
            col = col+1;
        end
    % otherwise N is not divisible by m so return -1
    else
        B = -1;
    end
end
