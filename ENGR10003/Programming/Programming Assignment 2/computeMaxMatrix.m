function [row_max, matrix_max] = computeMaxMatrix(X)
% This function computes the maximum value of rows and the maximum value
% of the matrix.
% Input:
% X - a is a matrix (the size is arbitrary)
% Outputs:
% row_max - a vector that contains the maximum value of each row
% matrix_max - the maximum value of the matrix

% write your code here
    dimensions = size(X);
    cols = dimensions(2);
    rows = dimensions(1);
    currRow = 1;

    % first find the max values of each row by checking each row one at a 
    % time
    while currRow <= rows
        row_max(currRow) = X(currRow);
        currCol = 1;
        while currCol <= cols
            if (X(currRow,currCol) > row_max(currRow))
                row_max(currRow) = X(currRow,currCol);
            end
            currCol = currCol + 1;
        end
        currRow = currRow + 1;
    end
    
    % now cycle through row_max and find the max of the whole matrix
    matrix_max = X(1);
    for num=row_max
        if (num > matrix_max)
            matrix_max = num;
        end
    end
end
        