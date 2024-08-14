function [loc_id, max_temp] = computeMaxTemperature(file_name)
% This function reads the location and temperature values from a file and 
% computes the maximum temperature of locations
% Inputs:
%   file_name - name of the file that contains the data
% Output:
%   loc_id - 1 x N vector with location IDs, in the same order they
%            appear in the file - N is the number of rows in the file
%            -1 if the file does not exist
%   max_temp - 1 x N vector that contains the maximum temperature for
%              each location
%              -1 if the file does not exist

% write your function here
fid = fopen(file_name);

    % check if the file exists
    if (fid ~= -1)
        % convert the file data into a matrix
        M = fscanf(fid, '%d', [13 inf])';
        fclose(fid);
        
        % put all the loc_ids into a row vector
        location_ids = M(:,1);
        i = 1;
        while i <= length(location_ids)
            loc_id(i) = location_ids(i);
            i = i+1;
        end
        
        % put the max temperature of each row of M into a row vector
        [row_max, ~] = computeMaxMatrix(M(:,2:end));
        j = 1;
        while j <= length(row_max)
            max_temp(j) = row_max(j);
            j = j+1;
        end
        
    % otherwise the file does not exist
    else
        loc_id = -1;
        max_temp = -1;   
    end
end
