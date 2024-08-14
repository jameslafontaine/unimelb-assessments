function [average_mark,frac_passed]=computeAverageMark(m,pass_threshold)
% Given a vector containing student marks, and a passing mark threshold, this function
% computes the average passing mark and the fraction of students who passed the subject
% Inputs:
%     m - a vector that contains the marks of students in a class
%     pass_threshold - threshold passing mark
% Outputs:
%    average_mark - average mark of all the students who passed,given by, 
%                   sum of marks for students who passed/number of students who passed
%    frac_passed - fraction of students who passed, given by, 
%                  number of students who passed/ total students in the class


% write your funciton here
    passing_mark_sum = 0;
    students_passed = 0;
    total_students = length(m);
    
    % find the number of students who passed and the sum of the passing marks
    for x = m
        if (x >= pass_threshold)
            students_passed = students_passed + 1; 
            passing_mark_sum = passing_mark_sum + x;
        end
    end
    
    % cover the case where students_passed is 0 to avoid dividing by 0
    if (students_passed == 0)
        average_mark = 0;
        frac_passed = 0;
    else
    % calculate the average mark 
        average_mark = passing_mark_sum / students_passed;
    
    % calculate the fraction of students who passed
        frac_passed = students_passed / total_students;
    end
end    