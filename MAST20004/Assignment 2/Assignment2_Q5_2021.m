 function [output] = Assignment2_Q5_2021
 
% This program simulates 'nreps' repetitions of the experiment described in
% Question 5 of Assignment 2 

n=5; % Number of tokens in the bag 

draws=1:n;  % Vector containing the draws numbers --- it will be used to count the number of matches


pmf_1=zeros(1,n+1); % This vector will contain the empirical pmf of the number of matches in the experiemnt with replacement

pmf_2=zeros(1,n+1); % This vector will contain the empirical pmf of the number of matches in the experiemnt witouth replacement


nreps=1000;   % Number of repetitions of the experiment

for i=1:nreps

%% Draws with replacement

token_numbers_1=randunifd(1,n,n);  % Sequence of token numbers corresponding to the n draws

nb_matches_1=sum(token_numbers_1==draws);   

pmf_1(nb_matches_1+1)=pmf_1(nb_matches_1+1)+1;

%% Draws without replacement

token_numbers_2=randperm(n);  % Sequence of token numbers corresponding to the n draws

nb_matches_2=sum(token_numbers_2==draws);

pmf_2(nb_matches_2+1)=pmf_2(nb_matches_2+1)+1;

end

pmf_1=pmf_1/nreps;

pmf_2=pmf_2/nreps;

display(sprintf('Empirical probability that there is at least one match with replacement is %8.3f', 1-pmf_1(1)));
display(sprintf('Empirical probability that there is at least one match without replacement is %8.3f', 1-pmf_2(1)));

%% Theoretical values

p_1=1-(1-1/n)^n;   % Probability that there is at least one match with replacement
display(sprintf('Theoretical probability that there is at least one match with replacement is %8.3f',p_1));

p_2=1;     % Probability that there is at least one match without replacement


for k=0:n
    
    p_2=p_2-(-1)^k/factorial(k);
    
end

display(sprintf('Theoretical probability that there is at least one match without replacement is %8.3f',p_2));


