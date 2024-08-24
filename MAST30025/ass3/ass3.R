library(MASS)



## FOR CHECKING ANSWERS, CANT USE LM IN QUESTION 2 OR 5 WORKING OUT ##
#homes = data.frame(Price = y, Age = X[,2], Train_distance = X[,3], 
                   #Conv_store = X[,4])
#homemodel = lm(Price ~ Age + Train_distance + Conv_store, data=homes)
#summary(homemodel)


## Question 2 ##

# 2a (plot an interaction plot)
rot_df = data.frame(rotting = c(13, 11, 3, 26, 19, 24, 10, 4, 7, 15, 22, 18, 15, 2, 7, 20, 24, 8),
      oxygen_level = c(rep(1, 6), rep(2, 6), rep(3, 6)),
      temperature = c(rep(c(10, 16), 3, each=3)) )

rot_df$oxygen_level = factor(rot_df$oxygen_level)
rot_df$temperature = factor(rot_df$temperature)

with(rot_df, (interaction.plot(oxygen_level, temperature, rotting)))

# There may be some interaction, although its hard to be certain due to the low sample size and
# due to the fact that the lines are somewhat parallel

#2b (fit an additive model, outputting design matrix. estimate common variance)

y = rot_df$rotting
n = length(y)
X = matrix(c(rep(1, n), rep(0, n*5)), n, 6)
X[cbind(1:n, as.numeric(rot_df$oxygen_level)+1)] = 1
X[cbind(1:n, as.numeric(rot_df$temperature)+4)] = 1

library(Matrix)

r=rankMatrix(X)[1]

# find conditional inverse of XtX
 
XtX = t(X) %*% X

M = XtX[2:5, 2:5]

det(M)

XtXc = matrix(0, 6, 6)

XtXc[2:5,2:5] = t(solve(M))

XtXc = t(XtXc)

all.equal(XtX %*% XtXc %*% XtX,XtX)
    
# can't use ginv
#round(ginv(t(X) %*% X), 5)
  
b = XtXc %*% t(X) %*% y

s2 = sum((y - X %*% b)^2) / (n-r)

X

s2

# 2c (calculate a 95% confidence interval for the difference between 
#     the temperature effects)

tt = c(0, 0, 0, 0, 1, -1) # temp10mean - temp16mean

ta = qt(0.975, n-r)

halfwidth = ta * sqrt(s2 * t(tt) %*% XtXc %*% tt)

tt %*% b + c(-1, 1) * halfwidth





# 2d (test the hypothesis that oxygen level has no effect on rotting at
#     5% significance level)

C = matrix(c(0,0,1,1,-1,0,0,-1,0,0,0,0), 2, 6)

all.equal(round(C %*% XtXc %*% t(X) %*% X, 3), C)

numer = t(C %*% b) %*% solve(C %*% XtXc %*% t(C)) %*% C%*%b

Fstat = (numer / 2 )/ s2

pf(Fstat, 2, n-r, lower=F) # cannot reject H0

#model = lm(rotting ~ oxygen_level + temperature, rot_df)

#summary(model)

# 2e (suppose we are interested in the effect of oxygen level only,
#     but know that temperature affects the results, so we include it
#     in our model. What type of design would this study be?)

# This would be a complete block design study with oxygen level as the 
# factor of interest and temperature as the blocking factor.

## Question 4 ##

toycars = read.csv("D:\\UniMelb\\2023 S1\\MAST30025 - Linear Statistical Models\\Assignments\\ass3\\toycars.csv")

toycars$car = factor(toycars$car)

# 4a (plot the data, note any observations)
plot(toycars$angle, toycars$distance, pch=array(toycars$car), col=as.numeric(toycars$car)+1, cex=1.5)

# It seems that the distance travelled by the car increases as the angle increases, unsurprisingly, and it would
# also seem that the type of car doesn't seem to have as much of an effect in this experiment although
# car 3 generally travels  the least distance, while car 1 travels slightly further and car 2 travels the furthest

# 4b (test for the presence of any interaction between angle and type of toy car)

imodel = lm(toycars$distance ~ toycars$car * toycars$angle, data=toycars)

amodel = lm(toycars$distance ~ toycars$car + toycars$angle, data=toycars)

anova(amodel, imodel) # interaction is not significant 

# 4c (use backward elimination to select relevant variables for the data)

fullmodel = imodel

drop1(fullmodel, scope = ~ ., test="F")

backmodel2 = lm(toycars$distance ~ toycars$car + toycars$angle, data=toycars)

drop1(backmodel2, scope = ~ ., test="F")

# all tests are significant, we stop at backmodel2

# 4d (for backmodel2, test the hypothesis that the type 1 toy car travels 0.05 metres
#     more on average than type 3 toy car at the 5% significance level)
linearHypothesis(backmodel2, c(0,0,-1, 0), 0.05)


# cannot reject H0

# 4e (in the full model with interaction, test the hypothesis that the distances travelled
#     by the type 2 and type 3 cars are the same at the 5% significance level)
linearHypothesis(fullmodel, c(0,1,-1, 0, 0, 0), 0)

# cannot reject H0 at 5% significance level

## QUESTION 5 ##

# 5a (State the reason why mu can be regarded as a nuisance parameter)

# Mu can be regarded as a nuisance parameter since we are not interested in it
# specifically but must account for it in our model

# 5b (X1 and X2)
X1 = matrix(c(rep(1, 6)), 6, 1)

library(MASS)
H1 = X1 %*% ginv(t(X1) %*% X1) %*% t(X1)


# 5c
X2 = matrix(c(1,1,1,0,0,0,0,0,0,1,1,1,2,4,8,7,6,4), 6,3)
all.equal(fractions(diag(6) - H1) %*% X2, X21)

# 5d
xs = c(2,4,8,7,6,4)

y = c(4,2,10,8,8,12)


xbar = mean(xs)



X21 = matrix(c(0.5,0.5,0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,0.5,0.5,0.5,xs[1]-xbar,xs[2]-xbar,xs[3]-xbar,xs[4]-xbar,xs[5]-xbar,xs[6]-xbar), 6,3)

b2 = ginv(t(X21) %*% X21) %*% t(X21) %*% y

b2

# 5e

b1 = ginv(t(X1) %*% X1) %*% (t(X1) %*% y - t(X1) %*% X2 %*% b2)

b1

X = cbind(X1, X2)

b = rbind(b1,b2)

t(X) %*% X %*% b 

t(X) %*% y

"
\documentclass{article}
\usepackage{graphicx} % Required for inserting images
\graphicspath{ {./images/} }
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{listings}

\title{MAST30025 Linear Statistical Models 
Assignment 3}
\author{Student Code: 1079860 }
\date{May 2023}

\begin{document}


\maketitle

\newpage
\section*{\section*{Question 1}}
\hspace{-0.5cm}
\subsection*{(a)}
$r(A^cA) \le min [r(A^c),\;r(A)] \le r(A)$ \\\\
$r(A) = r(AA^cA) \le min[r(A),\;r(A^cA)] \le r(A^cA)$ \\\\
$ \implies r(A^CA) = r(A)$ \\\\

\subsection*{(b)}
$(I - A(A^TA)^cA^T)(I - A(A^TA)^cA^T) $ \\\\
$ = I^2 - IA(A^TA)^cA^T - A(A^TA)^cA^TI + A(A^TA)^cA^TA(A^TA)^cA^T $ \\\\
$ = I - 2(A(A^TA)^cA^T) + A(A^TA)^cA^TA(A^TA)^cA^T $ \\\\
$ = I - 2(A(A^TA)^cA^T) + [A(A^TA)^cA^TA]\;(A^TA)^cA^T $ \\\\
$ = I - 2(A(A^TA)^cA^T) + A(A^TA)^cA^T $ \\\\
$ I - A(A^TA)^cA^T\;\;is\;\;idempotent,\;\;I\;\;and\;\; A(A^TA)^cA^T\;\;are\;\;idempotent\;\;and\;\;symmetric $ \\\\

\pagebreak
\subsection*{(c)}
$r(A(A^TA)^cA^T) \le min[r(A), r((A^TA)^c), r(A^T)] \le r(A) $ \\\\
$ r(A) = r(A(A^TA)^cA^TA) \le min[r(A), r(A(A^TA)^cA^T)] \le r(A(A^TA)^cA^T) $ \\\\
$ \implies r(A(A^TA)^cA^T) = r(A) $ \\\\
$ A(A^TA)^cA^T\;is\;\;n\times n\;\;so\;\;I\;must\;\;be\;\;n\times n $ \\\\
$ \implies r(I) = n $ \\\\
$ As\;\;I - A(A^TA)^cA^T\;\;is\;\;symmetric\;\;and\;\;n\times n,\;\;it\;\;is\;\;diagonalised\;\;by\;\;P,\;\;so $
$ r(I - A(A^TA)^cA^T) = r(P^T(I - A(A^TA)^cA^T)P) = r(I - D)$ \\\\
$Since\;\;A(A^TA)^cA^T\;\;is\;\;idempotent,\;\;it\;\;has\;\; r(A(A^TA)^cA^T)=r(A)\;\;1s\;\;on\;\;the\;\;diagonal \\ of\;\;its\;\;diagonal\;\;matrix\;\;D $ \\\\
$ \implies r(I-D) = r(I) - r(D) = n - r(A)\;\;as\;\;the\;\;rank\;\;of\;\;a\;\;diagonal\;\;matrix =the\;\;number\;\;of\;\;non-zero\;\;entries\;\;on\;\;its\;\;diagonal $


\newpage
\section*{\section*{Question 2}}

\subsection*{(a)}
\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]
rot_df = data.frame(rotting = c(13, 11, 3, 26, 19, 24, 10, 4, 7, 15,
                                22, 18, 15, 2, 7, 20, 24, 8),
                    oxygen_level = c(rep(1, 6), rep(2, 6), rep(3, 6)),
                    temperature = c(rep(c(10, 16), 3, each=3)) )

rot_df$oxygen_level = factor(rot_df$oxygen_level)
rot_df$temperature = factor(rot_df$temperature)

with(rot_df, (interaction.plot(oxygen_level, temperature, rotting)))
\end{lstlisting}  \\

\includegraphics[scale=0.55]{images/q2intplot.png}


\hspace{-0.52cm}It may be reasonable to assume that there is no interaction due to the lines being relatively parallel despite the low sample size, although its hard to be certain.

\pagebreak

\subsection*{(b)}
\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]
> y = rot_df$rotting
> n = length(y)
> X = matrix(c(rep(1, n), rep(0, n*5)), n, 6)
> X[cbind(1:n, as.numeric(rot_df$oxygen_level)+1)] = 1
> X[cbind(1:n, as.numeric(rot_df$temperature)+4)] = 1
> 
> library(Matrix)
> 
> r=rankMatrix(X)[1]
> 
> # find conditional inverse of XtX
>  
> XtX = t(X) %*% X
> 
> M = XtX[2:5, 2:5]
> 
> det(M)
[1] 972
> 
> XtXc = matrix(0, 6, 6)
> 
> XtXc[2:5,2:5] = t(solve(M))
> 
> XtXc = t(XtXc)
> 
> all.equal(XtX %*% XtXc %*% XtX,XtX)
[1] TRUE    
>   
> b = XtXc %*% t(X) %*% y
> 
> s2 = sum((y - X %*% b)^2) / (n-r)

\end{lstlisting} 
\pagebreak

\begin{lstlisting}[language=R]
> X
      [,1] [,2] [,3] [,4] [,5] [,6]
 [1,]    1    1    0    0    1    0
 [2,]    1    1    0    0    1    0
 [3,]    1    1    0    0    1    0
 [4,]    1    1    0    0    0    1
 [5,]    1    1    0    0    0    1
 [6,]    1    1    0    0    0    1
 [7,]    1    0    1    0    1    0
 [8,]    1    0    1    0    1    0
 [9,]    1    0    1    0    1    0
[10,]    1    0    1    0    0    1
[11,]    1    0    1    0    0    1
[12,]    1    0    1    0    0    1
[13,]    1    0    0    1    1    0
[14,]    1    0    0    1    1    0
[15,]    1    0    0    1    1    0
[16,]    1    0    0    1    0    1
[17,]    1    0    0    1    0    1
[18,]    1    0    0    1    0    1
> 
> s2
[1] 26.12698
\end{lstlisting} 

\pagebreak
\subsection*{(c)}

\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]
> tt = c(0, 0, 0, 0, 1, -1) # temp10mean - temp16mean
> 
> ta = qt(0.975, n-r)
> 
> halfwidth = ta * sqrt(s2 * t(tt) %*% XtXc %*% tt)
> 
> tt %*% b + c(-1, 1) * halfwidth
[1] -16.723555  -6.387556
\end{lstlisting} 
\vspace{5mm}
\hspace{-0.2cm}
$95\%\;\;confidence\;\;interval\;\;for\;\;temp10effect - temp16effect: $ \\

\hspace{-0.52cm}
$ [-16.724, -6.388] $
\vspace{5mm}

\subsection*{(d)}

$ H_0: \tau_1 = \tau_2 = \tau_3 = 0 $ \\

\hspace{-0.52cm}\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]
> C = matrix(c(0,0,1,1,-1,0,0,-1,0,0,0,0), 2, 6)
> 
> all.equal(round(C %*% XtXc %*% t(X) %*% X, 3), C)
[1] TRUE
> 
> numer = t(C %*% b) %*% solve(C %*% XtXc %*% t(C)) %*% C%*%b
> 
> Fstat = (numer / 2 )/ s2
> 
> pf(Fstat, 2, n-r, lower=F)
          [,1]
[1,] 0.4481124
\end{lstlisting} 

\vspace{5mm}
\hspace{-0.52cm}$ p-value = 0.448 \implies Cannot\;\;reject\;\;H_0\;\;at\;\;5\%\;\;significance\;\;level $\\

\subsection*{(e)}
This would be a complete block design study with oxygen level as the  factor of interest and temperature as the blocking factor.


\newpage
\section*{\section*{Question 3}}


\newpage

\section*{\section*{Question 4}}
\subsection*{(a)}

\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]
toycars = read.csv("toycars.csv")

toycars$car = factor(toycars$car)

plot(toycars$angle, toycars$distance, pch=array(toycars$car), 
col=as.numeric(toycars$car)+1, cex=1.5)

\end{lstlisting}

\includegraphics[scale=0.55]{images/q4dataplot.png}

\vspace{5mm}
\hspace{-0.52cm}It seems that the distance travelled by the car increases as the angle increases. It would also seem that the type of car doesn't seem to have as much of an effect as the angle in this experiment, although we can still say that car 3 generally travels the least distance, while car 1 travels slightly further and car 2 travels the furthest.

\pagebreak




\subsection*{(b)}
\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]

> imodel = lm(toycars$distance ~ toycars$car * toycars$angle, data=toycars)
> 
> amodel = lm(toycars$distance ~ toycars$car + toycars$angle, data=toycars)
> 
> anova(amodel, imodel)
Analysis of Variance Table

Model 1: toycars$distance ~ toycars$car + toycars$angle
Model 2: toycars$distance ~ toycars$car * toycars$angle
  Res.Df      RSS Df Sum of Sq      F Pr(>F)
1     23 0.105657                           
2     21 0.093271  2  0.012386 1.3944   0.27

\end{lstlisting}

\vspace{5mm}
\hspace{-0.52cm}No significant interaction present between the type of toy car and the angle

\pagebreak

\subsection*{(c)}

\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]

> fullmodel = imodel
> 
> drop1(fullmodel, scope = ~ ., test="F")
Single term deletions

Model:
toycars$distance ~ toycars$car * toycars$angle
                          Df Sum of Sq     RSS      AIC  F value   Pr(>F)
<none>                                 0.09327 -141.038                  
toycars$car                2   0.01979 0.11307 -139.842   2.2284   0.1325
toycars$angle              1   0.44593 0.53920  -95.664 100.4023 1.87e-09
toycars$car:toycars$angle  2   0.01239 0.10566 -141.672   1.3944   0.2700
                             
<none>                       
toycars$car                  
toycars$angle             ***
toycars$car:toycars$angle    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> backmodel2 = lm(toycars$distance ~ toycars$car + toycars$angle, data=toycars)
> 
> drop1(backmodel2, scope = ~ ., test="F")
Single term deletions

Model:
toycars$distance ~ toycars$car + toycars$angle
              Df Sum of Sq     RSS      AIC F value    Pr(>F)    
<none>                     0.10566 -141.672                      
toycars$car    2   0.16945 0.27511 -119.833  18.444 1.662e-05 ***
toycars$angle  1   1.65108 1.75673  -67.774 359.416 1.547e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> # all tests are significant, we stop at backmodel2

\end{lstlisting}

\pagebreak

\subsection*{(d)}

$ H_0: \tau_1 - \tau_3 = 0.05 $ \\

\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]

> linearHypothesis(backmodel2, c(0,0,-1, 0), 0.05)
Linear hypothesis test

Hypothesis:
- toycars$car3 = 0.05

Model 1: restricted model
Model 2: toycars$distance ~ toycars$car + toycars$angle

  Res.Df     RSS Df Sum of Sq      F Pr(>F)
1     24 0.11033                           
2     23 0.10566  1 0.0046722 1.0171 0.3237
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


\end{lstlisting}

\vspace{5mm}
\hspace{-0.52cm}p-value = 0.3237 $>$ 0.05, so we cannot reject $ H_0 $ at the 5\% significance level

\vspace{5mm}


\pagebreak

\subsection*{(e)}
$ H_0: \tau_2 = \tau_3 $ \\

\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]

> linearHypothesis(fullmodel, c(0,1,-1, 0, 0, 0), 0)
Linear hypothesis test

Hypothesis:
toycars$car2 - toycars$car3 = 0

Model 1: restricted model
Model 2: toycars$distance ~ toycars$car * toycars$angle

  Res.Df      RSS Df Sum of Sq      F  Pr(>F)  
1     22 0.108767                              
2     21 0.093271  1  0.015497 3.4891 0.07579 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


\end{lstlisting}

\vspace{5mm}
\hspace{-0.52cm}p-value = 0.0758 $>$ 0.05, so we cannot reject $ H_0 $ at the 5\% significance level


\newpage

\section*{\section*{Question 5}}

\subsection*{(a)}
Mu can be regarded as a nuisance parameter since we are not interested in it specifically but it still accounts for some variation in our model

\subsection*{(b)}
$ X_1 = 
\begin{bmatrix} 
1  \\
1  \\
1  \\
1  \\
1  \\
1  \\

\end{bmatrix}$ \\\\

\hspace{-0.52cm}$ X_2 = 
\begin{bmatrix} 
1 & 0 & x_{11}  \\
1 & 0 & x_{12}  \\
1 & 0 & x_{13} \\
0 & 1 & x_{21}  \\
0 & 1 & x_{22}  \\
0 & 1 & x_{23}  \\
\end{bmatrix}$

\pagebreak

\subsection*{(c)} 

\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]

> X1 = matrix(c(rep(1, 6)), 6, 1)
> 
> H1 = X1 %*% ginv(t(X1) %*% X1) %*% t(X1)
> 
> fractions(diag(6) - H1)
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]  5/6 -1/6 -1/6 -1/6 -1/6 -1/6
[2,] -1/6  5/6 -1/6 -1/6 -1/6 -1/6
[3,] -1/6 -1/6  5/6 -1/6 -1/6 -1/6
[4,] -1/6 -1/6 -1/6  5/6 -1/6 -1/6
[5,] -1/6 -1/6 -1/6 -1/6  5/6 -1/6
[6,] -1/6 -1/6 -1/6 -1/6 -1/6  5/6


\end{lstlisting}

\vspace{2mm}

\hspace{-0.52cm}$ X_{2\mid1} = [I-H_1]X_2$
$ = \begin{bmatrix} 
\frac{5}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6}   \\
-\frac{1}{6} & \frac{5}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6}   \\
-\frac{1}{6} & -\frac{1}{6} & \frac{5}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6}   \\
-\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & \frac{5}{6} & -\frac{1}{6} & -\frac{1}{6}    \\
-\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & \frac{5}{6} & -\frac{1}{6}   \\
-\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & -\frac{1}{6} & \frac{5}{6}  \\
\end{bmatrix}
\begin{bmatrix} 
1 & 0 & x_{11}  \\
1 & 0 & x_{12}  \\
1 & 0 & x_{13} \\
0 & 1 & x_{21}  \\
0 & 1 & x_{22}  \\
0 & 1 & x_{23}  \\
\end{bmatrix}$

\vspace{2mm}

\hspace{2.45cm}$ = 
\begin{bmatrix} 
\frac{1}{2} & -\frac{1}{2} & x_{11} - \bar{x}  \\
\frac{1}{2} & -\frac{1}{2} & x_{12} - \bar{x} \\
\frac{1}{2} & -\frac{1}{2} & x_{13} - \bar{x}\\
-\frac{1}{2} & \frac{1}{2} & x_{21} - \bar{x} \\
-\frac{1}{2} & \frac{1}{2} & x_{22} - \bar{x} \\
-\frac{1}{2} & \frac{1}{2} & x_{23} - \bar{x} \\
\end{bmatrix}$

\pagebreak

\subsection*{(d)}
$ \vec{b_2} = (X_{2\mid1}^T X_{2\mid1})^c X_{2\mid1}^T \vec{y} $ \\\\
\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]

> xs = c(2,4,8,7,6,4)
>
> y = c(4,2,10,8,8,12)
>
> xbar = mean(xs)
>
> X21 = matrix(c(0.5,0.5,0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,0.5,0.5,0.5,
         xs[1]-xbar,xs[2]-xbar,xs[3]-xbar,xs[4]-xbar,xs[5]-xbar,
         xs[6]-xbar), 6,3)
> 
> b2 = ginv(t(X21) %*% X21) %*% t(X21) %*% y
> 
> b2
           [,1]
[1,] -1.6857143
[2,]  1.6857143
[3,]  0.6285714


\end{lstlisting}

\vspace{2mm}

\hspace{-0.52cm}$\vec{b_2} = 
\begin{bmatrix} 
-1.6857   \\
1.6857 \\
0.6286\\
\end{bmatrix}$

\pagebreak 

\subsection*{(e)}
$ \vec{b_1} = (X_1^T X_1)^c(X_1^T \vec{y} - X_1^T X_2 \vec{b_2})$ \\\\
\textbf{\textit{[\textbf{\textit{R code}}]}}
\begin{lstlisting}[language=R]

> b1 = ginv(t(X1) %*% X1) %*% (t(X1) %*% y - t(X1) %*% X2 %*% b2)
> 
> b1
         [,1]
[1,] 4.085714
> 
> X = cbind(X1, X2)
> 
> b = rbind(b1,b2)
> 
> t(X) %*% X %*% b 
     [,1]
[1,]   44
[2,]   16
[3,]   28
[4,]  248
> 
> t(X) %*% y
     [,1]
[1,]   44
[2,]   16
[3,]   28
[4,]  248


\end{lstlisting}

\vspace{2mm}

\hspace{-0.52cm}$ \vec{b_1} = 4.0857 $ \\\\

\hspace{-0.52cm}$ X^T X \vec{b} = X^T \vec y$
\end{document}
"
