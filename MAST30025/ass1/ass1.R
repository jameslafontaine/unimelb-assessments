library(MASS)
library(Matrix)
library(mvtnorm)

## Question 4 ##
muVec = matrix(c(1,2,4), 3, 1)
V = matrix(c(1, 0, 0, 0, 1, -1, 0, -1, 1), 3, 3)

# 4a

polyroot(c(0,-2,3,-1))

eigen = eigen(V)

evalues = eigen$values
evectors = eigen$vectors

# 4b
A = matrix(c(3,2,1), 1, 3)
muZ1 = A%*%muVec
varZ1 = A%*%V%*%t(A)

# 4c
B = matrix(c(1, 0, 0, 0, 1/2, 0, 0, 0, 1/2), 3, 3)
B%*%V
(B%*%V)%*%(B%*%V)
1/2*t(muVec)%*%B%*%muVec


## Question 5 ##

y = c(85,97,76,79,76,99,49,72,83)
X = matrix(c(rep(1,9),86,85,89,82,84,86,84,78,92), 9, 2)

# 5b
b = solve(t(X)%*%X, t(X)%*%y)

# 5c 
e = y-X%*%b
SSRes = sum(e^2)
s2 = SSRes/(length(y)-length(b))
sampleVar = as.vector(t(y-X%*%b)%*%(y-X%*%b))/(length(y)-length(b)) 
# estimate of the common variance of the response variables


# 5d
H = X%*%solve(t(X)%*%X)%*%t(X)
z = e / sqrt(sampleVar*(1-diag(H)))

# 5e

cooksDist = (1/length(b))*(z^2)*(diag(H)/(1-diag(H)))

# 5f

new = c(1,90)
c(1,90)%*%b

scores = data.frame(Specialist = y, General = X[,2])
model = lm(Specialist ~ General, data=scores)
newscore = data.frame(General=90)
predict(model,newscore)

## Question 5 without intercept ##

y = c(85,97,76,79,76,99,49,72,83)
X = matrix(c(86,85,89,82,84,86,84,78,92), 9, 1)

# 5b
b = solve(t(X)%*%X, t(X)%*%y)

# 5c 
e = y-X%*%b
SSRes = sum(e^2)
s2 = SSRes/(length(y)-length(b))
sampleVar = as.vector(t(y-X%*%b)%*%(y-X%*%b))/(length(y)-length(b)) 
# estimate of the common variance of the response variables


# 5d
H = X%*%solve(t(X)%*%X)%*%t(X)
z = e / sqrt(sampleVar*(1-diag(H)))

# 5e

cooksDist = (1/length(b))*(z^2)*(diag(H)/(1-diag(H)))

# 5f

new = c(1,90)
c(90)%*%b

scores = data.frame(Specialist = y, General = X)
model = lm(Specialist ~ General, data=scores)
newscore = data.frame(General=90)
predict(model,newscore)

# testing results
y1 = rnorm(1000, mean = 3, sd = 3)
y2 = rnorm(1000, mean = 4, sd = 2)
y3 = rnorm(1000, mean = 4, sd = 1)



hist(3*y1+2*y2+y3,freq=FALSE)


hist(y1^2 + ((y2+y3)/2)^2 + ((y2-y3)/2)^2, freq=FALSE)
lines(density(y1^2 + ((y2+y3)/2)^2 + ((y2-y3)/2)^2),col="blue", add=TRUE)
curve(dchisq(x, df = 2, ncp = 11), add=TRUE, col='red')
curve(dchisq(x, df = 0.3, ncp = 4), add=TRUE, col='green')

sim_data = mvrnorm(n = 1000, mu = muVec, Sigma = V)
y1 = sim_data[,1]
y2 = sim_data[,2]
y3 = sim_data[,3]

"
\documentclass{article}
\usepackage{graphicx} % Required for inserting images
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{listings}

\title{MAST30025 Linear Statistical Models 
Assignment 1}
\author{Student Code: 1079860 }
\date{March 2023}

\begin{document}


\maketitle










\newpage
\section*{\section*{Question 1}}
Since  $A$ is symmetric, there exists an orthogonal matrix $P$ $(\implies P^T = P^{-1})$ which diagonalises $A$ such that
\newline

\hspace{0.1cm} $P^TAP = D $ \\ 
$\implies PP^TAP = PD$ \\
$\implies AP = PD$ \\
$\implies APP^T = PD P^T$ \\
$\implies A = PD P^T$ \\
$\implies A^2 = PD P^TPD P^T = PDDP^T = PD^2 P^T$  
\newline
\newline
$D$ is a diagonal matrix with $A$'s eigenvalues of 0s and 1s on its diagonal. Therefore $D^2$ can be represented like so
$$D^2 = \begin{bmatrix} 
I & 0 \\
0 & 0
\end{bmatrix}
\begin{bmatrix}
I & 0 \\
0 & 0
\end{bmatrix}
= \begin{bmatrix}
I & 0 \\
0 & 0
\end{bmatrix} = D$$
\newline
$\implies D^2 = D$ 
\newline 
$\implies PD^2 P^T = PDP^T$
\newline
$\implies A^2 = A \implies$ $A\;is\; idempotent$

\newpage
\section*{\section*{Question 2}}
$A = A^2$, \hspace{0.1cm}$B = B^2$, \hspace{0.1cm}$A + B = (A + B)^2$
\subsection*{(a)}
$A + B = (A + B)^2 = A^2 + AB + BA + B^2$ \\
$\implies A = A^2 + AB + BA + B^2 -B$  \\
$\implies A = A^2 + AB + BA + B -B$  \\
$\implies A - A^2 = AB + BA$  \\
$\implies A - A = AB + BA$  \\
$\implies AB + BA = 0$ \\

\subsection*{(b)}
$A = PDP^T$, \hspace{0.1cm}$B = P \Lambda P^T$ \\\\
$so\;\;from\;\;(a)\;\; we\;\; have\;\;$ \\ \\
\hspace{0.1cm} $PDP^TP\Lambda P^T + P\Lambda P^TPDP^T = 0$ \\
$\implies PD\Lambda P^T + P\Lambda DP^T = 0$ \\
$\implies PD\Lambda P^T = -P\Lambda DP^T$ \\
$\implies D\Lambda  = -\Lambda D$ \\
$\implies \begin{bmatrix} 
I_r & 0 \\
0 & 0
\end{bmatrix}
\begin{bmatrix}
\Lambda_{11} & \Lambda_{12} \\
\Lambda_{21} & \Lambda_{22}
\end{bmatrix}
= \begin{bmatrix}
-\Lambda_{11} & -\Lambda_{12} \\
-\Lambda_{21} & -\Lambda_{22}
\end{bmatrix} 
\begin{bmatrix} 
I_r & 0 \\
0 & 0
\end{bmatrix} $ \\
$\implies \begin{bmatrix} 
I_r\Lambda_{11} + 0 & I_r\Lambda_{12}+0 \\
0 & 0
\end{bmatrix}
= \begin{bmatrix}
-\Lambda_{11}I_r + 0 & 0 \\
-\Lambda_{21}I_r+0 & 0
\end{bmatrix} $ \\
$\implies \begin{bmatrix} 
\Lambda_{11} & \Lambda_{12} \\
0 & 0
\end{bmatrix}
= \begin{bmatrix}
-\Lambda_{11} & 0 \\
-\Lambda_{21} & 0
\end{bmatrix} $ \\\\\\
$Therefore\;\;we\;\;have$ \\\\
$\Lambda_{11} = -\Lambda_{11} \implies \Lambda_{11} = 0$ \\
$\Lambda_{12} = 0$ \\
$-\Lambda_{21} = 0 \implies \Lambda_{21} = 0$ \\
\pagebreak
\subsection*{(c)}
$\begin{bmatrix} 
0 & 0 \\
0 & 0
\end{bmatrix}
= \begin{bmatrix} 
I_r & 0 \\
0 & 0
\end{bmatrix}
\begin{bmatrix}
\Lambda_{11} & \Lambda_{12} \\
\Lambda_{21} & \Lambda_{22}
\end{bmatrix}
= \begin{bmatrix} 
I_r & 0 \\
0 & 0
\end{bmatrix} 
\begin{bmatrix}
0 & 0 \\
0 & \Lambda_{22}
\end{bmatrix}
= \begin{bmatrix}
0 & 0 \\
0 & \Lambda_{22}
\end{bmatrix}
\begin{bmatrix} 
I_r & 0 \\
0 & 0
\end{bmatrix}
$ \\
\newline
$\implies 0 = D\Lambda = \Lambda D$ \\
$\implies 0 = D\Lambda P^T = \Lambda DP^T$ \\
$\implies 0 = PD\Lambda P^T = P\Lambda DP^T$ \\
$\implies 0 = PDP^TP\Lambda P^T = P\Lambda P^TPDP^T$ \\\\
$from\;\;(b)\;\;A = PDP^T\;\;and\;\;B = P\Lambda P^T$ \\\\
$\implies AB = BA = 0$ 
\newpage
\section*{\section*{Question 3}}
$var\;A\vec{y} = E[(A\vec{y}-A\vec{\mu})(A\vec{y}-A\vec{\mu})^T]$ \\
 $ = E[A(\vec{y}-\vec{\mu})(\vec{y}-\vec{\mu})^TA^T]$ \\
 $= A\:E[(\vec{y}-\vec{\mu})(\vec{y}-\vec{\mu})^T]\:A^T$ \\
 $= A\:(var\;\vec{y})A^T$ \\
 $\implies var\;A\vec{y} = A\:(var\;\vec{y})A^T$ 
 \newpage
\section*{\section*{Question 4}}
\subsection*{(a)}
$\begin{vmatrix} 
V-\lambda I
\end{vmatrix} = 0$ \\
$(1-\lambda) 
\begin{vmatrix} 
1-\lambda & -1 \\
-1 & 1-\lambda
\end{vmatrix} = 0$
$\implies (1-\lambda)((1-\lambda)^2 - 1) = 0$ \\
$\implies (1-\lambda)(1-2\lambda + \lambda^2 - 1) = 0$
$\implies (1-\lambda)(\lambda^2-2\lambda) = 0$ \\\\
$so\;\;we\;\;have$ \\\\
$1-\lambda = 0 \implies \lambda = 1$ \\\\
$or$  \\\\
$\lambda^2 - 2\lambda = 0 \implies \lambda(\lambda - 2) = 0 \implies \lambda = 0,2$ \\\\
$(V-\lambda I)\vec{x} = 0$ \\\\
$for\;\;\lambda = 0$ \\\\
$\begin{bmatrix} 
1 & 0 & 0\\
0 & 1 & -1\\
0 & -1 & 1
\end{bmatrix}
\begin{bmatrix}
x_1 \\
x_2 \\
x_3 
\end{bmatrix}
= \begin{bmatrix}
0 \\
0 \\
0 
\end{bmatrix}$ \\\\
$x_1 = 0$ \\ 
$x_2 - x_3 = 0 \implies x_2 = x_3$ \\
$ \\
\implies \vec{x} = t\begin{bmatrix}
0 \\
1 \\
1 
\end{bmatrix}$ \\\\
$where\;\;t \in \mathbb{R}\setminus\{0\}$ \\\\\\
$for\;\;\lambda = 1$ \\\\
$\begin{bmatrix} 
0 & 0 & 0\\
0 & 0 & -1\\
0 & -1 & 0
\end{bmatrix}
\begin{bmatrix}
x_1 \\
x_2 \\
x_3 
\end{bmatrix}
= \begin{bmatrix}
0 \\
0 \\
0 
\end{bmatrix}$ \\\\
$-x_2 = 0$ \\ 
$-x_3 = 0 $ \\
$\implies x_1 \neq 0\;\;as\;\;\vec{x} \neq 0$ \\
$ \\
\implies \vec{x} = t\begin{bmatrix}
1 \\
0 \\
0 
\end{bmatrix}$ \\\\
$where\;\;t \in \mathbb{R}\setminus\{0\}$ \\\\\\
$for\;\;\lambda = 2$ \\\\
$\begin{bmatrix} 
-1 & 0 & 0\\
0 & -1 & -1\\
0 & -1 & -1
\end{bmatrix}
\begin{bmatrix}
x_1 \\
x_2 \\
x_3 
\end{bmatrix}
= \begin{bmatrix}
0 \\
0 \\
0 
\end{bmatrix}$ \\\\
$-x_1 = 0$ \\ 
$-x_2 - x_3 = 0 \implies -x_2 = x_3$ \\
$ \\
\implies \vec{x} = t\begin{bmatrix}
0 \\
-1 \\
1 
\end{bmatrix}$ \\\\
$where\;\;t \in \mathbb{R}\setminus\{0\}$
\subsection*{(b)}
$z_1 = 3y_1 + 2y_2 + y_3$ \\\\
$ = \begin{bmatrix} 
3 & 2 & 1
\end{bmatrix} 
\begin{bmatrix} 
y_1 \\
y_2 \\
y_3
\end{bmatrix} $ = $A\vec{y}\;\;where\;\; A = \begin{bmatrix} 
3 & 2 & 1
\end{bmatrix} $ \\\\\\
$\implies z_1 \sim MVN(A\vec{\mu},\; AVA^T)$ \\\\
$A\vec{\mu} = 
\begin{bmatrix} 
3 & 2 & 1
\end{bmatrix} 
\begin{bmatrix} 
1 \\
2 \\
4 
\end{bmatrix}
= 3 + 4 + 4 = 11$ \\\\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $AVA^T$]}}
\begin{lstlisting}[language=R]
    A = matrix(c(3,2,1), 1, 3)
    V = matrix(c(1, 0, 0, 0, 1, -1, 0, -1, 1), 3, 3)
    varZ1 = A %*% V %*% t(A)
    > varZ1 = 10
\end{lstlisting}
$\implies AVA^T = 10$  \\ 
$\implies z_1 \sim MVN(11,\; 10)$ \\
$\implies z_1 \sim N(11,\; 10)$ \\\\
\subsection*{(c)}
$z_2 = y_1^2 + (\frac{y_2+y_3}{2})^2 + (\frac{y_2-y_3}{2})^2$ \\
$\;\; = y_1^2 + \frac{1}{4}y_2^2 + \frac{1}{2}y_2y_3 + \frac{1}{4}y_3^2 + \frac{1}{4}y_2^2 - \frac{1}{2}y_2y_3 + \frac{1}{4}y_3^2$ \\
$\;\; = y_1^2 + \frac{1}{2}y_2^2 + \frac{1}{2}y_3^2$ \\\\
$\implies z_2 = \vec{y}^TB\vec{y}\;\;where\;\;B = 
\begin{bmatrix} 
1 & 0 & 0 \\
0 & \frac{1}{2} & 0 \\
0 & 0 & \frac{1}{2}
\end{bmatrix} $ \\\\
$BV = 
\begin{bmatrix} 
1 & 0 & 0 \\
0 & \frac{1}{2} & 0 \\
0 & 0 & \frac{1}{2}
\end{bmatrix}
\begin{bmatrix} 
1 & 0 & 0 \\
0 & 1 & -1 \\
0 & -1 & 1
\end{bmatrix}$ \\\\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $BV$]}}
\begin{lstlisting}[language=R]
    B = matrix(c(1, 0, 0, 0, 1/2, 0, 0, 0, 1/2), 3, 3)
    V = matrix(c(1, 0, 0, 0, 1, -1, 0, -1, 1), 3, 3)
    B %*% V
   (B %*% V) %*% (B %*% V)
\end{lstlisting}  \\
$BV = (BV)^2 =
\begin{bmatrix} 
1 & 0 & 0 \\
0 & \frac{1}{2} & -\frac{1}{2} \\
0 & -\frac{1}{2}  & \frac{1}{2}
\end{bmatrix}$ \\\\
$\implies BV\;\;is\;\;idempotent\;\;and\;\;rank(BV)=tr(BV)=2\;\;as\;\;BV\;\;is\;\;idempotent\;\;and\;\;symmetric$ \\
$\implies By\;\;Theorem\;\;3.8,\;\; z_2 \sim \chi_{2,\frac{11}{2}}^2\;\;with\;\;\frac{11}{2} = \frac{1}{2}\vec{\mu}^TB\vec{\mu} = \lambda$ \\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $\lambda$]}}
\begin{lstlisting}[language=R]
    B = matrix(c(1, 0, 0, 0, 1/2, 0, 0, 0, 1/2), 3, 3)
    muVec = matrix(c(1,2,4), 3, 1)
    ncp = 1/2 * t(muVec) %*% B %*% muVec
\end{lstlisting}  
\newpage
\section*{\section*{Question 5}}
\subsection*{(a)}
$\vec{y} = X\vec{\beta} + \vec{\epsilon}$  \\\\
$\begin{bmatrix} 
85 \\
97 \\
76 \\
79 \\
76 \\
99 \\
49 \\
72 \\
83 \\
\end{bmatrix} = 
\begin{bmatrix} 
1 & 86 \\      
1 & 85 \\
1 & 89 \\
1 & 82 \\
1 & 84 \\
1 & 86 \\
1 & 84 \\
1 & 78 \\
1 & 92
\end{bmatrix} 
\begin{bmatrix} 
\beta_0 \\
\beta_1
\end{bmatrix} +
\begin{bmatrix} 
\epsilon_1 \\
\epsilon_2 \\
\epsilon_3 \\
\epsilon_4 \\
\epsilon_5 \\
\epsilon_6 \\
\epsilon_7 \\
\epsilon_8 \\
\epsilon_9 \\
\end{bmatrix} $
\subsection*{(b)}
$\vec{b} = (X^TX)^{-1}X^T\vec{y}$ \\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $\vec{b}$]}}
\begin{lstlisting}[language=R]
    y = c(85,97,76,79,76,99,49,72,83)
    X = matrix(c(rep(1,9),86,85,89,82,84,86,84,78,92), 9, 2)
    b = solve(t(X) %*% X, t(X) %*% y)
\end{lstlisting}  \\
$\vec{b} = 
\begin{bmatrix} 
-3.245 \\
0.973
\end{bmatrix}$
\subsection*{(c)}
$s^2 = \frac{(\vec{y} - X\vec{b})^T(\vec{y} - X\vec{b})}{n-(k+1)}$ \\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $s^2$]}}
\begin{lstlisting}[language=R]
    y = c(85,97,76,79,76,99,49,72,83)
    X = matrix(c(rep(1,9),86,85,89,82,84,86,84,78,92), 9, 2)
    b = solve(t(X) %*% X, t(X) %*% y)
    e = y-X %*% b
    SSRes = sum(e^2)
    s2 = SSRes/(length(y)-length(b))
    > s2 = 231.447
\end{lstlisting}  \\
$s^2 = 231.447$
\subsection*{(d)}

$z_i = \frac{e_i}{\sqrt{s^2(1-H_{ii})}}$ \\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $\vec{z}$]}} 
\begin{lstlisting}[language=R]
    y = c(85,97,76,79,76,99,49,72,83)
    X = matrix(c(rep(1,9),86,85,89,82,84,86,84,78,92), 9, 2)
    b = solve(t(X) %*% X, t(X) %*% y)
    e = y-X %*% b
    SSRes = sum(e^2)
    s2 = SSRes/(length(y)-length(b))
    H = X %*% solve(t(X) %*% X) %*% t(X)
    z = e / sqrt(sampleVar*(1-diag(H)))
\end{lstlisting}  \\
$\vec{z} = 
\begin{bmatrix} 
0.320 \\
1.224 \\
-0.550 \\
0.180 \\
-0.173 \\
1.300 \\
-2.066 \\
-0.060 \\
-0.298 \\
\end{bmatrix}$
\pagebreak
\subsection*{(e)}
$D_i = \frac{1}{k+1}z_i^2(\frac{H_{ii}}{1-H_{ii}}) $ \\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $\vec{D}$]}}
\begin{lstlisting}[language=R]
    y = c(85,97,76,79,76,99,49,72,83)
    X = matrix(c(rep(1,9),86,85,89,82,84,86,84,78,92), 9, 2)
    b = solve(t(X) %*% X, t(X) %*% y)
    e = y-X %*% b
    SSRes = sum(e^2)
    s2 = SSRes/(length(y)-length(b))
    H = X %*% solve(t(X) %*% X) %*% t(X)
    z = e / sqrt(sampleVar*(1-diag(H)))
    cooksDist = (1/length(b))*(z^2)*(diag(H)/(1-diag(H)))
\end{lstlisting}  \\
$\vec{D} = 
\begin{bmatrix} 
0.007 \\
0.094 \\
0.045 \\
0.004 \\
0.002 \\
0.112 \\
0.293 \\
0.002 \\
0.042 \\
\end{bmatrix}$ \\\\
\subsection*{(f)}
$Let\;\;\hat{y} = the\;\;point\;\;estimate\;\;for\;\;x_1 = 90$ \\\\
$Then\;\;\hat{y}= \vec{t}^T\vec{b} $ \\\\
$where\;\;\vec{t} =  
\begin{bmatrix} 
1 \\
90
\end{bmatrix}\;\;and\;\;\vec{b} = 
\begin{bmatrix} 
-3.245 \\
0.973
\end{bmatrix}$\\\\
\textbf{\textit{[\textbf{\textit{R code for calculating}} $\hat{y}$]}}
\begin{lstlisting}[language=R]
    y = c(85,97,76,79,76,99,49,72,83)
    X = matrix(c(rep(1,9),86,85,89,82,84,86,84,78,92), 9, 2)
    b = solve(t(X) %*% X, t(X) %*% y)
    > c(1,90) %*% b = 84.312
\end{lstlisting}  \\
$\hat{y} = 84.312$
\end{document}
"