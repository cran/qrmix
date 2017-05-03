\name{Huber}
\alias{Huber}

\title{Huber Loss

}
\description{Evaluates the Huber loss function defined as \deqn{ f(r) = \left\{
\begin{array}{ll}
\frac{1}{2}|r|^2 & |r| \le c \\
c(|r|-\frac{1}{2}c)  & |r| > c
\end{array}
\right. }{f(r)=(1/2)*r^2 if |r|<=c}

\deqn{}{f(r)=c*(|r|-(1/2)*c) if |r|>c }

}
\usage{
Huber(r, c = 1.345)
}

\arguments{
  \item{r }{a real number or vector.

}
  \item{c }{a positive number. If the value is negative, it's absolute value will be used.

}
}



\examples{

set.seed(1)
x = rnorm(200, mean = 1)
y = Huber(x)
plot(x, y)
abline(h = (1.345)^2/2)

}

