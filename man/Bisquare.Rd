\name{Bisquare}
\alias{Bisquare}

\title{Tukey's Bisquare Loss
}
\description{
\code{"Bisquare"} evaluates Tukey's Bisquare function defined as
\deqn{ f(r) = \left\{
\begin{array}{ll}
1-(1-(\frac{r}{c})^2)^3) & |r| \le c \\
1  & |r| > c
\end{array}
\right. }{f(r)=1-(1-(r/c)^2)^3) if |r|<=c}

\deqn{}{f(r)=1  if |r|>c }

}
\usage{
Bisquare(r, c = 4.685)
}

\arguments{
  \item{r }{a real number or vector.}
  \item{c }{a positive number. If the value is negative, it's absolute value will be used.}
}

\examples{

set.seed(1)
x = rnorm(200, mean = 3)
y = Bisquare(x)
plot(x, y)

}
