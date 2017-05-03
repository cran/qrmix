\name{plot.qrmix}
\alias{plot.qrmix}



\title{Plot Method for a qrmix Object

}
\description{
Three types of plots (chosen with \code{type}) are currently available: density of the response variable by cluster, plots of the response variable against each covariate included in the model (scatterplots with the k fitted lines for continues variables and boxplots by cluster for the categorical variables), and boxplots of the residuals by cluster.

}
\usage{
\method{plot}{qrmix}(x, data = NULL, type = c(1,2,3), lwd = 2, bw = "SJ", adjust = 2, ...)

}

\arguments{
  \item{x }{a fitted object of class \code{"qrmix"}.

}
  \item{data }{the data used to fit the model \code{object}. It is only necessary when the when the parameter \code{xy} was set to FALSE when fitting the qrmix model.

}
  \item{type}{a numeric vector with values chosen from 1:3 to specify a subset of types of plots required.

}
  \item{lwd}{the line width for the first type of plot (density plot), a positive number. If a negative number is given, \code{lwd = 1} will be used instead. See \code{\link[graphics]{par}}.

}
  \item{bw}{the smoothing bandwidth to be used to obtain the density for the first type of plot. See \code{\link[stats]{density}} for details.

}
  \item{adjust}{the bandwidth used is adjust*bw. See \code{\link[stats]{density}} for details.

}
  \item{...}{other argumets passed to other methods.

}

}




\examples{

data(blood.pressure)

#qrmix model using default function values:
mod1 = qrmix(bmi ~ ., data = blood.pressure, k = 3)
plot(mod1)
plot(mod1, type = c(1,3), lwd = 1)

}

