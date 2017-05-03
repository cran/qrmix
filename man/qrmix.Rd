\name{qrmix}
\alias{qrmix}

\title{Quantile Regression Classification

}
\description{\code{qrmix} estimates the components of a finite mixture model by using quantile regression  to select a group of quantiles that satisfy an optimality criteria chosen by the user.

}
\usage{
qrmix(formula, data, k, Ntau=50, alpha=0.03, lossFn="Squared", fitMethod="lm",
xy=TRUE, ...)
}

\arguments{
  \item{formula}{an object of class \code{"formula"}.
}
  \item{data}{an optional data frame that contains the variables in \code{formula}.

  }
   \item{k}{number of clusters.

}
  \item{Ntau}{an optional value that indicates the number of quantiles that will be considered for quantile regression comparison. \code{Ntau} should be greater or equal than \eqn{2k}{2k}.

}
  \item{alpha}{an optional value that will determine the minimum separation between the k quantiles that represent each of the k clusters. \code{alpha} should be smaller than \eqn{\frac{1}{2k}}{1/(2k)}.

}
  \item{lossFn}{the loss function to be used to select the best combination of k quantiles. The available functions are \code{"Squared"}, \code{"Absolute"}, \code{"Bisquare"}, and \code{"Huber"}.

  }
   \item{fitMethod}{the method to be used for the final fitting. Use \code{"lm"} for OLS (default), \code{"rlm"} for robust regression, and \code{"rq"} to use fit from quantile regression.

  }
  \item{xy}{logical. If \code{TRUE} (the default), the data will be saved in the qrmix object.

  }
  \item{\dots}{additional arguments to be passed to the function determined in \code{fitMethod}.

}
}
\details{The optimality criteria is determined by the \code{lossFn} parameter. If, for example, the default value is used (\code{lossFn = "Squared"}), the \code{k} quantiles selected will minimize the sum of squared residuals. Use \code{"Bisquare"} or \code{"Huber"} to make the method less sensitive to outliers.

}
\value{
  \code{qrmix} returns an object of class "qrmix"

  \item{coefficients}{a matrix with k columns that represent the coefficients for each cluster.}
  \item{clusters}{cluster assignment for each observation.}
  \item{quantiles}{the set of k quantiles that minimize the mean loss.}
  \item{residuals}{the residuals, response minus fitted values.}
  \item{fitted.values}{the fitted values.}
  \item{call}{the matched call.}
  \item{xy}{the data used if xy is set to \code{TRUE}.}


}

\references{Emir, B., Willke, R. J., Yu, C. R., Zou, K. H., Resa, M. A., and Cabrera, J. (2017), "A Comparison and Integration of Quantile Regression and Finite Mixture Modeling" (submitted).

}

\examples{
data(blood.pressure)

#qrmix model using default function values:
mod1 = qrmix(bmi ~ ., data = blood.pressure, k = 3)
summary(mod1)

#qrmix model using Bisquare loss function and refitted with robust regression:
mod2 = qrmix(bmi ~ age + systolic + diastolic + gender, data = blood.pressure, k = 3,
Ntau = 25, alpha = 0.1, lossFn = "Bisquare", fitMethod = "rlm")
summary(mod2)



}
