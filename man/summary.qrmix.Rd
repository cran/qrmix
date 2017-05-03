\name{summary.qrmix}
\alias{summary.qrmix}

\title{Summarizing qrmix Fits

}

\description{\code{summary} method for class \code{"qrmix"}

}
\usage{
\method{summary}{qrmix}(object, fitMethod=NULL, data=NULL, ...)
}

\arguments{
  \item{object}{an object of class \code{"qrmix"}.

}
  \item{fitMethod}{an optional refitting method if the user wants a method different than the one used to obtain \code{"object"} Use \code{"lm"} for OLS, \code{"rlm"} for robust regression, and \code{"rq"} to use fit from quantile regression.

}
  \item{data}{data used to fit \code{object} if it is not contained in \code{object}.

}
  \item{...}{other argumets passed to other methods.

}

}

\value{
  \item{residuals }{the residuals, response minus fitted values.}
  \item{clusters }{cluster assignment for each observation.}
  \item{call }{the matched call.}
  \item{fitMethod }{the fitting method used to obtain \code{residuals} and \code{clusters}.}
  \item{quantiles }{the set of k quantiles that minimize the mean loss.}
  \item{clusters# }{generic summary from function \code{fitMethod} for data in cluster #.}

}

\examples{

data(blood.pressure)

#qrmix model using default function values:
mod1 = qrmix(bmi ~ ., data = blood.pressure, k = 3)

#summary using fitMethod = "rlm" instead of the one used when fitting the model mod1
summary1 = summary(mod1, fitMethod = "rlm")

#Are the quantiles selected in this case the same as in the original model?
summary1$quantiles
mod1$quantiles



}
