#' My first function
#'
#' @param a A numerical vector.
#' @param b Also a numerical vector.
#'
#' @return A numerical vector of a + b * a.
#' @export
#'
#' @examples
#' myfunc(3, 5)
myfunc <- function(a, b) {
  result <- a * b + a
  return(result)
}

#' Estimate Beta for Linear Regression
#'
#' @name estimate_beta
#' @param y A numerical vector of response values.
#' @param X A numerical matrix of predictor values.
#' @return A numerical vector of estimated beta coefficients.
#' @export
#'
#' @examples
#' y <- c(1, 2, 3, 4, 5)
#' X <- matrix(c(1, 1, 1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
#' estimate_beta(y, X)
estimate_beta <- function(y, X) {
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta_hat)
}


#' Custom ggplot2 Theme
#'
#' @name my_theme
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   my_theme()
my_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Helvetica", size = 14, color = "black"),
      panel.grid.major = ggplot2::element_line(color = "gray80"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16)
    )
}