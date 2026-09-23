#' Extract Fit Indices from Factor Analysis
#'
#' Extracts fit indices (RMSEA, TLI, CFI, BIC) from psych factor analysis results.
#'
#' @param factors_data A psych factor analysis result object.
#'
#' @return A data frame with fit index names and values.
#' @examples
#' \donttest{
#' library(psych)
#'
#' # Create sample data
#' set.seed(123)
#' n <- 300
#' # Likert items (1-5) generated from 2 correlated latent factors, 3 items each
#' eta <- matrix(rnorm(n * 2), n, 2) %*% chol(0.7 * diag(2) + 0.3)
#' data <- as.data.frame(sapply(1:6, function(j) as.numeric(cut(
#'   0.7 * eta[, ceiling(j / 3)] + rnorm(n, 0, 0.7),
#'   c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))))
#' names(data) <- paste0("Item", 1:6)
#'
#' # Run factor analysis with psych package
#' fa_result <- fa(data, nfactors = 2, rotate = "oblimin", fm = "pa")
#'
#' # Extract fit indices
#' fit_indices <- extract_fit(fa_result)
#' print(fit_indices)
#'
#' # The result contains: RMSEA, lower_RMSEA, upper_RMSEA,
#' # confidence, TLI, CFI, null.chisq, objective, BIC
#' }
#' @export
extract_fit <- function(factors_data) {
  # Verificar que el paquete requerido esté instalado
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' is required but not installed. Please install it with install.packages('psych')")
  }

  fa.CFI <- function(x) {
    nombre <- paste(x, "CFI", sep = ".")
    nombre <- ((x$null.chisq - x$null.dof) - (x$STATISTIC - x$dof)) / (x$null.chisq - x$null.dof)
    return(nombre)
  }
  cfi <- fa.CFI(factors_data)

  result <- data.frame(
    Bondades = c("RMSEA", "lower_RMSEA", "upper_RMSEA", "confidence", "TLI", "CFI", "null.chisq", "objective", "BIC"),
    values = c(
      factors_data[["RMSEA"]],
      factors_data[["lower"]][1],
      factors_data[["upper"]][1],
      factors_data[["lower"]][2],
      factors_data[["TLI"]],
      cfi,
      factors_data[["null.chisq"]],
      factors_data[["objective"]],
      factors_data[["BIC"]]
    )
  )
  return(result)
}

