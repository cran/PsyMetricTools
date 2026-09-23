#' Create Factor Analysis Summary
#'
#' Creates a summary table with loadings, communalities, and uniquenesses.
#'
#' @param factors_data A factor analysis result object with loadings.
#' @param num_items Number of items in the analysis.
#' @param num_factors Number of factors extracted.
#'
#' @return A data frame with items, factor loadings, h2, and u2.
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
#' # Run EFA with psych package
#' efa_result <- fa(data, nfactors = 2, rotate = "oblimin", fm = "pa")
#'
#' # Create summary table
#' summary_table <- factor_summary(
#'   factors_data = efa_result,
#'   num_items = 6,
#'   num_factors = 2
#' )
#' print(summary_table)
#' # Output:
#' #   Items   F1    F2   h2   u2
#' # 1 Item1 0.75  0.10 0.58 0.42
#' # 2 Item2 0.68  0.05 0.47 0.53
#' # ...
#' }
#' @export
factor_summary <- function(factors_data, num_items, num_factors) {
  factor_names <- paste0("F", 1:num_factors)
  column_names <- c(factor_names, "h2", "u2")

  result <- data.frame((factors_data$loadings)[1:num_items,],
                       factors_data$communalities,
                       factors_data$uniquenesses) %>%
    stats::setNames(., column_names) %>%
    round(2) %>%
    tibble::rownames_to_column(var = "Items")

  return(result)
}
