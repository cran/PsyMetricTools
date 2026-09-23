#' @title Modern Exploratory Factor Analysis
#' @description Performs exploratory factor analysis using lavaan with rotation.
#' @param n_factors Number of factors to extract.
#' @param n_items Number of items.
#' @param name_items Prefix for item names.
#' @param data Data frame containing the items.
#' @param apply_threshold Logical, whether to apply threshold to factor loadings.
#' @param estimator Estimator to use (default "WLSMV").
#' @param rotation Rotation method (default "oblimin").
#' @param exclude_items Items to exclude (default NULL).
#' @return A list containing fit indices, specifications, interfactor correlations, and pattern matrix.
#' @examples
#' \donttest{
#' # Create sample data with 15 Likert-type items
#' set.seed(123)
#' n <- 300
#' # Likert items (1-5) generated from 3 correlated latent factors, 5 items each
#' eta <- matrix(rnorm(n * 3), n, 3) %*% chol(0.7 * diag(3) + 0.3)
#' data_efa <- as.data.frame(sapply(1:15, function(j) as.numeric(cut(
#'   0.7 * eta[, ceiling(j / 5)] + rnorm(n, 0, 0.7),
#'   c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))))
#' names(data_efa) <- paste0("Item", 1:15)
#'
#' # Perform EFA with 3 factors using WLSMV estimator
#' result <- EFA_modern(
#'   n_factors = 3,
#'   n_items = 15,
#'   name_items = "Item",
#'   data = data_efa,
#'   apply_threshold = TRUE,
#'   estimator = "WLSMV",
#'   rotation = "oblimin"
#' )
#'
#' # Access fit indices
#' result$Bondades_Original
#'
#' # Access pattern matrix (factor loadings)
#' result$result_df
#'
#' # Access interfactor correlations
#' result$InterFactor
#'
#' # Example excluding specific items
#' result2 <- EFA_modern(
#'   n_factors = 3,
#'   n_items = 15,
#'   name_items = "Item",
#'   data = data_efa,
#'   apply_threshold = TRUE,
#'   estimator = "WLSMV",
#'   rotation = "oblimin",
#'   exclude_items = c("Item5", "Item10")
#' )
#' }
#' @export
EFA_modern <- function(n_factors,
                       n_items,
                       name_items,
                       data,
                       apply_threshold,
                       estimator = "WLSMV",
                       rotation = "oblimin",
                       exclude_items = NULL) {
  # Generar modelo para lavaan exploratorio
  modelos <- generate_modelos(n_factors = n_factors, n_items = n_items, name_items = name_items, exclude_items = exclude_items)

  # Especificación para lavaan exploratorio con rotación
  Specifications <- specification_models(modelos, data = data, estimator = estimator, rotation = rotation)

  # Bondades de ajuste para lavaan exploratorio
  Bondades_Original <- extract_fit_measures(Specifications)

  # Matriz patron de lavaan exploratorio
  result_df <- Standardized_solutions(Specifications[[n_factors]], name_items = name_items, apply_threshold = apply_threshold)

  # Correlación entre factores
  InterFactor <- lavaan::inspect(Specifications[[n_factors]], what = "std")$psi

  # Retornar todos los resultados
  return(list(
    Bondades_Original = Bondades_Original,
    Specifications = Specifications,
    InterFactor = InterFactor,
    result_df = result_df
  ))
}
