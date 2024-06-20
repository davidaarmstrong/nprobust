#' Neumayer-Plumper Robustness
#'
#' Calculate the Neumayer and Pluemper (2017) measure
#' of robustness for a robustness model relative to
#' a baseline model
#' @param base_mod A baseline model object
#' @param robust_mod An alternative robustness object
#' @param vbl A character string giving the variable
#' whose robustness is being tested.
#' @param type The quantity being tested - first difference "fd",  marginal effect "slope", or prediction "pred".
#' @param base_args Arguments to be passed to `avg_slopes()` or `avg_comparisons()` that govern the baseline model effects.
#' @param robust_args Arguments to be passed to `avg_slopes()` or `avg_comparisons()` that govern the baseline model effects; these should produce the same number of
#' effect estimates as the base_args specification does.
#' @param ... Other arguments to be passed down, not implemented.
#'
#' @returns A data frame that contains the baseline effect, SE and 95% CI along with the robustness calculation.
#'
#' @importFrom marginaleffects avg_comparisons avg_slopes datagrid
#' @importFrom stats pnorm
#' @importFrom dplyr as_tibble select
#' @export
np_robust <- function(base_mod,
                      robust_mod,
                      vbl,
                      base_args = list(),
                      robust_args = list(),
                      type = c("fd", "slope", "pred"),
                      ...){
  type <- match.arg(type)
  base_args$model <- base_mod
  base_args$variables <- vbl
  robust_args$model <- robust_mod
  robust_args$variables <- vbl
  if(type == "fd"){
    b_comps <- suppressWarnings(do.call(avg_comparisons, base_args))
    b_rob <- suppressWarnings(do.call(avg_comparisons, robust_args))
  }
  if(type == "slope"){
    b_comps <- suppressWarnings(do.call(avg_slopes, base_args))
    b_rob <- suppressWarnings(do.call(avg_slopes, robust_args))
  }
  if(type == "pred"){
    b_comps <- suppressWarnings(do.call(avg_predictions, base_args))
    b_rob <- suppressWarnings(do.call(avg_predictions, robust_args))
  }
  rob <- pnorm(b_comps$conf.high, b_rob$estimate, b_rob$std.error) -
    pnorm(b_comps$conf.low, b_rob$estimate, b_rob$std.error)
  res <- select(b_comps, c(1:std.error,  "conf.low", "conf.high"))
  res$robust<- rob
  as_tibble(res)
}
