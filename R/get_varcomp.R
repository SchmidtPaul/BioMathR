#' @title Extract variance components
#'
#' @description This function extracts the estimated variance components of a linear (mixed) model. It is compatible with models from \code{lme4}, \code{glmmTMB}, and \code{lm} functions. The resulting output is a tibble that contains the variance components, their proportion of the total variance, and standard deviations. This function is heavily based on \code{mixedup::extract_vc()}. Use \code{docx_tab()} to format output for reports.
#'
#' @param model a fitted model object
#' @param digits Rounding. Default is 3.
#'
#' @import dplyr
#' @import tibble
#' @importFrom scales percent
#'
#' @seealso
#'   [lme4::VarCorr()],
#'   [glmmTMB::VarCorr()],
#'   [mixedup::extract_vc()](https://m-clark.github.io/mixedup/reference/extract_vc.html),
#'   [BioMathR::docx_tab()]
#'
#' @examples
#' # Simple linear model
#' lm_model <- lm(mpg ~ wt + hp, data = mtcars)
#' get_varcomp(lm_model)
#'
#' # Format for reports using docx_tab()
#' \dontrun{
#' get_varcomp(lm_model) %>% docx_tab()
#' get_varcomp(lm_model) %>% docx_tab(lang = "ger")
#'
#' # Mixed model with formatted output
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   mm_model <- lme4::lmer(mpg ~ wt + hp + (1 | cyl), data = mtcars)
#'   get_varcomp(mm_model) %>% docx_tab(lang = "ger")
#' }
#' }
#'
#' @export
get_varcomp <- function(model,
                        digits = 3) {

  # return NULL if model is NULL
  if (is.null(model)) {
    return(NULL)
  }

  assertthat::assert_that(inherits(model, c("lm", "merMod", "glmmTMB")),
                          msg = "This is not a supported model class.")

  UseMethod("get_varcomp")
}


# internal helper functions -----------------------------------------------
# obtain percentages
varcomp_percentages <- function(vc) {

  var <- NULL # avoid package check warning

  vc <- vc %>%
    dplyr::mutate(
      var_prop = var / sum(var),
      var_p = dplyr::case_when(
        var_prop < 0.000001 ~ "   0%",
        TRUE ~ scales::percent(var_prop, accuracy = 0.1, trim = TRUE)
      )
    )
  return(vc)
}

# format final table
varcomp_formatter <- function(vc, digits) {

  effect <- NULL # avoid package check warning

  vc <- vc %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., digits = digits))) %>%
    dplyr::mutate(effect = gsub(effect, pattern = "[\\(,\\)]", replacement = "")) %>%
    dplyr::mutate(effect = ifelse(is.na(effect) | effect == "", NA_character_, effect)) %>%
    dplyr::relocate("group", "effect", "var", "var_p", "var_prop", "sd", .before = 1)

  rownames(vc) <- NULL

  return(vc)
}


# methods for different model classes -------------------------------------
#' @export
#' @rdname get_varcomp
get_varcomp.lm <- function(model, digits = 3) {

  effect <- NULL # avoid package check warning

  # Extract the residual variance
  resvar <- summary(model)$sigma^2

  # Create the data frame
  vc <- data.frame(
    group = "Residual",
    effect = NA,
    var = resvar,
    sd = sqrt(resvar),
    stringsAsFactors = FALSE
  )

  vc <- varcomp_percentages(vc)
  vc <- varcomp_formatter(vc, digits)

  return(vc)
}


#' @export
#' @rdname get_varcomp
get_varcomp.merMod <- function(model, digits = 3) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("When model object is 'merMod', package 'lme4' must be installed.")
  }

  group <- effect <- effect_2 <- var <- sd <- NULL # avoid package check warning

  vc_mat <- lme4::VarCorr(model)
  vc <- data.frame(vc_mat)
  colnames(vc) <- c("group", "effect", "effect_2", "var", "sd")

  vc <- vc %>%
    data.frame() %>%
    dplyr::filter(is.na(effect) | is.na(effect_2))

  vc <- varcomp_percentages(vc)
  vc <- varcomp_formatter(vc, digits)

  # TODO: https://github.com/m-clark/mixedup/blob/f04aaea11b0fb760e3dbd171b20f3bd7f405f21f/R/extract_vc.R#LL178C17-L178C17

  return(vc)
}

#' @export
#' @rdname get_varcomp
get_varcomp.glmmTMB <- function(model, digits = 3) {

  if (!suppressWarnings(requireNamespace("glmmTMB", quietly = TRUE))) {
    stop("When model object is 'glmmTMB', package 'glmmTMB' must be installed.")
  }

  group <- effect <- effect_2 <- var <- sd <- NULL

  vc_mat <- glmmTMB::VarCorr(model)[["cond"]]
  variance <- purrr::map(vc_mat, diag)

  variance <- data.frame(
    group  = rep(names(variance), purrr::map_int(variance, length)),
    effect = unlist(purrr::map(variance, names)),
    var    = unlist(variance)
  )

  if (model$modelInfo$family$family == "gaussian") {
    resvar = data.frame(
      group  = "Residual",
      effect = "",
      var    = attr(vc_mat, "sc")^2
    )

    variance <- rbind(variance, resvar)
  }

  vc <- data.frame(variance, sd = sqrt(variance$var))

  vc <- varcomp_percentages(vc)
  vc <- varcomp_formatter(vc, digits)

  return(vc)
}
