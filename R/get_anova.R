#' @title Obtain analysis of variance table
#'
#' @description This function obtains the ANOVA table for a model.
#'
#' @param model a fitted model object. Supported classes: \code{"lm"}, \code{"lmerMod"} & \code{"glmmTMB"}.
#' @param type Type of ANOVA test: \code{"I"}, \code{"II"} or \code{"III"} (default).
#' @param ... additional arguments to be passed to the specific anova function.
#'
#' @details The function utilizes \code{car::Anova()} for all supported model classes. For models of class 'lm' and 'lmerMod', the F-test is employed. In contrast, for models of the 'glmmTMB' class, the Chi-Square test is used since the F-test is \href{https://github.com/glmmTMB/glmmTMB/blob/bd1932addbfb4edf2ce933675f5a8bf72abc0e7c/glmmTMB/R/Anova.R#L73C8-L73C8}{currently unavailable}. It's worth noting that only the 'lmerMod' models support (and default to) the Kenward-Roger method as a degrees of freedom method.
#'
#' @seealso
#'   [car::Anova()],
#'   [lmerTest::lmer()],
#'   [glmmTMB::glmmTMB()]
#'
#' @export
get_anova <- function(model, type, ...) {
  assertthat::assert_that(inherits(model, c("lm", "lmerMod", "glmmTMB")),
                          msg = "This is not a supported model class.")

  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' must be installed.")
  }

  UseMethod("get_anova")
}


#' @export
#' @rdname get_anova
get_anova.lm <- function(model,
                         type = c("I", "II", "III")[3],
                         ...) {

  anova_table <- car::Anova(model, type, ...)
  return(anova_table)
}


#' @export
#' @rdname get_anova
get_anova.lmerMod <- function(model,
                              type = c("I", "II", "III")[3],
                              ...) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("When model object is 'lmerMod', package 'lme4' must be installed.")
  }

  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("When model object is 'lmerMod', package 'lmerTest' must be installed.")
  }

  anova_table <- car::Anova(model, type = type, test.statistic = "F", ...)
  return(anova_table)
}

#' @export
#' @rdname get_anova
get_anova.glmmTMB <- function(model,
                              type = c("I", "II", "III")[3],
                              ...) {

  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("When model object is 'glmmTMB', package 'glmmTMB' must be installed.")
  }

  anova_table <- car::Anova(model, type = type, test.statistic = "Chisq", ...)
  return(anova_table)
}
