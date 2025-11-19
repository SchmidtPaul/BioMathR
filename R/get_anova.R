#' @title Obtain analysis of variance table
#'
#' @description This function obtains the ANOVA table for a model.
#'
#' @param model a fitted model object. Supported classes: \code{"lm"}, \code{"lmerModLmerTest"}, \code{"glmmTMB"}, \code{"gls"}, \code{"lme"}.
#' @param type Type of ANOVA test: \code{"I"}, \code{"II"} or \code{"III"} (default).
#' @param ... additional arguments to be passed to the specific anova function.
#' @param info Logical, if \code{TRUE}, information about the type of ANOVA and
#'   test statistic used is printed to the console. Default is \code{FALSE}.
#' @param suppressWarnings Logical; if \code{TRUE}, suppresses warnings from \code{car::Anova()}. Default is TRUE.
#'
#' @details The function utilizes \code{car::Anova()} for all supported model
#' classes and defaults to type III of sum of squares. For models of class
#' 'lm' and 'lmerModLmerTest', the F-test is employed. In contrast, for models of the
#' 'glmmTMB', 'gls', and 'lme' class, the Chi-Square test is used since the
#' F-test is
#' \href{https://github.com/glmmTMB/glmmTMB/blob/bd1932addbfb4edf2ce933675f5a8bf72abc0e7c/glmmTMB/R/Anova.R#L73C8-L73C8}{currently
#' unavailable}. It's worth noting that only the 'lmerModLmerTest' models support (and
#' default to) the Kenward-Roger method as a degrees of freedom method.
#'
#' \describe{
#'   \item{\code{lm} (Package: stats):}{Types I/II/III; F-Test; Standard df.}
#'   \item{\code{lmer/lmerTest} (Packages: lme4/lmerTest):}{Types I/II/III; F-Test; Kenward-Roger (KR) df.}
#'   \item{\code{lme} (Package: nlme):}{Types I/II/III; Chi-Square Test (Chisq*); Standard df.}
#'   \item{\code{gls} (Package: nlme):}{Types I/II/III; Chi-Square Test (Chisq*); Standard df.}
#'   \item{\code{glmmTMB} (Package: glmmTMB):}{Types I/II/III; Chi-Square Test; Standard df.}
#' }
#'
#' * You can obtain the F-Test only for type I/II via \code{stats::anova()}
#'
#' The \code{type} argument specifies the type of sum of squares to be used in the analysis:
#' \describe{
#'   \item{Type I (Sequential) sum of squares:}{
#'     The order in which factors are entered into the model does matter. Each factor is adjusted for the factors listed before it.
#'   }
#'   \item{Type II (Marginal) sum of squares:}{
#'     The order in which factors are entered into the model does not matter. Each factor is adjusted for all of the other factors in the model.
#'   }
#'   \item{Type III sum of squares:}{
#'     The order in which factors are entered into the model does not matter, similar to Type II. However, each factor is adjusted for all
#'     of the other factors as well as for itself, which allows for the testing of each factor in the presence of interactions.
#'   }
#' }
#'
#' Kenward-Roger Degrees of Freedom: The Kenward-Roger (KR) method is
#' a sophisticated approach to approximating the degrees of freedom in mixed
#' models, particularly in the presence of small sample sizes or unbalanced
#' data. It is not applicable to non-mixed models. Unlike the classical
#' degrees of freedom methods which may overestimate the significance of
#' effects, the KR approximation tends to provide a more conservative and
#' accurate estimation. This method adjusts the degrees of freedom to account
#' for the complexity of the mixed model structure, thereby enhancing the
#' robustness of the resulting inference. The Kenward-Roger method is
#' especially beneficial when working with complex models that include
#' multiple random effects and/or nested structures, as it helps to mitigate
#' the risk of Type I errors, offering a more reliable foundation for
#' hypothesis testing. Importantly, employing the KR method is never
#' disadvantageous when compared to using the default method; it provides a
#' more accurate reflection of the model's complexity and the data structure,
#' thus leading to more reliable statistical inferences.
#'
#' @seealso
#'   [car::Anova()],
#'   [lmerTest::lmer()],
#'   [glmmTMB::glmmTMB()],
#'   [nlme::gls()],
#'   [nlme::lme()]
#'
#' @export
get_anova <- function(model, type = "III", ..., info = FALSE, suppressWarnings = TRUE) {

  # return NULL if model is NULL
  if (is.null(model)) {
    return(NULL)
  }

  assertthat::assert_that(
    inherits(model, c("lm", "lmerMod", "lmerModLmerTest", "glmmTMB", "gls", "lme")),
    msg = "This is not a supported model class."
  )

  model_class <- class(model)[1]  # Extract the primary class name of the model

  if (model_class == "lmerMod" && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Please fit your model via lmerTest::lmer() instead of lme4::lmer().")
  }

  test_statistic <- switch(
    model_class,
    lm = "F",
    lmerModLmerTest = "F",
    glmmTMB = "Chisq",
    gls = "Chisq",
    lme = "Chisq",
    stop("Unsupported model class")
  )

  ddf <- if (model_class == "lmerModLmerTest") "Kenward-Roger" else NULL

  necessary_package <- switch(
    model_class,
    lm = "stats",
    lmerModLmerTest = c("lmerTest", "lme4"),
    glmmTMB = "glmmTMB",
    gls = "nlme",
    lme = "nlme",
    NULL
  )

  if (!is.null(necessary_package)) {
    for (pkg in necessary_package) {
      assertthat::assert_that(
        requireNamespace(pkg, quietly = TRUE),
        msg = sprintf("When model object is '%s', package '%s' must be installed.", model_class, pkg)
      )
    }
  }

  # If suppressWarnings is TRUE, suppress warnings from car::Anova
  if (suppressWarnings) {
    anova_table <- suppressWarnings(car::Anova(model, type = type, test.statistic = test_statistic, ddf = ddf, ...))
  } else {
    anova_table <- car::Anova(model, type = type, test.statistic = test_statistic, ddf = ddf, ...)
  }

  # Get an info summary above the table
  anova_summary <- sprintf(
    "The ANOVA performed is of Type %s, utilizing a %s test statistic.",
    type,
    test_statistic
  )

  if (!is.null(ddf)) {
    anova_summary <- paste(anova_summary, sprintf("Degrees of freedom were adjusted using the %s method.", ddf))
  }

  # Output the summary only if info = TRUE
  if (info) {
    cat(paste0(anova_summary, "\n"))
  }

  return(anova_table)
}
