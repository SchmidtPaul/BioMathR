#' @title Extract variance components
#'
#' @description This function extracts the estimated variance components of the random-effects terms in a linear mixed model. It is very heavily based on \href{https://m-clark.github.io/mixedup/reference/extract_vc.html}{\code{mixedup::extract_vc()}}.
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
#'   [nlme::VarCorr()],
#'   [mixedup::extract_vc()]
#'
#' @export
get_varcomp <- function(model,
                        digits = 3) {

  assertthat::assert_that(inherits(model, c("merMod", "glmmTMB")),
                          msg = "This is not a supported model class.")

  UseMethod("get_varcomp")
}

#' @export
#' @rdname get_varcomp
get_varcomp.merMod <- function(model,
                               digits = 3) {

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

  vc <- vc %>%
    dplyr::mutate(
      var_prop = var / sum(var),
      var_p = case_when(
        var_prop < 0.000001 ~ "   0%",
        TRUE ~ scales::percent(var_prop, accuracy = 0.1, trim = FALSE)
      ),
      effect = gsub(effect, pattern = "[\\(,\\)]", replacement = ""),
      effect = ifelse(is.na(effect), "", effect)
    )

  vc <- vc %>%
    dplyr::select("group", "effect", "var", "var_p", "var_prop", "sd",  everything())


  vc <- vc %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::mutate(effect = ifelse(effect == "", NA_character_, effect))

  # TODO: https://github.com/m-clark/mixedup/blob/f04aaea11b0fb760e3dbd171b20f3bd7f405f21f/R/extract_vc.R#LL178C17-L178C17

  return(vc)
}

#' @export
#' @rdname get_varcomp
get_varcomp.glmmTMB <- function(model,
                               digits = 3) {

  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
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

  vc <- vc %>%
    dplyr::mutate(
      var_prop = var / sum(var),
      var_p = case_when(
        var_prop < 0.000001 ~ "   0%",
        TRUE ~ scales::percent(var_prop, accuracy = 0.1, trim = FALSE)
      ),
      effect = gsub(effect, pattern = "[\\(,\\)]", replacement = ""),
      effect = ifelse(is.na(effect), "", effect)
    )

  vc <- vc %>%
    dplyr::select("group", "effect", "var", "var_p", "var_prop", "sd",  everything())

  vc <- vc %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::mutate(effect = ifelse(effect == "", NA_character_, effect))

  rownames(vc) <- NULL

  return(vc)
}
