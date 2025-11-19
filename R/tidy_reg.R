#' Summary of linear regression (for a time series)
#'
#' @param x A regression object created via \code{stats::lm()} or \code{openair::TheilSen()}
#' @param reg_dat The exact same dataset that was also provided when creating \code{x} above
#' @param n_preds Number of evenly spread out points on the x-axis for which predictions (including confidence interval) should be made
#' @param ... Other arguments passed to \code{broom::tidy()} from \code{lm} models.
#' @param conf.level The confidence level to use for the confidence interval
#' @param adjust_x0 If \code{TRUE} (default) the estimate and confidence limits for the intercept are adjusted so that it is no longer at \{openair\}'s default x = 1970, but instead at the minimum x present in the data.
#'
#' @export
tidy_reg <- function(x, reg_dat, n_preds = 10, ...){

  assertthat::assert_that(inherits(x, c("lm", "openair")),
                          msg = "This is not a supported model class.")

  UseMethod("tidy_reg")
}

#' @export
#' @rdname tidy_reg
tidy_reg.lm <- function(x, reg_dat, n_preds = 10, ..., conf.level = 0.95){

  assertthat::assert_that(requireNamespace("broom", quietly = TRUE),
                          msg = "When model object is 'lm', package 'broom' must be installed.")

  assertthat::assert_that(requireNamespace("insight", quietly = TRUE),
                          msg = "When model object is 'lm', package 'insight' must be installed.")

  assertthat::assert_that(length(x$fitted.values) == nrow(reg_dat),
                          msg = "x does not match reg_dat!\nlength(x$fitted.values) != nrow(reg_dat)")

  # avoid package check warning
  a <- conf.high <- conf.incl.0 <- conf.low <- conf.upp <- estimate <- .fitted <- int.conf.low <- int.conf.upp <- intercept <- intercept.lower <- lower <- intercept.upper <- meth <- name <- .resid <- slo.conf.low <- slo.conf.upp <- slope <- .std.resid <- term <- tmp <- upper <- value <- year <- NULL

  out <- list()

  # coefficients
  out$parms$long <- broom::tidy(x, conf.int = TRUE, conf.level = conf.level, ...) %>%
    rename(conf.upp = conf.high) %>%
    mutate(meth = "lm",
           term = if_else(str_detect(term, "ntercept"), "intercept", "slope"),
           conf.incl.0 = conf.low < 0 & conf.upp > 0) %>%
    select(meth, term, estimate, conf.low, conf.upp, conf.incl.0, everything())

  out$parms$wide <- out$parms$long %>%
    tidyr::pivot_longer(cols = -c(meth, term)) %>%
    mutate(term = str_sub(term, 1, 3)) %>%
    mutate(tmp = str_c(term,".",name), .keep = "unused") %>%
    tidyr::pivot_wider(names_from = tmp, values_from = value)

  # predictions
  rowids <- stats::quantile(
    1:nrow(reg_dat),
    probs = seq(0, 1, 1 / n_preds),
    na.rm = TRUE,
    names = FALSE
  )

  out$pred <- insight::get_predicted(x = x,
                                     data = reg_dat[round(rowids),],
                                     ci = conf.level)

  out$pred <- bind_cols(reg_dat[round(rowids),], as_tibble(out$pred))

  # residuals
  out$resid <- broom::augment(x) %>%
    rename(predicted = .fitted,
           resid = .resid,
           stdresid = .std.resid) %>%
    select(-contains("."))

  out$resid <- full_join(reg_dat, out$resid) %>% suppressMessages()

  return(out)
}

#' @export
#' @rdname tidy_reg
tidy_reg.openair<- function(x, reg_dat, n_preds = 10, ..., adjust_x0 = TRUE){

  assertthat::assert_that(requireNamespace("openair", quietly = TRUE),
                          msg = "When model object is 'openair', package 'openair' must be installed.")

  # avoid package check warning
  a <- conf.high <- conf.incl.0 <- conf.low <- conf.upp <- estimate <- .fitted <- int.conf.low <- int.conf.upp <- intercept <- intercept.lower <- lower <- intercept.upper <- meth <- name <- .resid <- slo.conf.low <- slo.conf.upp <- slope <- .std.resid <- term <- tmp <- upper <- value <- year <- NULL

  out <- list()

  # coefficients
  out$parms$wide <- x$data$res2 %>%
    as_tibble() %>%
    filter(!is.na(a)) %>%
    transmute(
      meth = "TheilSen",
      int.estimate = intercept,
      int.conf.low = intercept.lower,
      int.conf.upp = intercept.upper,
      slo.estimate = slope,
      slo.conf.low = lower,
      slo.conf.upp = upper
    )

  # Correct for default x0 being 1970 in openair::TheilSen()
  if (adjust_x0) {
    x0_year <- min(reg_dat$date) %>% format("%Y") %>% as.integer()
    adj <- out$parms$wide$slo.estimate * (x0_year - 1970)
    out$parms$wide <- out$parms$wide %>%
      mutate(across(contains("int"), ~ . + adj))
  }

  out$parms$wide <- out$parms$wide %>%
    mutate(int.conf.incl.0 = int.conf.low < 0 & int.conf.upp > 0, .after = "int.conf.upp") %>%
    mutate(slo.conf.incl.0 = slo.conf.low < 0 & slo.conf.upp > 0, .after = "slo.conf.upp")

  out$parms$long <- out$parms$wide %>%
    tidyr::pivot_longer(-meth) %>%
    mutate(term = str_sub(name, 1, 3),
           tmp = str_sub(name, 5,-1),
           .keep = "unused") %>%
    tidyr::pivot_wider(names_from = tmp,
                values_from = value) %>%
    mutate(
      term = if_else(term == "int", "intercept", "slope"),
      conf.incl.0 = as.logical(conf.incl.0)
    )

  # predictions
  rowids <- stats::quantile(
    1:nrow(x$data$main.data),
    probs = seq(0, 1, 1 / n_preds),
    na.rm = TRUE,
    names = FALSE
  )

  out$pred <- tibble(date = x$data$main.data[round(rowids), "date"]) %>%
    mutate(
      year = difftime(date, min(date), units = "days") %>% as.numeric() %>% (function(x) x / 365.25),
      Predicted = out$parms$wide$int.estimate + year * out$parms$wide$slo.estimate
    )

  # residuals # TODO: Does this collide with adjust_x0?
  # out$resid <- x$data$main.data %>%
  #   as_tibble() %>%
  #   transmute(date = date,
  #             obs = conc) %>%
  #   mutate(
  #     year = difftime(date, min(date), units = "days") %>% as.numeric() %>% \code{/}(365.25),
  #     predicted = out$parms$wide$int.estimate + year * out$parms$wide$slo.estimate
  #   ) %>%
  #   select(-year) %>%
  #   mutate(resid = obs - predicted)

  return(out)
}
