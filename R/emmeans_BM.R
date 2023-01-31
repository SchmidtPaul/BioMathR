#' @title Get adjusted means with compact letter display
#'
#' @description This function is a wrapper for \code{emmeans::emmeans()} and \code{multcomp::cld()} (\href{https://schmidtpaul.github.io/dsfair_quarto/summaryarticles/compactletterdisplay.html}{details here}). Based on a given linear model object, it computes estimated marginal means (a.k.a. least-squares means, adjusted means, modelbased means) for specified factors or factor combinations as well as comparisons or contrasts among them. Moreover, it adds the compact letter display to the comparisons/contrasts. Finally, its output is a list with six elements: for the means and their diffs, respectively, (i) a tibble with the results, (ii) the same tibble but in a print-ready format and (iii) an info text summarizing the results.
#'
#' @param model a linear model object. See the \href{https://cran.r-project.org/web/packages/emmeans/vignettes/models.html}{list of supported model classes}
#' @param specs_string a character vector specifying the names of the predictors over which emmeans are desired
#' @param lmer.df degrees of freedom method. See the \href{https://cran.r-project.org/web/packages/emmeans/vignettes/sophisticated.html#lmerOpts}{list of available methods}
#' @param adjust \href{https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html#adjust}{multiplicity adjustment}. Follow \href{https://www.rdocumentation.org/packages/emmeans/versions/1.6.2-1/topics/summary.emmGrid}{this link} and scroll down o to the “P-value adjustments” heading within the “summary.emmGrid” section for more details on e.g. Fisher’s LSD test, Tukey-test, Bonferroni adjustment etc.
#' @param out_lang language for the formatted result table and text
#' @param num_accuracy format the number of digits shown after the decimal separator
#' @param sort sort the levels of the predictors given in \code{specs_string} in the output
#'
#' @details \href{https://github.com/rvlenth/emmeans/issues/235}{Here} is a discussion on the speed of calculating confidence intervals with different degrees of freedom methods.
#'
#' @export
#'
#' @import dplyr
#' @import emmeans
#' @import multcomp
#' @import multcompView
#' @import stringr
#' @import tibble
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_reorder
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom graphics pairs
#' @importFrom insight format_p
#' @importFrom scales number
#' @importFrom stats formula
#'
#' @examples
#' library(BioMathR)
#' pigsmod <- lm(conc ~ source, data = emmeans::pigs)
#'
#' emmeans_BM(model = pigsmod, specs_string = "~ source", out_lang = "eng")
#'
#' emmeans_BM(model = pigsmod, specs_string = "~ source", out_lang = "ger")

emmeans_BM <- function(model,
                       specs_string,
                       lmer.df = c("Asymptotic", "Satterthwaite", "Kenward-Roger")[3],
                       adjust = c("none", "tukey", "bonferroni")[1],
                       out_lang = c("ger", "eng")[1],
                       num_accuracy = 0.01,
                       sort = c("none", "asc", "desc")[1]) {

  renames <- list(
    ger =
      c(
        "Mittelwert" = "emmean",
        "FG" = "df",
        "Untere Grenze KI" = "lower.CL",
        "Obere Grenze KI" = "upper.CL",
        "CLD" = ".group",
        "Kontrast" = "contrast",
        "Differenz" = "estimate",
        "p-Wert" = "p.value",
        "Term" = "term",
        "95%-Vertrauensint." = "CI",
        "Fishers LSD Test" = "none",
        "Tukey-Test" = "tukey",
        "Fishers LSD Test mit Bonferroni-Korrektur" = "bonferroni"
      ),
    eng =
      c(
        "Mean" = "emmean",
        "DF" = "df",
        "Lower CL" = "lower.CL",
        "Upper CL" = "upper.CL",
        "CLD" = ".group",
        "Contrast" = "contrast",
        "Difference" = "estimate",
        "p-value" = "p.value",
        "Term" = "term",
        "95% Conf. Int." = "CI",
        "Fisher's LSD Test" = "none",
        "Tukey's test" = "tukey",
        "Fisher's LSD Test with bonferroni correction" = "bonferroni"
      )
  )

  emm <- model %>%
    emmeans::emmeans(
      specs = stats::formula(specs_string),
      lmer.df = lmer.df, # degrees-of-freedom method
      infer = c(TRUE, FALSE) # get CI but not p-value (test: mean = 0)
    )

  diffs <- emm %>%
    graphics::pairs(
      adjust = adjust, # p-value adjustment
      infer = c(TRUE, TRUE) # get CI and p-value (test: diff = 0)
    ) %>%
    as_tibble() %>%
    rename("SED" = "SE") %>%
    select(-all_of("t.ratio"))

  means <- emm %>%
    multcomp::cld(adjust = adjust, # p-value adjustment
                  Letters = letters) %>%
    tibble::as_tibble() %>%
    rename("SEM" = "SE") %>%
    dplyr::mutate(across(all_of(".group"), ~ stringr::str_trim(.x)))


  # column order ------------------------------------------------------------
  varcolumn <- specs_string %>%
    stringr::str_remove("~") %>%
    stringr::str_extract("[^|]+") %>% # 1 or more characters other than |
    stringr::str_trim()

  bycolumns <- c(names(means), names(diffs)) %>%
    .[. %not_in% renames[[out_lang]]] %>%
    .[. %not_in% varcolumn] %>%
    .[. %not_in% c("SEM", "SED")] %>%
    unique()

  diffs <- diffs %>%
    dplyr::select(all_of(bycolumns), everything())

  means <- means %>%
    dplyr::select(all_of(bycolumns), all_of(varcolumn), everything())


  # sort --------------------------------------------------------------------
  if (sort != "none") {
    desc <- sort == "desc"

    if (length(bycolumns) == 0) {
      diffs <- diffs %>%
        dplyr::mutate(across(
          .cols = "contrast",
          .fns = ~ forcats::fct_reorder(
            .f = .x,
            .x = estimate,
            .desc = desc
          )
        )) %>%
        dplyr::arrange(contrast)

      means <- means %>%
        dplyr::mutate(across(
          .cols = all_of(varcolumn),
          .fns = ~ forcats::fct_reorder(
            .f = .x,
            .x = emmean,
            .desc = desc
          )
        )) %>%
        dplyr::arrange(across(all_of(varcolumn)))
    }

    if (length(bycolumns) == 1) {
      diffs <- diffs %>%
        dplyr::mutate(across(
          .cols = bycolumns,
          .fns = ~ forcats::fct_reorder(
            .f = .x,
            .x = estimate,
            .desc = desc
          )
        )) %>%
        dplyr::arrange(across(all_of(bycolumns)))

      means <- means %>%
        dplyr::mutate(across(
          .cols = bycolumns,
          .fns = ~ forcats::fct_relevel(.f = .x, levels(diffs[[bycolumns]]))
        )) %>%
        dplyr::arrange(across(all_of(bycolumns)))
    }

    if (length(bycolumns) > 1) {
      stop("sort does not work for more than one by-columns")
    }

  }


  # print format ------------------------------------------------------------
  emm_out <- list()
  emm_out$means <- means
  emm_out$diffs <- diffs

  for (i in names(emm_out)) {
    temp <- emm_out[[i]] %>%
      dplyr::mutate(across(
        .cols = where(is.numeric) & !contains("p.value"),
        .fns = ~ scales::number(.x, accuracy = num_accuracy),
        .names = "{.col}_fmt"
      )) %>%
      dplyr::mutate(
        CI_fmt = glue::glue("[{lower.CL_fmt}; {upper.CL_fmt}]"),
        .keep = "unused",
        .after = "df_fmt"
      ) %>%
      dplyr::mutate(across(
        .cols = any_of("p.value"),
        .fns = ~ insight::format_p(.x, stars = TRUE),
        .names = "{.col}_fmt"
      ))

    # exception: df do not need trailing 0s due to accuracy
    while (all(stringr::str_detect(temp$df_fmt, "0$"))) {
      temp$df_fmt <- stringr::str_remove(temp$df_fmt, "0$")

      if (all(stringr::str_detect(temp$df_fmt, "\\.$"))) {
        temp$df_fmt <- stringr::str_remove(temp$df_fmt, "\\.$")
      }
    }

    if (out_lang == "ger") {
      temp <- temp %>%
        dplyr::mutate(across(where(is.character), ~
                        stringr::str_replace_all(.x, "\\.", "\\,")))
    }

    emm_out[[i]] <- temp

    emm_out[[paste0(i, "_print")]] <- temp %>%
      dplyr::select(
        any_of(bycolumns),
        any_of(varcolumn),
        any_of("contrast"),
        contains("fmt"),
        any_of(".group")
      ) %>%
      dplyr::rename_all( ~ stringr::str_remove(.x, "_fmt")) %>%
      dplyr::rename(any_of(renames[[out_lang]]))
  }

  # Infotext ----------------------------------------------------------------
  PERSTATEMENT <- glue::glue_collapse(bycolumns, sep = ", ", last = " und ")
  if (length(bycolumns)== 0) {PERSTATEMENT <- ""}

  if (out_lang == "ger") {

    emm_out[["means_info"]] <- glue::glue(
      "Modellbasierte Mittelwerte: {varcolumn} {PERSTATEMENT} \\
      (FG nach {lmer.df} Methode). Es gilt {PERSTATEMENT} dass Mittelwerte,
      die keinen gemeinsamen CLD-Buchstaben aufweisen, sich laut \\
      {renames[[out_lang]] %>% .[. == adjust] %>% names()} \\
      statistisch signifikant voneinander unterscheiden (5% Signifikanzniveau). \\
      Abk.: Standard Error of the Mean (SEM); Freiheitsgrade (FG); Compact Letter Display (CLD)"
    ) %>% stringr::str_squish()

    emm_out[["diffs_info"]] <- glue::glue(
      "Modellbasierte Mittelwertvergleiche: {varcolumn} {PERSTATEMENT} \\
      (FG nach {lmer.df} Methode). Kontraste mit einem p-Wert < 0.05 sind laut \\
      {renames[[out_lang]] %>% .[. == adjust] %>% names()} \\
      statistisch signifikant, d.h. die Differenz zwischen den jeweiligen Mittelwerten \\
      ist statistisch signifikant verschieden von 0. \\
      Abk.: Standard Error of Difference (SED); Freiheitsgrade (FG); Compact Letter Display (CLD)"
    ) %>% stringr::str_squish()
  }

  if (out_lang == "eng") {

    PERSTATEMENT <- stringr::str_replace(PERSTATEMENT, " und ", " and ")

    emm_out[["means_info"]] <- glue::glue(
      "Modelbased means for {varcolumn} {PERSTATEMENT} \\
      (df according to {lmer.df} method). \\
      Regarding the CLD, it is true that {PERSTATEMENT} \\
      means not sharing any letter are significantly different according to the \\
      {renames[[out_lang]] %>% .[. == adjust] %>% names()} \\
      at the 5% level of significance. \\
      Abb.: Standard Error of the Mean (SEM); Degrees of Freedom (DF); Compact Letter Display (CLD)"
    ) %>% stringr::str_squish()

    emm_out[["diffs_info"]] <- glue::glue(
      "Modelbased mean comparisons for {varcolumn} {PERSTATEMENT} \\
      (df according to {lmer.df} method). \\
      Contrasts with a p-value < 0.05 are statistically significant accordint to the \\
      {renames[[out_lang]] %>% .[. == adjust] %>% names()} \\
      at the 5% level of significance, i.e. the difference between the respective means \\
      is significantly different from 0. \\
      Abb.: Standard Error of the Mean (SEM); Degrees of Freedom (DF); Compact Letter Display (CLD)"
    ) %>% stringr::str_squish()

  }

  return(emm_out)

}








