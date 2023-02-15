# library(BioMathR)
#
# library(lme4)
# mod_lmer <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#
# library(glmmTMB)
# mod_glmmTMB <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#
# get_varcomp(mod_lmer)
# get_varcomp(mod_glmmTMB)
#
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
