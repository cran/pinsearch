## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(lavaan)
library(pinsearch)

## -----------------------------------------------------------------------------
data(lui_sim)

## -----------------------------------------------------------------------------
summary(lui_sim)

## -----------------------------------------------------------------------------
config_mod <- "
f1 =~ class1 + class2 + class3 + class4 + class5 + class6 + 
      class7 + class8 + class9 + class10 + class11 + class12 + 
      class13 + class14 + class15
"
config_fit <- cfa(config_mod, data = lui_sim, group = "group", ordered = TRUE)
# Release covariance between class1 and class2
config_fit2 <- update(config_fit, c(config_mod, "class1 ~~ class2"))
anova(config_fit, config_fit2)

## -----------------------------------------------------------------------------
ps_path <- "ps_lui.rds"
if (!file.exists(ps_path)) {
    ps <- pinSearch(c(config_mod, "\nclass1 ~~ class2"),
        data = lui_sim,
        group = "group", ordered = TRUE,
        type = "residual.covariances"
    )
    saveRDS(ps, ps_path)
} else {
    ps <- readRDS(ps_path)
}

## -----------------------------------------------------------------------------
knitr::kable(ps[[2]])

## -----------------------------------------------------------------------------
# Obtain fmacs effect size
(f_omni <- pin_effsize(ps[[1]]))
# fmacs by gender
(f_gender <- pin_effsize(ps[[1]], group_factor = c(1, 1, 1, 2, 2, 2)))
# fmacs by ethnicity
(f_eth <- pin_effsize(ps[[1]], group_factor = c(1, 2, 3, 1, 2, 3)))
# interaction (using contrast matrix)
contr <- local({
    gen <- factor(c("F", "M"))
    contrasts(gen) <- contr.sum(length(gen))
    eth <- factor(1:3)
    contrasts(eth) <- contr.sum(length(eth))
    model.matrix(~ gen * eth, data = expand.grid(eth = eth, gen = gen))
})
(f_int <- pin_effsize(ps[[1]], contrast = contr[, 5:6, drop = FALSE]))

## -----------------------------------------------------------------------------
# Render as table
item_names <- gsub("class", replacement = "Item ", colnames(f_omni)) |>
    gsub(pattern = "-f1", replacement = "")
t(rbind(f_omni, f_gender, f_eth, f_int)) |>
    as.data.frame() |>
    `dimnames<-`(list(
        item_names,
        c("Overall", "Gender", "Ethnicity", "Gender x Ethnicity")
    )) |>
    knitr::kable(digits = 2, caption = "$f_\\text{MACS}$ effect sizes")

