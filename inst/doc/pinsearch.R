## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(pinsearch)
library(lavaan)
mod <-
    " visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9 "
# Output the final partial invariance model, and the noninvariant items
ps1 <-
    pinSearch(mod,
        data = HolzingerSwineford1939,
        group = "school", type = "residuals",
        inv_test = "mod"  # use modification indices for search (default)
    )

## -----------------------------------------------------------------------------
ps1$`Non-Invariant Items`

## -----------------------------------------------------------------------------
ps1$`Partial Invariance Fit` |>
    summary()

## -----------------------------------------------------------------------------
pinSearch(mod,
    data = HolzingerSwineford1939,
    group = "school", type = "residuals",
    inv_test = "mod",
    effect_size = TRUE
)

## -----------------------------------------------------------------------------
# Fit the partial strict model by hand
pstrict_fit <- cfa(mod, data = HolzingerSwineford1939,
                   group = "school",
                   group.equal = c("loadings", "intercepts",
                                   "residuals"),
                   group.partial = c("x3~1", "x7~1"))
# Effect sizes
pin_effsize(pstrict_fit)

## -----------------------------------------------------------------------------
mod2 <- "
   visual =~ x1 + x2 + x3 + x9
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
ps2 <-
    pinSearch(mod2,
        data = HolzingerSwineford1939,
        group = "school", type = "residuals",
        inv_test = "mod",
        effect_size = TRUE
    )
ps2

## -----------------------------------------------------------------------------
pinSearch(mod2,
    data = HolzingerSwineford1939,
    group = "school", type = "residuals",
    inv_test = "score",
    effect_size = TRUE
)

## -----------------------------------------------------------------------------
pinSearch(mod2,
    data = HolzingerSwineford1939,
    group = "school", type = "residuals",
    inv_test = "lrt",
    effect_size = TRUE
)

## -----------------------------------------------------------------------------
pinSearch(mod2,
    data = HolzingerSwineford1939,
    group = "school", type = "residuals",
    inv_test = "score",
    effect_size = TRUE,
    control_fdr = TRUE
)

