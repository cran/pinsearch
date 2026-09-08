## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE, echo = TRUE------------------------------------------------
# mod <- c(
#   "group: 1", "F =~ y1 + y2 + y3 + y4",         # e.g. the short form
#   "group: 2", "F =~ y1 + y2 + y3 + y4 + y5")    # e.g. the long form

## -----------------------------------------------------------------------------
library(lavaan)
library(pinsearch)
library(MASS, include.only = "mvrnorm")
set.seed(8)

lamS <- c(.90, .85, .80, .75)              # short form: y1-y4
lamL <- c(.90, .85, .50, .75, .70)         # long form:  y1-y5 (y3 weakened)
sigS <- tcrossprod(lamS) + diag(1 - lamS^2)
sigL <- tcrossprod(lamL) + diag(1 - lamL^2)

n  <- 500
gS <- mvrnorm(n, rep(0, 4), sigS)          # short form
gL <- mvrnorm(n, rep(0, 5), sigL)          # long form
dS <- as.data.frame(gS);  names(dS) <- paste0("y", 1:4)
dL <- as.data.frame(gL);  names(dL) <- paste0("y", 1:5)
df <- rbind(cbind(dS, y5 = NA, group = "short"),
            cbind(dL,          group = "long"))
df$group <- factor(df$group, levels = c("short", "long"))

## -----------------------------------------------------------------------------
mod <- c(
  "group: 1", "F =~ y1 + y2 + y3 + y4",          # short: no y5
  "group: 2", "F =~ y1 + y2 + y3 + y4 + y5")     # long:  full form
ps <- pinSearch(mod,
    data = df, group = "group",
    type = "intercepts"        # search loadings, then intercepts
)
ps$`Non-Invariant Items`

## -----------------------------------------------------------------------------
summary(ps$`Partial Invariance Fit`)

## -----------------------------------------------------------------------------
pinSearch(mod, data = df, group = "group",
          type = "intercepts", effect_size = TRUE)

