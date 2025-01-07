## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment = "#>", fig.width=6, fig.height=4, fig.align = "center")

## -----------------------------------------------------------------------------
library(vaccine)
set.seed(123)

## -----------------------------------------------------------------------------
data(hvtn505)

dat <- load_data(
  time = "HIVwk28preunblfu",
  event = "HIVwk28preunbl",
  vacc = "trt",
  marker = "IgG_V2",
  covariates = c("age","BMI","bhvrisk"),
  weights = "wt",
  ph2 = "casecontrol",
  data = hvtn505
)

## -----------------------------------------------------------------------------
summary_stats(dat)

## -----------------------------------------------------------------------------
est_overall(dat=dat, t_0=578, method="KM")
est_overall(dat=dat, t_0=578, method="Cox")

## -----------------------------------------------------------------------------
ests_cox <- est_ce(dat=dat, type="Cox", t_0=578)
ests_np <- est_ce(dat=dat, type="NP", t_0=578)

## -----------------------------------------------------------------------------
plot_ce(ests_cox, ests_np)

## -----------------------------------------------------------------------------
plot_ce(ests_cox, ests_np, density_type="kde", dat=dat)

## -----------------------------------------------------------------------------
ests_cox <- trim(ests_cox, dat=dat, quantiles=c(0.05,0.95))
ests_np <- trim(ests_np, dat=dat, quantiles=c(0.1,0.9))
plot_ce(ests_cox, ests_np, density_type="kde", dat=dat)

## -----------------------------------------------------------------------------
library(ggplot2)
my_plot <- plot_ce(ests_cox, ests_np, density_type="kde", dat=dat)

my_plot +
  labs(x="IgG Binding to V1V2") +
  scale_color_manual(labels = c("Cox model", "Nonparametric"),
                     values = c("darkorchid3", "deepskyblue3")) +
  scale_fill_manual(labels = c("Cox model", "Nonparametric"),
                    values = c("darkorchid3", "deepskyblue3"))

## -----------------------------------------------------------------------------
ests_table <- as_table(ests_cox, ests_np)
head(ests_table)

