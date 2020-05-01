<!-- README.md is generated from README.Rmd. Please edit that file -->
R package helda (HELpful functions for Data Analysis in R)
==========================================================

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/helda)](https://CRAN.R-project.org/package=helda)
[![Build
Status](https://travis-ci.com/Redcart/helda.svg?branch=master)](https://travis-ci.com/Redcart/helda)
[![Codecov test
coverage](https://codecov.io/gh/Redcart/helda/branch/master/graph/badge.svg)](https://codecov.io/gh/Redcart/helda?branch=master)
[![Documentation](https://www.rdocumentation.org/badges/version/helda)](https://www.rdocumentation.org/packages/helda)
[![License: GPL
v3](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/helda)](https://cranlogs.r-pkg.org/badges/grand-total/helda)
<!-- badges: end -->

Overview
--------

This package provides functionalities that aim at facilitating and
saving time when analysing data.

Installation
------------

You can install helda from CRAN by simply running:

``` r
install.packages("helda")
```

Development version
-------------------

To get a bug fix, or use a feature from the development version, you can
install helda from this GitHub repository.

``` r
# install.packages("devtools")
devtools::install_github("Redcart/helda")
```

Usage
-----

This is a quick introduction to the lift curve function of the package:

``` r
library(helda)

data_training <- titanic_training
data_validation <- titanic_validation

model_glm <- glm(formula = "Survived ~ Pclass + Sex + Age + 
                 SibSp + Fare + Embarked",
                 data = data_training,
                 family = binomial(link = "logit"))

predictions <- predict(object = model_glm, 
                       newdata = titanic_validation, 
                       type = "response")

plot <- lift_curve(predictions = predictions, 
                   true_labels = titanic_validation$Survived, 
                   positive_label = 1)

plot
```

<img src="man/figures/README-lift_curve-1.png" width="100%" />

Getting help
------------

If you encounter a clear bug, please file a minimal reproducible example
on the [issues section](https://github.com/Redcart/helda/issues) of the
repository.

Author
------

Simon Corde
