#!/usr/bin/Rscript
library(datasets)
library(tidyverse)

beta <- 1
set.seed(261285)
rexp(20, 1/beta)