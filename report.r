## ---- init ----

library(reticulate)
library(ggplot2)
library(hrbrthemes)
library(scales)
library(tidyr)
library(viridis)

theme_set(theme_ipsum(base_family = "") +
  theme(axis.title.x = element_text(
    hjust = 0.5
  ), axis.title.y = element_text(
    hjust = 0.5
  ), plot.margin = margin(
    t = 0.5, r = 2, b = 0.5, l = 2, "cm"
  )))

knitr::opts_chunk$set(dev = "tikz")
options(tikzDefaultEngine = "luatex")