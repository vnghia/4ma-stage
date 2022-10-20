## ---- init ----

library(reticulate)
library(ggplot2)
library(scico)
library(dplyr, quietly = T)
library(hrbrthemes, quietly = T)
library(tidyverse, quietly = T)
library(reshape2, quietly = T)

default_theme <- theme_ipsum(base_family = "") + theme(
  axis.title.x = element_text(hjust = 0.5),
  axis.title.y = element_text(hjust = 0.5),
  plot.margin = margin(
    t = 0.5,
    r = 2, b = 0.5, l = 2, "cm"
  ),
  legend.position = "bottom"
)

theme_set(default_theme)

knitr::opts_chunk$set(dev = "tikz", echo = F)
options(tikzDefaultEngine = "luatex")

np <- import("numpy")

## ---- 3x3-python ----

setNames(melt(
  np$load("binary/3x3-python-1e6.npy")
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 22) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 22, hjust = 0.5),
    axis.title.y = element_text(size = 22, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

## ---- 10x10-python ----

setNames(melt(
  np$load("binary/10x10-python-1e6.npy")
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 22) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 22, hjust = 0.5),
    axis.title.y = element_text(size = 22, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill")) +
  guides(fill = "none")

setNames(melt(
  np$load("binary/10x10-python-1e7.npy")
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 22) +
  theme(
    aspect.ratio = 1,
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 22, hjust = 0.5),
  ) +
  xlab("$Q_{2}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

## ---- 10x10-python-n-visit ----

n_visit <- np$load("binary/10x10-python-1e7-n-visit.npy")

setNames(melt(
  n_visit[, , 1]
), c("Q1", "Q2", "n_visit")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, n_visit = as.integer(n_visit)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = n_visit), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 22) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 22, hjust = 0.5),
    axis.title.y = element_text(size = 22, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  guides(fill = "none") +
  scale_fill_scico(palette = "grayC")

setNames(melt(
  n_visit[, , 2]
), c("Q1", "Q2", "n_visit")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, n_visit = as.integer(n_visit)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = n_visit), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 22) +
  theme(
    aspect.ratio = 1,
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 22, hjust = 0.5),
  ) +
  xlab("$Q_{2}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  labs(fill = "Number of visits") +
  scale_fill_scico(palette = "grayC")