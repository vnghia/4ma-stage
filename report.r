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

knitr::opts_chunk$set(dev = "tikz", echo = F, cache = T)
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
    axis.title.x = element_text(size = 22, hjust = 0.5),
    axis.title.y = element_text(size = 22, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

## ---- 10x10-python-n-visit ----

n_visit <- np$load("binary/10x10-python-1e7-n-visit.npy")
n_visit[n_visit == 0] <- NA

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
  scale_fill_scico(palette = "grayC", na.value = NA)

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
    axis.title.x = element_text(size = 22, hjust = 0.5),
    axis.title.y = element_text(size = 22, hjust = 0.5),
  ) +
  xlab("$Q_{1}$") +
  ylab("$Q_{2}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  labs(fill = "Number of visits") +
  scale_fill_scico(palette = "grayC", na.value = NA)

## ---- 20x20-simple-queuing-1 ----

q_20_20 <- np$load("binary/20x20-simple-queuing-1.npy")

setNames(melt(
  q_20_20
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  guides(fill = "none") +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

setNames(melt(
  q_20_20
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  guides(fill = "none") +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

setNames(melt(
  q_20_20
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

## ---- 20x20-simple-queuing-2 ----

q_20_20 <- np$load("binary/20x20-simple-queuing-2.npy")

setNames(melt(
  q_20_20
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  guides(fill = "none") +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

setNames(melt(
  q_20_20
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  guides(fill = "none") +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

setNames(melt(
  q_20_20
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

# ---- 20x20-simple-lb-1 ----

setNames(melt(
  np$load("binary/20x20-simple-lb-1-q-on.npy")
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  guides(fill = "none") +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

setNames(melt(
  np$load("binary/20x20-simple-lb-1-q-off.npy")
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  guides(fill = "none") +
  scale_colour_manual(values = c("white", "black"), aesthetics = c("fill"))

setNames(melt(
  np$load("binary/20x20-simple-lb-1-v.npy")
), c("Q1", "Q2", "queue")) %>%
  mutate(Q1 = Q1 - 1, Q2 = Q2 - 1, queue = factor(as.integer(queue) + 1)) %>%
  ggplot(aes(Q1, Q2)) +
  geom_tile(aes(fill = queue), colour = "grey10", size = 0.001) +
  coord_flip() +
  theme_void(base_size = 33) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 33, hjust = 0.5),
    axis.title.y = element_text(size = 33, hjust = 0.5),
  ) +
  ylab("$Q_{2}$") +
  xlab("$Q_{1}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  scale_colour_manual(
    values = c("white", "black", "grey50"),
    aesthetics = c("fill"), label = c("1", "2", "neutral")
  )

## ---- 20x20-simple-lb-1-n-visit ----

n_visit <- np$load("binary/20x20-simple-lb-1-n-visit.npy")
n_visit[n_visit == 0] <- NA

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
  scale_fill_scico(palette = "grayC", na.value = NA)

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
    axis.title.x = element_text(size = 22, hjust = 0.5),
    axis.title.y = element_text(size = 22, hjust = 0.5),
  ) +
  xlab("$Q_{1}$") +
  ylab("$Q_{2}$") +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_discrete(position = "right", expand = c(0.1, 0.1)) +
  labs(fill = "Number of visits") +
  scale_fill_scico(palette = "grayC", na.value = NA)
