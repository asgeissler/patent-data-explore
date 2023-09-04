# Helper script to load data and soem libraries
# (shared between scripts)
################################################################################
library(tidyverse)
library(ggpubr)
library(cowplot)
library(patchwork)
library(corrplot)

# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

# load tsv and add dataset name from path
helper <- function(path) {
  path |>
    read_tsv() |>
    mutate(dataset = path |> dirname() |> basename())
}

patents <-
  'data/*/patents.tsv.gz' |>
  Sys.glob() |>
  map(helper) |>
  bind_rows() |>
  mutate(
    priority.year = ifelse(
      is.na(date.priority),
      date.application, date.priority) |>
      as_date() |>
      year()
  )
works <-
  'data/*/works.tsv.gz' |>
  Sys.glob() |>
  map(helper) |>
  map(mutate_at, 'volume', as.character) |>
  bind_rows()
links <-
  'data/*/links-patent-work.tsv.gz' |>
  Sys.glob() |>
  map(helper) |>
  bind_rows()
ipc <-
  'data/*/ipc.tsv.gz' |>
  Sys.glob() |>
  map(helper) |>
  bind_rows()

ipc.titles <- read_tsv('data/ipc.tsv.gz')