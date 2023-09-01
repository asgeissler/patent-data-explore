#!/usr/bin/env Rscript
# Pre-process Lens exports

library(tidyverse)

library(furrr)

# Parse argments from command line
# 1. no of threads
# 2. path input

args <- commandArgs(trailingOnly=TRUE)
xs <- as.integer(args[1])
path <- args[2]

# xs <- 4
# path <- 'downloaded-datasets/CRISPR'

plan(multicore, workers = xs)
dataset <- basename(path)

################################################################################
# Downloaded datasets from lens.org 
# The export is limited to 50k rows -> spread across multiple files

# 1. Patents in the dataset
lens.patents <-
  file.path(path, 'patents-*.csv.gz') |>
  Sys.glob() |>
  # head(n = 2) |>
  future_map(read_csv) |>
  bind_rows() |>
  # Depending on how the download collections were set, there might be an overlap
  group_by(`Lens ID`) |> 
  slice(1) |>
  ungroup()

# 2. Scholarly works cited in these patents
lens.works <-
  file.path(path, 'citations-*.csv.gz') |>
  Sys.glob() |>
  # head(n = 2) |>
  future_map(read_csv, col_types = 'c') |>
  map(mutate_all, as.character) |>
  bind_rows() |>
  # Depending on how the download collections were set, there might be an overlap
  group_by(`Lens ID`) |> 
  slice(1) |>
  ungroup()


################################################################################
# Create tidy tables

patents <- 
  lens.patents |>
  transmute(
    lens.id = `Lens ID`,
    title = Title, abstract = Abstract,
    date.publication = `Publication Date`,
    date.application = `Application Date`,
    date.priority = `Earliest Priority Date`,
    status = `Legal Status`,
    citing.patents = `Cited by Patent Count`
  ) |>
  unique()

ipc <- 
  lens.patents |>
  select(
    lens.id = `Lens ID`,
    ipc = `IPCR Classifications`
  ) |>
  separate_rows(ipc, sep = ';;') |>
  drop_na() |>
  filter(ipc != '') |>
  unique()

works <-
  lens.works |>
  select(
    lens.id = `Lens ID`,
    title = Title, 
    journal = `Source Title`,
    volume = Volume,
    issue = `Issue Number`,
    year = `Publication Year`,
    doi = DOI,
    authors = `Author/s`,
    abstract = Abstract,
    citations = `Citing Works Count`,
    patents = `Citing Patents Count`
  ) |>
  unique()

################################################################################
# Link patents to works

link.patent.works <-
  lens.patents |>
  select(
    lens.id = `Lens ID`,
    ref = `NPL Resolved Lens ID(s)`
  ) |>
  separate_rows(ref, sep = ';;') |>
  drop_na() |>
  filter(ref != '') |>
  inner_join(works, c('ref' = 'lens.id')) |>
  select(patent = lens.id, work = ref) |>
  unique()

  
################################################################################
# Write for analysis

write_tsv(patents,
          file.path('data', dataset, 'patents.tsv.gz'),
          quote = 'needed')
write_tsv(patents.ipc,
          file.path('data', dataset, 'ipc.tsv.gz'),
          quote = 'needed')
write_tsv(works,
          file.path('data', dataset, 'works.tsv.gz'),
          quote = 'needed')
write_tsv(link.patent.works,
          file.path('data', dataset, 'links-patent-work.tsv.gz'),
          quote = 'needed')

################################################################################