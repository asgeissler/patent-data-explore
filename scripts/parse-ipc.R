#!/usr/bin/env Rscript
# Pre-process IPC XML into a more concise form for R
# - 1. Title list in lens like format
# - 2. hierarchy in IPC section-class-subclass-group format

library(tidyverse)
library(xml2)

################################################################################

dat <-
  'data/ipc-scheme.xml' |>
  read_xml()
  # xml_ns_strip()

xml_ns(dat)

# The here relevant XML scheme parts are
# Nested tags per class '/IPCScheme/ipcEntry/ipcEntry/...'
# with each ipcEntry containing as attributes: kind, symbol, entryType
# the human readable title in (multiple) 'ipcEntry/textBody/title/titlePart/text'

# all entries
all.entries <-
  dat |>
  xml_find_all('.//d1:ipcEntry', xml_ns(dat))

# extract path and title
all.entries.paths <-
  all.entries |>
  xml_path() |>
  unlist()
all.entries.titles <-
  all.entries |>
  map(xml_find_all, xpath = './d1:textBody/d1:title/d1:titlePart/d1:text',
      ns = xml_ns(dat)) |>
  map(map, xml_text) |>
  map(str_c, collapse = ';;') |>
  unlist()

# combine into single table
dat.tbl <-
  all.entries |>
  map(xml_attrs) |>
  map(as.list) |>
  bind_rows() |>
  mutate(
    path = all.entries.paths,
    title = all.entries.titles
  )

# match via path to parent node
dat.parent <-
  dat.tbl |>
  mutate(parent.path = str_remove(path, '/[^/]*$')) |>
  left_join(
    dat.tbl |>
      select(parent = symbol, parent.path = path),
    'parent.path'
  )

################################################################################
# Following the official scheme
# https://www.wipo.int/edocs/pubdocs/en/wipo-guide-ipc-2023-en-guide-to-the-international-patent-classification-2023.pdf
# make one clean table, that also resolve "dot" definitions and removed indices etc
# Purpose: Table with self-contained symbol-text-parent pairs

# All section class symbols etc
top.hierarchy <-
  dat.parent |>
  mutate(ns = nchar(symbol)) |>
  filter(ns < 14) |>
  mutate(
    hierarchy = case_when(
      ns == 1 ~ 'section',
      ns == 3 ~ 'class',
      ns == 4 ~ 'subclass'
    )
  ) %>%
  filter(title != '') %>%
  group_by(symbol, hierarchy, parent) |>
  summarize(title = str_c(title, collapse = ';;')) |>
  ungroup()


# extract main groups
mains <-
  dat.parent |>
  filter(nchar(symbol) == 14, str_detect(symbol, '000000$')) |>
  # filter(symbol == parent) |> count(title)
  # They are all empty titled -> remove
  filter(symbol != parent, title != '') |>
  group_by(symbol, parent) |>
  summarize(title = str_c(title, collapse = ';;')) |>
  ungroup()

# add higher hierarchy info
main.wide <-
  mains |>
  left_join(
    top.hierarchy,
    c('parent' = 'symbol'),
    suffix = c('', '.sub')
  ) |>
  left_join(
    top.hierarchy,
    c('parent.sub' = 'symbol'),
    suffix = c('', '.cls')
  ) |>
  left_join(
    top.hierarchy,
    c('parent.cls' = 'symbol'),
    suffix = c('', '.sect')
  ) |>
  transmute(
    symbol, title,
    subclass = paste(parent, title.sub, sep = '-'),
    class = paste(parent.sub, title.cls, sep = '-'),
    section = paste(parent.cls, title.sect, sep = '-')
  ) |>
  mutate(type = 'maingroup')


# sub groups
tmp.subs <-
  dat.parent |>
  filter(nchar(symbol) == 14, !str_detect(symbol, '000000$')) |>
  # filter(symbol == parent) |> count(title)
  # They are all empty titled -> remove
  filter(symbol != parent, title != '') |>
  group_by(symbol, parent) |>
  summarize(title = str_c(title, collapse = ';;')) |>
  ungroup()

# iterate to collect all names up to the main group for self-contained names
ext <-
  main.wide |>
  select(symbol, title) |>
  mutate(maingroup = symbol)

# iterativly add text of parents groups
repeat{
  print('iter')
  ext <-
    tmp.subs |>
    inner_join(ext, c('parent' = 'symbol')) |>
    unite('title', title.y, title.x, sep = ';;') |>
    select(- parent) |>
    bind_rows(ext)
  
  tmp.subs <-
    tmp.subs |>
    anti_join(ext, c('symbol'))
  
  if(nrow(tmp.subs) == 0) {
    break
  }
}

self.subs <-
  ext |>
  anti_join(main.wide, 'symbol') |>
  left_join(
    main.wide |>
      select(-title),
    c('maingroup' = 'symbol')
  ) |>
  mutate(type = 'subgrop')

################################################################################
# combine up and arrange in a nice form

ipc.full <-
  main.wide |>
  mutate(maingroup = symbol) |>
  bind_rows(self.subs) %>%
  arrange(section, class, subclass, maingroup, title)

################################################################################
# extra step:
# Encode full symbol to shorter format
# A01B0019000000 -> A01B19/00

ipc.shorter <-
  ipc.full |>
  select(symbol) |>
  mutate(
    # The levels are encoded in position "block" of variable lengths that are
    # 1 2 1 4 6 long
    ipc1 = str_extract(symbol, '^.'),
    ipc2 = str_extract(symbol, '(?<=^.).{2}'),
    ipc3 = str_extract(symbol, '(?<=^.{3}).'),
    ipc4 = str_extract(symbol, '(?<=^.{4}).{4}'),
    ipc5 = str_extract(symbol, '.{6}$'),
    # the  nice tag in lens has a '/' between group 4 and 5 with
    # leading/following '0's removed
    ipc.nice = paste0(
      ipc1, ipc2, ipc3, 
      str_remove(ipc4, '^0*'),
      '/',
      # exception, not all following 0s, because ipc5 has min length 2
      str_remove(ipc5, '0{0,4}$')
    )
  ) |>
  select(symbol, ipc = ipc.nice)

################################################################################
# wrap up and save for later ues

ipc <-
  ipc.shorter |>
  left_join(ipc.full, 'symbol') |>
  select(ipc, symbol, title, type, maingroup,
         subclass, class, section)

write_tsv(ipc, 'data/ipc.tsv.gz')