#!/usr/bin/env Rscript
# Nice figure and table to get an overview over the datasets

source('scripts/helper_load.R')

################################################################################
# Patents per year

p.patents <-
  patents |>
  count(dataset, priority.year, status) |>
  ungroup() |>
  left_join(
    patents |>
      count(dataset, name = 'total'),
    'dataset'
  ) |>
  mutate(
    nice = sprintf(
      '%s dataset\nPatents in total: %s',
      dataset,
      prettyNum(total, big.mark = ',')
    )
  ) |>
  ggplot(aes(priority.year, n, fill = status)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(values = cbPalette, name = 'Patent status') +
  xlab('Year of earliest priority') +
  ylab('No. patents') +
  facet_wrap(~ nice, scale = 'free') +
  theme_pubr(18) +
  theme(legend.position = 'bottom')


################################################################################
# Work per year

p.works <-
  works |>
  filter(year >= 1950) |>
  count(dataset, year) |>
  ungroup() |>
  left_join(
    works |>
      count(dataset, name = 'total'),
    'dataset'
  ) |>
  mutate(
    nice = sprintf(
      '%s dataset\nScholarly works in total: %s',
      dataset,
      prettyNum(total, big.mark = ',')
    )
  ) |>
  ggplot(aes(year, n)) +
  geom_bar(stat = 'identity') +
  xlab('Year of publication') +
  ylab('No. scholarly works') +
  facet_wrap(~ nice, scale = 'free_y') +
  labs(caption =  'Scholarly works (journal articles, book chapters, ...) before 1950 are hidden)') +
  theme_pubr(18)


################################################################################
# Venn diagrams of works and patents

pv.patents <-
  patents |>
  group_by(dataset) |>
  do(i = list(.$lens.id)) |>
  with(set_names(i, dataset)) |>
  map(1) |>
  venn::venn(zcolor = 'style', box = FALSE, sncs = 2,
             ilcs = 2, ggplot = TRUE) +
  coord_cartesian() +
  ylim(c(200, 800)) +
  theme_pubr(18) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank()) +
  ggtitle('Patents shared by datasets')

pv.works <-
  works |>
  group_by(dataset) |>
  do(i = list(.$lens.id)) |>
  with(set_names(i, dataset)) |>
  map(1) |>
  venn::venn(zcolor = 'style', box = FALSE, sncs = 2,
             ilcs = 2, ggplot = TRUE) +
  coord_cartesian() +
  ylim(c(200, 800)) +
  theme_pubr(18) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank()) +
  ggtitle('Scholarly works shared by datasets')


################################################################################
# Overlap on top 1000

pv.top <-
  links |>
  count(work, dataset) |>
  group_by(dataset) |>
  slice_max(n, n = 1000) |>
  do(i = list(.$work)) |>
  with(set_names(i, dataset)) |>
  map(1) |>
  venn::venn(zcolor = 'style', box = FALSE, sncs = 2,
             ilcs = 2, ggplot = TRUE) +
  coord_cartesian() +
  ylim(c(200, 800)) +
  theme_pubr(18) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank()) +
  ggtitle('Top 1000 patent-referenced works',
          '(incl. ties, works with equal number of references)')

################################################################################
# Single combined plot of overview

((p.patents  / p.works) | (pv.patents / pv.works/ pv.top)) +
  plot_layout(widths = c(2, 1.5)) +
  plot_annotation(tag_levels = 'A', theme = theme_pubr(18))

ggsave('analysis/overview-datasets.png',
       width = 16, height = 12, dpi = 400)

################################################################################
# top 5 journals

works |>
  pull(journal) |>
  unique() |>
  length() -> no.journal
top <- 5
other <- sprintf('Remaining %s journals', prettyNum(no.journal - top, big.mark = ','))

dat <-
  works |>
  mutate_at('journal', fct_lump, top, other_level = other) |>
  mutate_at('journal', replace_na, other) |>
  count(dataset, journal) |>
  arrange(desc(n)) |>
  mutate_at('journal', str_remove, '\\(New York.*$') |>
  mutate_at('journal', str_remove, 'of the United States of America') |>
  # line breaks after four words
  mutate_at('journal', str_replace,
            '(?<=[^ ]{1,20} [^ ]{1,20} [^ ]{1,20} [^ ]{1,20}) ',
            '\n') |>
  mutate_at('journal', fct_inorder) |>
  mutate(
    # offset for label
    x2 = journal |> as.integer(),
    x2 = x2 + ifelse(dataset == 'CRISPR', -1, +1) * .2
  )

p.journals <-
  dat |>
  ggplot(aes(journal, n, label = prettyNum(n, big.mark = ','), fill = dataset)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = cbPalette[2:3]) +
  geom_label(aes(x = x2), size = 4, nudge_y = .3, show.legend = FALSE) +
  scale_y_log10() +
  annotation_logticks(sides = 'l') +
  xlab('Journal') +
  ylab('No. patent-referenced\nscholarly works') +
  geom_text(
    aes(x2, 2, label = j2, fill = NULL),
    data = dat |>
      group_by(journal) |>
      summarize_at('n', max) |>
      mutate(
        j2 = str_replace(journal, '\n', '\n\n'),
        x2 = as.integer(journal) - ifelse(str_detect(j2, '\n'), 0, 0.2)
      ),
    angle = 90, hjust = 0,
    size = 6,
    show.legend = FALSE
  ) +
  theme_pubr(18) +
  theme(
    # axis.text.x = element_text(angle = 50, hjust = 1),
    axis.text.x = element_blank(),
    legend.position = 'bottom'
  )
################################################################################

links |>
  select(- dataset) |>
  unique() |>
  count(work, name = 'patents') |>
  left_join(
    works |>
      select(lens.id, citations) |>
      unique(),
    c('work' = 'lens.id')
  ) -> dataset.pat.refs

p.scatter <-
  dataset.pat.refs |>
  mutate_at(c('citations', 'patents'),
            ~ .x + 1) |>
  ggplot(aes(citations, patents)) +
  # geom_point(alpha = 0.5) +
  geom_hex(bins = 30) +
  scale_fill_viridis_c(name = 'No. works in bin',
                       guide = guide_colorbar(barwidth = 20)) +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_cor(color = 'red', size = 5) +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10(labels = scales::comma) +
  xlab('No. patents refering to publication') +
  ylab('No. citations in scholarly articles') +
  labs(caption = '(incl. pseudo-count +1)') +
  theme_pubr(18) +
  theme(legend.position = 'bottom')

################################################################################
links |>
  count(dataset, work) |>
  group_by(dataset) |>
  slice_max(n, n = 5) |>
  arrange(desc(n)) |>
  rename(lens.id = work) |>
  left_join(
    works |>
      select(- dataset) |>
      unique(),
    'lens.id'
  ) -> foo

p.top.works <-
  foo |>
  select(- c(lens.id, abstract)) |>
  mutate_at('journal', str_remove, ' \\(New York.*$') |>
  mutate_at(c('citations', 'patents'), prettyNum, big.mark = ',') |>
  mutate_at(c('volume', 'issue'), as.character) |>
  mutate_at(c('volume', 'issue'), replace_na, '') |>
  mutate_at('authors', str_replace, ';.*$', ', et al.') |>
  mutate_at('title', str_replace, '(?<=.{40}).*', '...') |>
  select(
    Dataset = dataset,
    'References\nin dataset' = n,
    'References\nglobally' = patents,
    'Citations' = citations,
    Title = title,
    Authors = authors,
    'Journal/Publisher' = journal,
    Year = year,
    DOI = doi,
  ) |>
  unique() |>
  gridExtra::tableGrob(rows = NULL, 
                       theme = gridExtra::ttheme_default(14)) |>
  plot_grid() +
  theme_pubr(18) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank())
  # plot_annotation(title = 'Top 5 works per referencing patents per dataset')

################################################################################

((p.journals + p.scatter ) / p.top.works) +
  plot_layout(heights = c(1.5, 1)) +
  plot_annotation(tag_levels = 'A')

ggsave('analysis/overview-journals.png',
       width = 21, height = 12, dpi = 400)

################################################################################


