#!/usr/bin/env Rscript
# Identify trend changes of interst from Markov chain analysis
# Then check for enrichment by publications

source('scripts/helper_load.R')

library(pheatmap)

mc.chain <- read_tsv('analysis/markov-chain.tsv.gz')

################################################################################
# Identify the 'interesting' changes

mc.interest <-
  mc.chain |>
  filter(p.step <= 1e-10, change >= 100) |>
  group_by(dataset, ipc) |>
  summarize(min.p = min(p.step)) |>
  group_by(dataset) |>
  slice_min(min.p, n = 10) |>
  ungroup() |>
  left_join(ipc.titles, 'ipc') |>
  mutate(
    title = case_when(
      ipc == 'C12N15/11' ~ 'Recombinant DNA-technology',
      ipc == 'A23L25/00' ~ 'Preparation or treatment of nutmeat/seed food',
      TRUE ~ title
    ),
    nice = sprintf(
      '%s (%s)',
      title |>
        str_remove('^.*;;(?=[A-Z])')  |>
        str_replace('(?<=^.{50}).{4}.*$', '...'),
      ipc
    )
  )

################################################################################
# Plot heatmap for the 'interesting' Top 10 trends


# Matrix of log counts for significant IPCs
xs <-
  mc.interest |>
  select(dataset, ipc, nice) |>
  left_join(ipc, c('ipc', 'dataset')) |>
  left_join(patents, c('dataset', 'lens.id')) |>
  count(dataset, nice, priority.year) |>
  spread(priority.year, n, fill = 0)

assertthat::are_equal(xs |> count(nice) |> count(n) |> nrow(), 1)

xs.mat <-
  xs |>
  select_if(is.numeric) |>
  as.matrix() |>
  magrittr::set_rownames(xs$nice)

colors <-
  list(
    dataset = xs |>
      pull(dataset) |>
      unique() |>
      sort() |>
      set_names(x = cbPalette[2:3])
  )

apply(xs.mat, 1, scale) |> max()

pheatmap(
  xs.mat,
  scale = 'row',
  cluster_cols = FALSE,
  annotation_row = with(
    xs,
    data.frame(dataset = dataset, row.names = nice)
  ),
  annotation_colors = colors,
  color = rev(colorRampPalette(RColorBrewer::brewer.pal(11, 'RdBu'))(99)),
  legend_breaks = c(seq(-6, 6, 2), 4.5),
  legend_labels = c(as.character(seq(-6, 6, 2)), "row z-score\n"),
  filename = 'analysis/trend-heatmap.png',
  width = 12,
  height = 8
)
#dev.off()
################################################################################
# Enrichment test for papers predominately present in trend

# Counts for part of fisher test
x.total <-
  patents |>
  count(dataset, name = 'dataset.patents')

x.trend <-
  mc.interest |>
  select(dataset, nice, ipc) |>
  inner_join(ipc, c('dataset', 'ipc')) |>
  count(dataset, nice, name = 'trend.patents')

x.work.trend <-
  mc.interest |>
  select(dataset, nice, ipc) |>
  inner_join(ipc, c('dataset', 'ipc')) |>
  inner_join(links, c('dataset', 'lens.id' = 'patent'),
             relationship = 'many-to-many') |>
  count(dataset, work, nice, name = 'work.trend.references')

x.work.total <-
  x.work.trend |>
  select(work, dataset) |>
  unique() |>
  inner_join(links, c('dataset', 'work')) |>
  count(dataset, work, name = 'work.total')

# Combine and run fisher test
enrich.dat <-
  x.total |>
  left_join(x.work.total, 'dataset') |>
  left_join(x.work.trend, c('dataset', 'work')) |>
  left_join(x.trend, c('dataset', 'nice'))
################################################################################
# Determine min number cutoff

p.minref <-
  enrich.dat |>
  ggplot(aes(work.trend.references, color = dataset)) +
  stat_ecdf(size = 1.2) +
  scale_y_continuous(breaks = seq(0, 1, .1)) +
  scale_x_log10() +
  scale_color_manual(values = cbPalette[2:3]) +
  geom_hline(yintercept = .9, color = 'red') +
  geom_vline(xintercept = 10, color = 'red') +
  xlab('No. patent references per scholalry work in IPC') +
  ylab('Empirical cumulative density') +
  theme_pubr(18)

################################################################################
# Fisher test of citation enrichment
enrich.dat |>
  filter(work.trend.references >= 10) |>
  group_by_all() |>
  do(
    pval = matrix(c(.$work.trend.references,                .$trend.patents - .$work.trend.references,
                    .$work.total - .$work.trend.references, .$dataset.patents - .$work.total - .$trend.patents + .$work.trend.references),
                  byrow = TRUE, nrow = 2) %>%
      # test for greater:
      # odds for being significant (first column) higher for MC selected (first row)
      fisher.test(alternative = 'greater') %>%
      `[[`('p.value')
  ) %>%
  ungroup %>%
  unnest(pval) |>
  group_by(dataset) |>
  mutate(
    padj = p.adjust(pval, method = 'fdr'),
    expect = work.total * trend.patents / dataset.patents,
    enrich = work.trend.references / expect
  ) |>
  ungroup() -> work.enriched



################################################################################


p.scatter <-
  work.enriched |>
  ggplot(aes(enrich, - log10(padj), color = dataset, group = dataset)) +
  geom_point(alpha = .7) +
  scale_color_manual(values = cbPalette[2:3]) +
  scale_x_log10() +
  annotation_logticks(sides = 'b') +
  xlab('Enrichment of citations in patents with IPC of interest') +
  ylab('- log10(FDR)') +
  geom_hline(yintercept = - log10(1e-3), color = 'red') +
  annotate('text', .5, 30, color = 'red', label = '0.001', size = 7) +
  # variable cut-off
  geom_vline(xintercept = 10, color = 'red') +
  theme_pubr(18)

p.scatter <- ggExtra::ggMarginal(p.scatter, type = 'boxplot', groupColour = TRUE)


################################################################################
# Potentially interesting papers

my.sel <-
  work.enriched |>
  filter(padj <= 1e-3, enrich >= 10) |>
  left_join(works, c('dataset', 'work' = 'lens.id'))

p.sel <-
  my.sel |>
  count(dataset, nice) |>
  arrange(desc(n)) |>
  mutate_at('nice', fct_inorder) |>
  ggplot(aes(nice, n, fill = dataset)) +
  scale_fill_manual(values = cbPalette[2:3]) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab('No. signigicantly enriched scholarly works') +
  coord_flip() +
  theme_pubr(18)


################################################################################

highlight <-
  my.sel |>
  count(dataset, nice, name = 'row') |>
  left_join(my.sel, c('dataset', 'nice')) |>
  # keep most sig per IPC
  # count(dataset, nice)
  group_by(dataset, nice) |>
  # multiple slices to untie
  slice_min(padj) |>
  slice_max(enrich) |>
  slice_max(year) |>
  ungroup() |>
  mutate_at('journal', str_remove, ' \\(New York.*$') |>
  mutate(journal = case_when(
    journal == 'Proceedings of the National Academy of Sciences of the United States of America' ~
      'Proceedings of the National Academy of Sciences',
    journal == 'TAG. Theoretical and applied genetics. Theoretische und angewandte Genetik' ~
      'Theoretical and Applied Genetics',
    journal == 'Neurogastroenterology and motility : the official journal of the European Gastrointestinal Motility Society' ~
      'Neurogastroenterology and motility',
    TRUE ~ journal
  )) |>
  mutate_at(c('citations', 'patents'), prettyNum, big.mark = ',') |>
  mutate_at(c('volume', 'issue'), as.character) |>
  mutate_at(c('volume', 'issue'), replace_na, '') |>
  mutate_at('authors', str_replace, ';.*$', ', et al.') |>
  mutate_at('title', str_replace, '(?<=.{70}).*', '...') |>
  mutate_at('enrich', round, 1) |>
  mutate_at('padj', sprintf, fmt = '%.2e') |>
  arrange(row) |>
  select(- row) |>
  select(
    IPC = nice,
    Title = title,
    Authors = authors,
    'Journal/Publisher' = journal,
    Year = year,
    DOI = doi,
    'References\nIPC' = work.trend.references,
    'References\ndataset' = work.total,
    'Patents\nin IPC' = trend.patents,
    Enrichment = enrich,
    FDR = padj
  ) |>
  unique()

p.table <-
  highlight |>
  mutate_at('IPC', str_remove, '^.* \\(') |>
  mutate_at('IPC', str_remove, '\\)$') |>
  select(- c(Authors, `Journal/Publisher`, Year)) |>
  gridExtra::tableGrob(rows = NULL, 
                       theme = gridExtra::ttheme_default(14)) |>
  plot_grid() +
  theme_pubr(18) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank())

################################################################################
# single figure


((((p.minref / p.scatter) | p.sel) & theme_pubr(18))
 / p.table) +
  plot_layout(heights = c(1.5, 1)) +
  plot_annotation(tag_levels = 'A')

ggsave('analysis/trend-enrich.png',
       width = 19, height = 12, dpi = 400)

write_tsv(highlight, 'analysis/trend-enrich.tsv')
