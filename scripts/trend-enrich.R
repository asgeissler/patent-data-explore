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
  filter(change > 0) |>
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
      ipc == 'G16B50/10' ~ 'Annotations (bioinformatics)',
      ipc == 'A23L1/00' ~ 'Foods or foodstuffs',
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
  filename = 'analysis/trend-heatmap.png',
  width = 12,
  height = 8
)
dev.off()


################################################################################
# Enrichment test for papers predominately present in trend

# Counts for part of fisher test
x.total <-
  patents |>
  count(dataset, name = 'dataset.patents')

x.trend <-
  mc.interest |>
  select(dataset, nice, ipc) |>
  left_join(ipc, c('dataset', 'ipc')) |>
  count(dataset, nice, name = 'trend.patents')

x.work.trend <-
  mc.interest |>
  select(dataset, nice, ipc) |>
  left_join(ipc, c('dataset', 'ipc')) |>
  inner_join(links, c('dataset', 'lens.id' = 'patent'),
             relationship = 'many-to-many') |>
  count(dataset, work, nice, name = 'work.trend.references')

x.work.total <-
  x.work.trend |>
  select(work, dataset) |>
  unique() |>
  left_join(links, c('dataset', 'work')) |>
  count(dataset, work, name = 'work.total')

# Combine and run fisher test
enrich.dat <-
  x.total |>
  left_join(x.work.total, 'dataset') |>
  left_join(x.work.trend, c('dataset', 'work')) |>
  left_join(x.trend, c('dataset', 'nice'))
enrich.dat |>
  # Fisher test of citation enrichment
  filter(work.trend.references >= 5) |>
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

# cutoff enrichment by quantile of dataset
var.cut <-
  work.enriched |>
  group_by(dataset) |>
  summarize(m = quantile(enrich, .9)) |>
  with(set_names(m, dataset))

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
  geom_vline(xintercept = var.cut, color = cbPalette[2:3]) +
  theme_pubr(18)

p <- ggExtra::ggMarginal(p.scatter, type = 'boxplot', groupColour = TRUE)

ggsave('analysis/trend-enrich.png', p,
       width = 8, height = 6, dpi = 400)

################################################################################

work.enriched |>
  filter(padj <= 1e-3) |>
  mutate(var = var.cut[dataset]) |>
  filter(enrich >= var) |>
  select(dataset, nice, work, enrich) |>
  spread(work, enrich, fill = 0) -> foo

foo |>
  select_if(is.numeric) |>
  as.matrix() |>
  magrittr::set_rownames(foo$nice) -> bar

pheatmap(
  log10(bar + 1),
  # bar,
  show_colnames = FALSE,
  scale = 'none',
  annotation_row = with(
    foo,
    data.frame(dataset = dataset, row.names = nice)
  ),
  annotation_colors = colors,
  # color = rev(colorRampPalette(RColorBrewer::brewer.pal(11, 'GnBu'))(99)),
  color = colorRampPalette(RColorBrewer::brewer.pal(9, 'Reds'))(99),
  filename = 'analysis/trend-enrich-heatmap.png',
  width = 12,
  height = 5
)
dev.off()

################################################################################

work.enriched |>
  filter(padj <= 1e-3) |>
  mutate(var = var.cut[dataset]) |>
  filter(enrich >= var) |>
  select(work, dataset) |>
  unique() |>
  count(work) |> count(n)
  group_by(dataset, work) |>
  slice_max(enrich) |>
  arrange(dataset, desc(enrich), desc(work.total)) |>
  group_by(dataset) |>
  slice(1:5) |>
  ungroup() |>
  left_join(works, c('dataset', 'work' = 'lens.id')) |>
  mutate_at('journal', str_remove, ' \\(New York.*$') |>
  mutate(journal = case_when(
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
  # mutate_at('title', str_replace, '(?<=.{40}).*', '...') |>
  arrange(desc(work.total)) |>
  select(
    Dataset = dataset,
    # nice,
    'References\nglobally' = work.total,
    # 'References\nin trend' = work.trend.references,
    # Enrichment = enrich,
    # FDR = padj,
    Title = title,
    Authors = authors,
    'Journal/Publisher' = journal,
    Year = year,
    DOI = doi,
  ) |>
  unique() |>
  View()
  gridExtra::tableGrob(rows = NULL, 
                       theme = gridExtra::ttheme_default(14)) |>
  plot_grid() +
  theme_pubr(18) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank())
