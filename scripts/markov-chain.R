#!/usr/bin/env Rscript
# Markov chain analysis of trend changes in IPCs

source('scripts/helper_load.R')

################################################################################
# Count per year etc

ipc.freqs <-
  ipc |>
  left_join(
    patents |>
      select(dataset, lens.id, priority.year),
    c('lens.id', 'dataset')
  ) |>
  count(dataset, ipc, priority.year, name = 'n.year.ipc') |>
  drop_na() |>
  complete(dataset, priority.year, ipc) |>
  mutate_at('n.year.ipc', replace_na, 0) |>
  left_join(
    patents |>
      count(dataset, priority.year, name = 'n.year'),
    c('priority.year', 'dataset'),
    relationship = 'many-to-one'
  ) |>
  left_join(
    ipc |>
      count(dataset, ipc, name = 'n.ipc'),
    c('ipc', 'dataset'),
    relationship = 'many-to-one'
  ) |>
  # exclude IPCs that do not appear in dataset at all
  drop_na(n.ipc) |>
  # exclude in the scheme outdated IPCs
  semi_join(ipc.titles, 'ipc') |>
  # exclude year without patents (eg CRISPR dataset is 'young')
  drop_na(n.year)

################################################################################
# Show correlations in total numbers

helper <- function() {
 ipc.freqs |>
    select(
      'per year and IPC' = n.year.ipc,
      'per year' = n.year,
      'per IPC' = n.ipc
    ) |>
    as.matrix() |>
    cor() |>
    corrplot(
      method = 'square',
      order = 'AOE',
      type = 'lower',
      diag = FALSE,
      insig='blank',
      addCoef.col ='black',
      number.cex = 0.8
    )
}

p.cor <-
  plot_grid(helper) +
  coord_cartesian() +
  ylim(c(0, .9)) +
  xlim(c(.2, .8)) +
  theme_pubr(18) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank()) +
  plot_annotation(title = 'Pearson correlations between no. patents...')
# p.cor

################################################################################
# Correlation to proceeding year

p.cor.year <-
  ipc.freqs |>
  select(dataset, priority.year, ipc, n.year.ipc) |>
  mutate(before = priority.year - 1) |>
  inner_join(ipc.freqs, c('ipc', 'before' = 'priority.year', 'dataset')) |>
  ggscatter(
    'n.year.ipc.x', 'n.year.ipc.y',
    color = 'dataset',
    alpha = .7,
    add = 'reg.line',
    add.params = list(color = 'blue'),
    cor.coef = TRUE,
    cor.coeff.args = list(color = 'red')
  ) +
  scale_color_manual(values = cbPalette[2:3]) +
  xlab('No. patents per year and IPC classification') +
  ylab('No. in preceding year') +
  theme_pubr(18)

################################################################################
# Estimate "negative binomial-ness"

ipc.freqs |>
  group_by(dataset, ipc) |>
  summarize(
    m = mean(n.year.ipc),
    v =  var(n.year.ipc)
  ) |>
  ungroup() -> yi.avg

# estimate dispersion
disp.model <- lm(v ~ offset(m) + I(m^2) - 1, data = yi.avg)
alpha <- disp.model$coefficients[['I(m^2)']]
# r2
r2 <- summary(disp.model)$r.squared
# regression for plot
reg <- tibble(
  m = seq(0, max(yi.avg$m)),
) %>%
  mutate(v = predict(disp.model, .))

p.nb <-
  yi.avg |>
  ggscatter(
    'm', 'v',
    color = 'dataset'
  ) +
  scale_color_manual(values = cbPalette[2:3]) +
  xlab('Per IPC classification averaged no. patents') +
  ylab('Variance') +
  geom_line(
    data = reg,
    color = 'blue'
  ) +
  annotate(
    'text',
    label = sprintf(
      ' y == x + %.2f * x ^ 2  ~~ ~~ R ^ 2 == %.2f',
      alpha, r2
    ),
    color = 'red',
    size = 5,
    parse = TRUE,
    x = 300,
    y = 3e4
  ) +
  theme_pubr(18)


################################################################################
# Time-series approach with markov chain


# Assuming NP -> Conditional is Poisson

# Exclyde per IPC the first year with a patent (avoid "flank" calling)
ipc.freqs |>
  filter(n.year.ipc > 0) |>
  group_by(dataset, ipc) |>
  summarize(first.year = min(priority.year)) |>
  ungroup() -> ipc.first

mc.chain <-
  ipc.freqs |>
  select(dataset, priority.year, ipc, n.year.ipc) |>
  left_join(ipc.first, c('dataset', 'ipc')) |>
  filter(priority.year > first.year) |>
  mutate(before = priority.year - 1) |>
  inner_join(ipc.freqs, c('ipc', 'before' = 'priority.year', 'dataset')) |>
  transmute(
    dataset, priority.year, ipc,
    xiplus = n.year.ipc.x + 1,
    xi = n.year.ipc.y + 1,
    change = xiplus - xi
  ) |>
  group_by(dataset, ipc) |>
  mutate(p.step = dpois(xiplus, xi))  |>
  ungroup()

mc.chain |>
  write_tsv('analysis/markov-chain.tsv.gz')

################################################################################

# A Volcano plot inspired visualization
p.volcano <-
  mc.chain |>
  ggplot(aes(change, - log10(p.step), color = dataset)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = cbPalette[2:3]) +
  geom_hline(yintercept = 10, color = 'red') +
  geom_vline(xintercept = c(-100, 100), color = 'red') +
  annotate('text', x = -200, y = 200, label = '± 100',
           color = 'red', size = 5) +
  annotate('text', x = -500, y = 0, label = 'prob. 1e-10',
           color = 'red', size = 5) +
  xlab('Absolute change in no. patents\nfrom preceeding year') +
  ylab('- log10(Poisson probability)') +
  theme_pubr(18)


################################################################################
# Save combined figures

p.cor
ggsave(
  'analysis/markov-chain-correlations.png',
  width = 6, height = 6,
  dpi = 400
)

(p.cor.year + p.nb + p.volcano ) +
  plot_annotation(tag_levels = 'A')

ggsave(
  'analysis/markov-chain-scatter.png',
  width = 18, height = 7,
  dpi = 400
)

################################################################################
