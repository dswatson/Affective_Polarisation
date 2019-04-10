# Set working directory
setwd('./Documents/Affective_Polarisation')

# Load libraries, register cores
library(memisc)
library(data.table)
library(ggsci)
library(RColorBrewer)
library(tidyverse)
library(doMC)
registerDoMC(4)

# Import data
df <- readRDS('./Data/likes_melt.rds')
df[partyId == 1, pId := 'Conservative'
  ][partyId == 2, pId := 'Labour'
  ][partyId == 3, pId := 'Lib Dem']

# PHI over time
df %>%
  filter(!is.na(phi), !is.na(weight), year > 1992) %>%
  group_by(year) %>%
  summarise(PHI = weighted.mean(phi, weight),
            SE = sd(phi) / sqrt(length(phi))) %>%
  rename(Year = year) %>%
  select(Year, PHI, SE) %>%
  ggplot(aes(Year, PHI)) + 
  geom_point() + 
  geom_smooth(method = 'gam', se = FALSE, formula = y ~ s(x, k = 6)) + 
  geom_errorbar(aes(ymin = PHI - SE, ymax = PHI + SE), width = 0.25) +
  labs(title = 'PHI Over Time',
       y = 'Personal Hostility Index') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./Figures/phi_over_time.pdf')

# PHI by party over time
df %>%
  filter(!is.na(phi), !is.na(weight), year > 1992) %>%
  group_by(year, pId) %>%
  summarise(PHI = weighted.mean(phi, weight),
            SE = sd(phi) / sqrt(length(phi))) %>%
  rename(Year = year) %>%
  select(Year, PHI, SE, pId) %>%
  ggplot(aes(Year, PHI, group = pId, color = pId)) + 
  geom_point() + 
  geom_smooth(method = 'gam', se = FALSE, formula = y ~ s(x, k = 6)) + 
  geom_errorbar(aes(ymin = PHI - SE, ymax = PHI + SE), width = 0.25) +
  labs(title = 'PHI by Party Affiliation',
       y = 'Personal Hostility Index') + 
  scale_color_manual(name = 'Party Affiliation',
                     labels = c('Conservative', 'Labour', 'Lib Dem'),
                     values = pal_d3()(4)[c(1, 4, 2)]) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./Figures/phi_by_party_dynamic.pdf')

# PHI by party, all at once
df %>%
  filter(!is.na(phi), !is.na(weight), year > 1992) %>%
  ggplot(aes(pId, phi, fill = pId)) + 
  geom_boxplot() + 
  labs(title = 'PHI Distribution by Party', 
       y = 'Personal Hostility Index') +
  scale_fill_manual(name = 'Party Affiliation',
                    labels = c('Conservative', 'Labour', 'Lib Dem'),
                    values = pal_d3()(4)[c(1, 4, 2)]) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45L, hjust = 1))
ggsave('./Figures/phi_by_party_static.pdf')

# Trajectories by party
likes <- fread('./Data/likes.csv')[year > 1992]
parties <- c('Con', 'Lab', 'LD')
dm_df <- crossing(Year = likes[, unique(year)],
                  Evaluator = parties,
                  Evaluated = parties)
mu_fn <- function(yr, x, y) {
  likes[year == yr & from == y & to == x, 10 - mean(like, na.rm = TRUE)]
}
se_fn <- function(yr, x, y) {
  likes[year == yr & from == y & to == x, sd(like, na.rm = TRUE)]
}
dm_df <- dm_df %>%
  rowwise(.) %>%
  mutate(Distance = mu_fn(Year, Evaluated, Evaluator),
         SE = se_fn(Year, Evaluated, Evaluator)) %>%
  as.data.table(.)
dm_df[Evaluator == 'Con', Evaluator := 'Conservative'
  ][Evaluator == 'Lab', Evaluator := 'Labour'
  ][Evaluator == 'LD', Evaluator := 'Lib Dem'
  ][Evaluated == 'Con', Evaluated := 'Conservative'
  ][Evaluated == 'Lab', Evaluated := 'Labour'
  ][Evaluated == 'LD', Evaluated := 'Lib Dem'
  ][, Evaluator := factor(Evaluator, 
                          levels = c('Conservative', 'Labour', 'Lib Dem'))
  ][, Evaluated := factor(Evaluated, 
                          levels = c('Conservative', 'Labour', 'Lib Dem'))]
ggplot(dm_df, aes(Year, Distance, group = Evaluated, color = Evaluated)) + 
  geom_point() + 
  geom_smooth(method = 'gam', se = FALSE, formula = y ~ s(x, k = 6),
              size = 0.75) +
  geom_errorbar(aes(ymin = Distance - SE, ymax = Distance + SE), width = 0.25) + 
  scale_color_manual(name = 'Evaluating',
                     labels = c('Conservative', 'Labour', 'Lib Dem'),
                     values = pal_d3()(4)[c(1, 4, 2)]) +
  labs(x = 'Year', y = 'Affective Distance') + 
  theme_bw() + 
  facet_wrap(~ Evaluator, nrow = 3)





# Plot per party
plot_fn <- function(party) {
  if (party == 'Conservatives') {
    pId <- 1
  } else if (party == 'Labour') {
    pId <- 2
  } else if (party == 'Lib Dems') {
    pId <- 3
  }
  df %>%
    filter(partyId == pId, !is.na(phi), !is.na(weight)) %>%
    group_by(year) %>%
    summarise(PHI = weighted.mean(phi, weight),
              SE = sd(phi) / sqrt(length(phi))) %>%
    rename(Year = year) %>%
    ggplot(aes(Year, PHI)) + 
    geom_point() + 
    geom_smooth(method = 'loess', se = FALSE, span = 0.75) +
    geom_errorbar(aes(ymin = PHI - SE, ymax = PHI + SE), width = 0.25) + 
    labs(y = 'Personal Hostility Index', 
         title = paste('PHI Over Time:', party)) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
}


# For linear associations:
ggplot(out, aes(AP, MAP, label = Year)) + 
  geom_text() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  labs(x = 'Affective Polarisation', 
       y = 'Mass Affective Polarisation',
       title = 'AP vs. MAP') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
