# Set working directory
setwd('./Documents/Affective_Polarisation')

# Load libraries, register cores
library(memisc)
library(data.table)
library(ggsci)
library(tidyverse)
library(doMC)
registerDoMC(4)

# Import data
df <- readRDS('./Data/likes_melt.rds')
df[partyId == 1, pId := 'Conservative'
  ][partyId == 2, pId := 'Labour'
  ][partyId == 3, pId := 'Lib Dem']

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

# Distance matrices
dm <- function(yr) {
  
  # Must be a year in likes
  if (!yr %in% likes[, unique(year)]) {
    stop('No survey data for ', yr)
  }
  # Create affective distance matrix
  mat <- matrix(nrow = 3, ncol = 3, dimnames = list(parties, parties))
  for (i in parties) {
    for (j in parties) {
      tmp <- likes %>% filter(year == yr, from == i, to == j)
      mat[i, j] <- 10 - mean(tmp$like, na.rm = TRUE)
    }
  }
  # Melt
  mat %>%
    as_tibble(.) %>%
    gather(x, Distance) %>%
    mutate(y = rep(parties, 3)) %>%
    mutate(y = rep(parties, 3)) %>%
    mutate(x = factor(x, levels = unique(parties)),
           y = factor(y, levels = rev(unique(parties))),
           year = yr) %>%
    return(.)
}

# Build big data frame
dm_df <- foreach(yr = df[, unique(year)], .combine = rbind) %dopar% dm(yr) %>%
  arrange(year)

# Plot
ggplot(dm_df, aes(x, y, fill = Distance)) + 
  geom_tile() + 
  scale_fill_gradientn(colors = brewer.pal(10L, 'RdBu')) +
  coord_equal() + 
  labs(title = 'Affective Distance Over Time',
       x = 'Evaluated',
       y = 'Evaluator') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ year)








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
