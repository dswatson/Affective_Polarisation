# Set working directory
setwd('./Documents/Affective_Polarisation')

# Load libraries
library(memisc)
library(data.table)
library(ggsci)
library(tidyverse)

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


