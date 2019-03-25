# Set working directory
setwd('./Documents/IPP2018')

# Load libraries, register cores
library(memisc)
library(data.table)
library(tidyverse)
library(ggsci)
library(doMC)
registerDoMC(8)

# Import data
df <- readRDS('./Data/merged_1.rds') %>%
  as.data.table(.)
votes <- fread('./Data/Vote_share.csv')

# Restrict focus to big parties 
parties <- c('Con', 'Lab', 'LD')
df <- df %>% 
  filter(partyId %in% c(1, 2, 3)) %>%
  select(-likeSNP, -likePC, -likeGreen, -likeUKIP) %>%
  as.data.table(.)
df[partyId == 1, pId := 'Con'
  ][partyId == 2, pId := 'Lab'
  ][partyId == 3, pId := 'LD']

# Tidy up
votes <- votes %>%
  gather(key = party, value = true_prop, -year) %>%
  mutate(true_prop = as.numeric(gsub('%', '', true_prop)) / 100) %>%
  group_by(year) %>%
  mutate(fctr = 1 / sum(true_prop),
         prop = true_prop * fctr) %>%
  as.data.table(.)
likes <- df %>%
  rename(from = pId) %>%
  gather(key = party, value = like, likeLab, likeLD, likeCon) %>%
  mutate(party = gsub('like', '', party),
         year = as.integer(year)) %>%
  inner_join(votes[, .(party, year, prop)], by = c('party', 'year')) %>%
  rename(to = party) 
fwrite(likes, './Data/likes.csv')
likes <- fread('./Data/likes.csv')

# Create personal hostility index (this actually takes a while)
fn <- function(idx) {
  tmp <- likes[unique_id == idx]
  if (tmp[, any(is.na(like))]) {
    phi <- NA_real_
  } else {
    a <- tmp[from == to, like]
    x <- 1 - tmp[from == to, prop]
    b <- tmp[from != to, like]
    y <- tmp[from != to, prop]
    summand_1 <- (a - b[1]) * (y[1] / (x))
    summand_2 <- (a - b[2]) * (y[2] / (x))
    phi <- sum(summand_1, summand_2)
  }
  tmp$phi <- phi
  return(tmp)
}
likes <- foreach(idx = likes[, unique(unique_id)], .combine = rbind) %dopar% 
  fn(idx)



# Affective polarisation index (Reiljan, 2016)
ap_idx <- function(yr, unit = 'all', strong_partisans = FALSE) {
  
  # Must be a year in likes
  if (!yr %in% likes[, unique(year)]) {
    stop('No survey data for ', yr)
  }
  # Must be a known party or all of them
  if (!unit %in% c('all', parties)) {
    stop('Unknown unit')
  }
  # Just strong partisans?
  if (strong_partisans) {
    likes <- likes[partyIdStrength == 1]
  }
  
  # Use vote shares from last election
  votes <- votes %>% 
    filter(year == yr) %>%
    as.data.table(.)
  
  # Create affective distance matrix
  dm <- matrix(nrow = 3, ncol = 3, dimnames = list(parties, parties))
  for (i in parties) {
    for (j in parties) {
      tmp <- likes %>% filter(year == yr, from == i, to == j)
      dm[i, j] <- mean(tmp$like, na.rm = TRUE)
    }
  }
  
  # Calculate AP for each party
  ap_parties <- sapply(parties, function(i) {
    others <- parties[parties != i]
    summands <- sapply(others, function(j) {
      (dm[i, i] - dm[i, j]) * 
        (votes[party == j, prop] / (1 - votes[party == i, prop]))
    })
    ap_i <- sum(summands)
    return(ap_i)
  })
  
  # By party or in aggregate?
  if (unit == 'all') {
    # Weighted sum
    out <- sum(ap_parties * votes$prop)
  } else {
    # Just our party of interest
    out <- ap_parties[which(parties == unit)]
  }
  
  # Export
  return(out)
  
}


# Mass affective polarization index (Lauka et al., 2018)
map_idx <- function(yr) {
  
  # Must be a year in likes
  if (!yr %in% likes[, unique(year)]) {
    stop('No survey data for ', yr)
  }
  
  # Calculate like/dislike proportions
  like_prop <- sapply(parties, function(party) {
    likes[year == yr & to == party & !is.na(like), 
          sum(like %in% 8:10) / .N]
  })
  dislike_prop <- sapply(parties, function(party) {
    likes[year == yr & to == party & !is.na(like), 
          sum(like %in% 0:2) / .N]
  })
  
  # Divide 
  numerator <- sum(like_prop * dislike_prop)
  denominator <- length(parties) * 0.25
  map_idx <- numerator / denominator
  return(map_idx)
  
}

# Calculate all index values
yrs <- likes[, unique(year)]
out <- data.frame(
  Year = yrs, 
    AP = sapply(yrs, function(y) ap_idx(y)), 
   MAP = sapply(yrs, function(y) map_idx(y))
)

# Spread back to df
df <- likes %>%
  select(-prop) %>%
  spread(key = to, value = like)

# Plot phi
phi_df <- df %>%
  filter(!is.na(phi), !is.na(weight)) %>%
  group_by(year) %>%
  summarise(PHI = weighted.mean(phi, weight),
             SE = sd(phi) / sqrt(length(phi))) %>%
  rename(Year = year)
out <- phi_df %>%
  inner_join(out, by = 'Year')


df %>%
  filter(!is.na(phi), !is.na(weight), year > 1992) %>%
  group_by(year, partyId) %>%
  summarise(PHI = weighted.mean(phi, weight),
            SE = sd(phi) / sqrt(length(phi))) %>%
  rename(Year = year) %>%
  mutate(partyId = as.factor(partyId)) %>%
  ggplot(aes(Year, PHI, group = partyId, color = partyId)) + 
  geom_point() + 
  geom_smooth(method = 'gam', se = FALSE, formula = y ~ s(x, k = 6)) + 
  geom_errorbar(aes(ymin = PHI - SE, ymax = PHI + SE), width = 0.25) +
  labs(title = 'PHI by Party Affiliation') + 
  scale_color_d3() + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

df <- df %>% 
  filter(!is.na(phi)) %>%
  mutate(neg_phi = if_else(phi < 0, TRUE, FALSE)) %>%
  group_by(year, partyId) %>%
  mutate(prop_nphi = sum(neg_phi) / length(neg_phi)) %>%
  select(year, partyId, prop_nphi) %>%
  spread(year, prop_nphi)

table(df$neg_phi, df$partyId, df$year)



# Plot change over time
ggplot(out, aes(Year, AP)) + 
  geom_point() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(y = 'Affective Polarisation', 
       title = 'AP Over Time') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(out, aes(Year, MAP)) + 
  geom_point() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(y = 'Mass Affective Polarisation',
       title = 'MAP Over Time') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(out, aes(Year, PHI)) + 
  geom_point() + 
  geom_smooth(method = 'loess', se = FALSE, span = 0.75) +
  geom_errorbar(aes(ymin = PHI - SE, ymax = PHI + SE), width = 0.25) + 
  labs(y = 'Personal Hostility Index', 
       title = 'PHI Over Time') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

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

# Correlation test?
with(out, cor.test(AP, MAP))

# Linear model coefficients?
summary(lm(AP ~ MAP, data = out))


# Affective distance matrix, tbl output
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
































# False polarization index (Dalton, 2008)
fp_idx <- function(yr) {
  
  # Must be a year in likes
  if (!yr %in% likes[, unique(Year)]) {
    stop('No survey data for ', yr)
  }
  
  # Use vote shares from last election
  if (yr %in% votes[, unique(Year)]) {
    vote_yr <- yr
  } else {
    gaps <- yr - votes[, unique(Year)]
    vote_yr <- yr - min(gaps[gaps > 0])
  }
  votes <- votes %>% filter(Year == vote_yr)
  
  # Normalize vote shares
  fctr <- 1 / sum(votes$prop)
  votes <- votes %>%
    mutate(prop = prop * fctr) %>%
    as.data.table(.)
  
  # Ideological placement
  lr_global <- df[year == yr, mean(leftRight, na.rm = TRUE)]
  lr_party <- sapply(parties, function(party) {
    lr_i <- df[year == yr & partyId == party, mean(leftRight, na.rm = TRUE)]
    lr_var <- ((lr_i - lr_global) / (length(parties) - 1))^2
    return(lr_var)
  })
  
  # Weight variance by vote share
  fp_idx <- sqrt(sum(lr_party * votes$prop))
  return(fp_idx)
  
}



