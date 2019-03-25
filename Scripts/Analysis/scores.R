# Set working directory
setwd('./Documents/Affective_Polarisation')

# Load libraries, register cores
library(memisc)
library(data.table)
library(tidyverse)
library(ggsci)
library(doMC)
registerDoMC(8)

# Import data
df <- readRDS('./Data/merged.rds') %>%
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

### COMPUTE INDICES ###

# Personal hostility index (this actually takes a while)
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
saveRDS(likes, './Data/likes_with_phi.rds')

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
saveRDS(df, './Data/likes_melt.rds')

# Plot phi
phi_df <- df %>%
  filter(!is.na(phi), !is.na(weight)) %>%
  group_by(year) %>%
  summarise(PHI = weighted.mean(phi, weight),
             SE = sd(phi) / sqrt(length(phi))) %>%
  rename(Year = year)
out <- phi_df %>%
  inner_join(out, by = 'Year')
saveRDS(out, './Data/indices_per_yr.rds')




# Correlation test?
with(out, cor.test(AP, MAP))

# Linear model coefficients?
summary(lm(AP ~ MAP, data = out))



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



