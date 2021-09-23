#####
# packages #
#####

require(MASS)
require(readr)
require(readxl)
require(tidyverse)
require(tibble)
require(data.table)
require(mgcv)

#####
# functions #
#####

# calc hit direction given x and y coords, returns a numeric #
hit_dir <- function(hc_x, hc_y) {
  x0 = 126
  y0 = 204
  x = x0 - hc_x
  y = y0 - hc_y
  atan2(x0 - hc_x, y0 - hc_y)*57.2958
}

# calculates the hit prob and returns as numeric given the data and the models already generated # 
get_hit_prob <- function(data, mod, gbmod) {
  
  # bring in the data and create a data frame for it #
  la = as.numeric(data['launch_angle'])
  ls = as.numeric(data['launch_speed'])
  hd = as.numeric(data['hit_dir'])
  newdata = data.frame(launch_speed = ls, 
                       launch_angle = la,
                       hit_dir = hd)
  
  # calc prob of hit #
  if (data['events'] == 'strikeout') z = 0
  else if (data['bb_type'] == 'popup') z = 0
  else if (data['bb_type'] == 'ground_ball') z = logit2prob(predict(gbmod, newdata = newdata))
  else z = logit2prob(predict(mod, newdata = newdata))
  rm(newdata)
  return(z)
}

# 
xba <- function(data) {
  
  # generates general model #
  mod <- gam(hit ~ s(hit_dir) + s(launch_angle) + s(launch_speed), family = 'binomial', data = data)
  
  # generates the gb model #
  gbdata  <- data[bb_type == 'ground_ball']
  gbmod <- gam(hit ~ s(hit_dir) + s(launch_speed), family = 'binomial', data = data)
  
  # call gget_hit_prob and creates a column with the value #
  data$hit_prob <- apply(data, 1, get_hit_prob, mod, gbmod)
  
  # if the hit prob is over 50%, its a hit #
  data$xhit <- ifelse(data$hit_prob >= .5, 1, 0)
  
  # calc xba # 
  num <- sum(data$xhit, na.rm = T)
  denom <- nrow(data[!is.na(data$xhit)])
  xba <- as.numeric(num/denom)
  
  return(xba)
  
}

# generates a plot of ba vs xba for a player for all years with data after the data has been melted #
plot.xba.ba <- function(data, player_name) {
  plot <- ggplot(ba.melt[ba.melt$player == player_name,], aes(x = game_year, y = value, col = variable)) +
    geom_line(size = 2) +
    ylim(c(0,.5)) +
    theme(legend.title = element_blank(),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size = 22),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.key.size = unit(2, 'cm'),
          title = element_text(size = 22)) +
    ggtitle(paste(player_name,'xba and ba vs time'))
  return(plot)
}

#####
# setup data # 
#####

ab_dic <- c('single','double','triple','home_run','double_play','fielders_choice_out','fielders_choice','strikeout',
            'field_out','force_out','grouinded_into_double_play','field_error','triple_play')

for_ab_dic <- c('single','double','triple','home_run')


# statcast_pbp.csv is the data file from a statcast query, we read in and convert to a data table for speed #
pbp <- read_csv('statcast_pbp.csv')
pbp <- as.data.table(pbp)

# select only the columns pertinent to us, and exclude 13 rows that misread #
prune_pbp <- dplyr::select(pbp, 1:10, 15, 16, 18, 19, 24:31, 38, 39, 45:58, 69:76, 88, 89) 
prune_pbp <- prune_pbp[(prune_pbp$game_year > 2000 & prune_pbp$game_year < 2021),]

# calc hit direction based on coordinates of hit and create a column #
prune_pbp$hit_dir <- mapply(hit_dir, prune_pbp$hc_x, prune_pbp$hc_y)

# create a column for if the event was a hit or not #
for (i in 1:nrow(prune_pbp)) ifelse(prune_pbp[i,'events'] %in% ab_dic, 
                                    ifelse(prune_pbp[i,'events'] %in% for_ab_dic, 
                                           prune_pbp[i, 'hit' := 1], 
                                           prune_pbp[i, 'hit' := 0]),
                                    prune_pbp[i, 'hit' := NA])

# sorting out just the ends of abs and dividing into years # 
ab_ends_pbp <- prune_pbp[prune_pbp$events %in% ab_dic,]

ends_2015 <- ab_ends_pbp[ab_ends_pbp$game_year == 2015,]
ends_2016 <- ab_ends_pbp[ab_ends_pbp$game_year == 2016,]
ends_2017 <- ab_ends_pbp[ab_ends_pbp$game_year == 2017,]
ends_2018 <- ab_ends_pbp[ab_ends_pbp$game_year == 2018,]
ends_2019 <- ab_ends_pbp[ab_ends_pbp$game_year == 2019,]
ends_2020 <- ab_ends_pbp[ab_ends_pbp$game_year == 2020,]

# list of unique batter ids # 
batIds <- unique(ab_ends_pbp$batter)

## example of calculating all batting averages for qualified players for one year (2015), to generate for another
## year just change ends_XXXX to the corresponding year 

# sorts each batter into own list #
rm(byBat15)
byBat15 <- list()
for (i in 1:length(batIds)) {
  byBat15[[i]] <- ends_2015[ends_2015$batter == batIds[i],]
}

# elinimates batters with less than 200 ABs #
rm(enoughAbs15)
enoughAbs15 <- c()
for (i in 1:length(byBat15)) {
  if (nrow(byBat15[[i]]) > 100) { 
    enoughAbs15[length(enoughAbs15) + 1] <- i
  }
}
enough15 <- byBat15[enoughAbs15]

# creates a new data frame with player id, name, and batting average #
rm(ba15)
ba15 <- data.frame(player = numeric(),
                   id = numeric(),
                   ba = numeric())
for (x in 1:length(enough15)) {
  all_ab <- enough15[[x]][!is.na('bb_type')]
  forab <- all_ab[all_ab$events %in% for_ab_dic,]
  ba15[x,1] <- enough15[[x]][1,'player_name']
  ba15[x,2] <- enough15[[x]][1,'batter']
  ba15[x,3] <- as.numeric(nrow(forab)/nrow(all_ab))
  rm(all_ab,forab)
}

# creates new column with xba #
ba15$xba <- lapply(enough15, xba)
ba15$xba <- as.numeric(ba15$xba)

# calculates a difference value for analysis # 
ba15$diff <- ba15$ba - ba15$xba

# adds game year for the melt data frame later #
ba15$game_year <- 2015
view(ba15)

# combind all years and melt so we can more easily graphs #
ba <- bind_rows(ba15,ba16,ba17,ba18,ba19,ba20)
ba.melt <- melt(ba, c('player','id','game_year'), c('ba','xba'))

# use the plotting function to make a graph # 

jgp <- plot.xba.ba(ba.melt, 'Joey Gallo')
jgp
