#####
# setup #
#####
require(MASS)
require(readr)
require(readxl)
require(tidyverse)
require(tibble)
require(data.table)
require(mgcv)

#####
#Climate Change
#####
#We want to create a model to predict the temperature given different environmental factors.

#Read in our data
ClimateData = read.csv("climate_change.csv")

str(ClimateData)

#Build the training set and testing set for the model
TrainingSet = subset(ClimateData, Year <=2006)
TestingSet = subset(ClimateData, Year >2006)

#Build the model.  This is a linear model (lm), where the dependent variable is Temp and the independent variables are all the others
Model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = TrainingSet)
summary(Model1)
cor(TrainingSet)

#build model with only MEI, TSI, Aerosols and N2O as independent variables.
Model2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=TrainingSet)
summary(Model2)

#The step() function will automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R^2.
StepModel = step(Model1)
summary(StepModel)
#predict using step function model
TempPredict = predict(StepModel, newdata=TestingSet)
SSE = sum((TempPredict - TestingSet$Temp)^2)
SST = sum((mean(TrainingSet$Temp) - TestingSet$Temp)^2)
R2=1-SSE/SST
R2

#####
# setup #
#####

#functions
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fac_to_num <- function(x) {
  x <- as.numeric(as.character(x))
}

hit_dir <- function(hc_x, hc_y) {
  x0 = 126
  y0 = 204
  x = x0 - hc_x
  y = y0 - hc_y
  atan2(x0 - hc_x, y0 - hc_y)*57.2958
}

get_hit_prob <- function(data, mod, gbmod) {
  
  la = as.numeric(data['launch_angle'])
  ls = as.numeric(data['launch_speed'])
  hd = as.numeric(data['hit_dir'])
  newdata = data.frame(launch_speed = ls, 
                       launch_angle = la,
                       hit_dir = hd)
  
  if (data['events'] == 'strikeout') z = 0
  else if (data['bb_type'] == 'popup') z = 0
  else if (data['bb_type'] == 'ground_ball') z = logit2prob(predict(gbmod, newdata = newdata))
  else z = logit2prob(predict(mod, newdata = newdata))
  rm(newdata)
  return(z)
}

xba <- function(data) {
  
  mod <- gam(hit ~ s(hit_dir) + s(launch_angle) + s(launch_speed), family = 'binomial', data = data)
  
  gbdata  <- data[bb_type == 'ground_ball']
  gbmod <- gam(hit ~ s(hit_dir) + s(launch_speed), family = 'binomial', data = data)
  
  data$hit_prob <- apply(data, 1, get_hit_prob, mod, gbmod)
  
  data$xhit <- ifelse(data$hit_prob >= .5, 1, 0)
  
  num <- sum(data$xhit, na.rm = T)
  denom <- nrow(data[!is.na(data$xhit)])
  xba <- as.numeric(num/denom)
  
  return(xba)
  
}

#prepare verlander data
dat <- read.csv('savant_data.csv') 
dat$launch_speed <- as.numeric(as.character(dat$launch_speed)) 
dat$release_speed <- as.numeric(as.character(dat$release_speed)) 
dat$plate_x <- as.numeric(as.character(dat$plate_x)) 
dat$plate_z <- as.numeric(as.character(dat$plate_z))
dat$release_spin_rate <- as.numeric(as.character(dat$release_spin_rate))

trainer <- subset(dat, game_year <= 2018 & launch_speed != 80.0) %>% 
  dplyr::select(.,game_year, launch_speed, release_speed, release_spin_rate, plate_z, plate_x)
trainer <- trainer[complete.cases(trainer),]
tester <- subset(dat, game_year == 2019 & launch_speed != 80.0) %>% 
  dplyr::select(.,game_year, launch_speed, release_speed, release_spin_rate, plate_z, plate_x)
tester <- tester[complete.cases(tester),]

cordat <- dplyr::select(bind_rows(tester,trainer), launch_speed, release_speed, plate_z, plate_x, release_spin_rate)

#####
#prepare all ptichers data
#####

all_pitchers <- read_csv('all_pitchers_bbe.csv')

all_pitchers$launch_speed <- as.numeric(as.character(all_pitchers$launch_speed)) 
all_pitchers$release_speed <- as.numeric(as.character(all_pitchers$release_speed)) 
all_pitchers$plate_x <- as.numeric(as.character(all_pitchers$plate_x)) 
all_pitchers$plate_z <- as.numeric(as.character(all_pitchers$plate_z))
all_pitchers$release_spin_rate <- as.numeric(as.character(all_pitchers$release_spin_rate))
all_pitchers$launch_angle <- as.numeric(as.character(all_pitchers$launch_angle)) 
all_pitchers$woba_value <- as.numeric(as.character(all_pitchers$woba_value)) 
all_pitchers$estimated_woba_using_speedangle <- as.numeric(as.character(all_pitchers$estimated_woba_using_speedangle))
all_pitchers$iso_value <- as.numeric(as.character(all_pitchers$iso_value))


dt <- sort(sample(nrow(all_pitchers), nrow(all_pitchers)*.7))

trainall <- all_pitchers[dt,]
testall  <- all_pitchers[-dt,]


trainall <- subset(trainall, launch_speed != 80.0 & launch_angle != -21) %>% 
  dplyr::select(.,game_year, launch_speed, release_speed, release_spin_rate, plate_z, plate_x, launch_angle, woba_value,
         estimated_woba_using_speedangle)
trainall <- trainall[complete.cases(trainall),]
testall <- subset(testall, launch_speed != 80.0 & launch_angle != -21) %>% 
  dplyr::select(.,game_year, launch_speed, release_speed, release_spin_rate, plate_z, plate_x,launch_angle,woba_value,
         estimated_woba_using_speedangle)
testall <- testall[complete.cases(testall),]

cordatall <- dplyr::select(bind_rows(testall,trainall), launch_speed, release_speed, plate_z, plate_x, release_spin_rate,launch_angle)


#####
# verlander models  #
#####

m1 <- lm(launch_speed ~ release_speed + plate_x + plate_z + release_spin_rate, data = trainer)
summary(m1)
cor(cordatall)

stepmod <- step(m1)
summary(stepmod)

evpredict <- predict(stepmod, newdata = tester)
evpredict
sse <- sum((evpredict - tester$launch_speed)^2) 
sse
sst <- sum((mean(trainer$launch_speed) - tester$launch_speed)^2)
sst
r2 <- 1 - sse/sst
r2

p <- tibble(predict = evpredict,
            real = tester$launch_speed)

ggplot(p, aes(x = predict, y = real)) +
  geom_jitter()

ggplot(tester, aes(x = launch_speed)) +
  geom_density()

#####
# all pitchers models #
#####

cor(cordatall)
corrplot(cor(cordatall), method = 'color', type = 'upper', diag = F)

allm <- lm(launch_speed ~ release_speed + plate_x + plate_z + release_spin_rate + plate_z*release_spin_rate + plate_z*release_speed, 
           data = trainall)
allm

stepmodall <- step(allm)

evpredictall <- predict(stepmodall, newdata = testall)

sse_all <- sum((evpredictall - testall$launch_speed)^2) 
sse_all
sst_all <- sum((mean(trainall$launch_speed) - testall$launch_speed)^2)
sst_all
r2_all <- 1 - sse_all/sst_all
r2_all

lalm <- lm(launch_angle ~ release_speed + plate_x + plate_z + release_spin_rate, data = trainall)
lalm

stepmodla <- step(lalm)

evpredictla <- predict(stepmodla, newdata = testall)
ogpredeictla <- predict(lalm, newdata = testall)

sse_la <- sum((evpredictla - testall$launch_angle)^2) 
sse_la
sst_la <- sum((mean(trainall$launch_angle) - testall$launch_angle)^2)
sst_la
r2_la <- 1 - sse_la/sst_la
r2_la

p3 <- tibble(predict = evpredictla,
             real = testall$launch_angle,
             og = ogpredeictla)


uh <- c(evpredictla,testall$launch_angle)
yuh <- c(rep('predict', length(evpredictla)),rep('real', length(testall$launch_angle)))

p4 <- data.frame(value = uh,
                 type = yuh)


ggplot(p3, aes(x = predict, y = real)) +
  geom_jitter()

ggplot(p3, aes(x = real)) +
  geom_density(col = 'red') +
  geom_density(data = p3, aes(x = predict), col = 'blue', linetype = 'longdash') +
  geom_density(data = p3, aes(x = og), col = 'green', linetype = 'twodash')

ggplot(p3, aes(x = predict)) +
  geom_density()

ggplot(p3, aes(x = real)) +
  geom_density()

ggplot(p3) +
  geom_density(data = p3, aes(x = predict), col = 'blue', linetype = 'longdash') +
  geom_density(data = p3, aes(x = og), col = 'green', linetype = 'twodash')

ggplot(p4, aes(x = value)) +
  geom_density(aes(color = type))

vis_miss(trainall)

#####
# prune db #
#####

ggplot() + 
  geom_point(aes(x = bruh[1], y = bruh[2])) + 
  geom_point(aes(x = 126, y = 204)) +
  xlim(0,250) +
  ylim(0,250) +
  geom_abline(slope = -1, intercept = 330) +
  geom_abline(slope = 1, intercept = 78)

test <- all_pitchers
test$hc_x <- as.numeric(test$hc_x, digits = 5)
test$hc_y <- as.numeric(test$hc_y, digits = 5)

test$hit_dir <- mapply(hit_dir, test$hc_x,test$hc_y)

all_pitchers$hit_dir <- mapply(hit_dir, all_pitchers$hc_x, all_pitchers$hc_y)



prune <- dplyr::select(all_pitchers, 1:11, 15, 16, 18, 19, 24:31, 38, 39, 45:58, 69:76, 88, 89)
prune$hc_x <- as.numeric(prune$hc_x, digits = 5)
prune$hc_y <- as.numeric(prune$hc_y, digits = 5)
prune$hit_dir <- mapply(hit_dir, prune$hc_x, prune$hc_y)

ba_prune <- dplyr::select(prune, 7, 9, 35, 36, 50)

#####
# calc batting averages #
#####

ab_dic <- c('single','double','triple','home_run','double_play','fielders_choice_out','fielders_choice','strikeout',
            'field_out','force_out','grouinded_into_double_play','field_error','triple_play')

for_ab_dic <- c('single','double','triple','home_run')

against_ab_dic <- c('fielders_choice_out','fielders_choice','double_play','strikeout','field_out','force_out',
                'grounded_into_doubele_play','field_error','triple_play')


all_ab_ends <- prune[prune$events %in% ab_dic,]

uni_bat_ids <- unique(all_ab_ends$batter)

rm(abs_by_batter)
abs_by_batter <- list()
for (i in 1:length(uni_bat_ids)) {
  abs_by_batter[[i]] <- all_ab_ends[all_ab_ends$batter == uni_bat_ids[i],]
}

rm(uh)
uh <- c()
for (i in 1:length(abs_by_batter)) {
  if (nrow(abs_by_batter[[i]]) > 100) { 
    uh[[i]] <- i
  }
}
uh <- na.omit(uh)

enough <- abs_by_batter[uh]

rm(ish)
ish <- list()
for (x in 1:length(enough)) {
  all_ab <- enough[[x]]
  forab <- all_ab[all_ab$events %in% for_ab_dic,]
  ish[[x]] <- nrow(forab)/nrow(all_ab)
  rm(all_ab,forab)
}



#####
# woba models #
#####

wobamod <- glm(estimated_woba_using_speedangle ~ release_speed + release_spin_rate + plate_z + plate_x + launch_angle + launch_speed, 
              data = trainall)

stepwoba <- step(wobamod)  
wobapredict <- predict(stepwoba, newdata = testall)

sse_woba <- sum((wobapredict - testall$estimated_woba_using_speedangle)^2) 
sse_woba
sst_woba <- sum((mean(trainall$estimated_woba_using_speedangle) - testall$estimated_woba_using_speedangle)^2)
sst_woba
r2_woba <- 1 - sse_woba/sst_woba
r2_woba  

e <- c(wobapredict, testall$estimated_woba_using_speedangle)
k <- c(rep('predict', length(wobapredict)), rep('real',length(testall$estimated_woba_using_speedangle)))

p5 <- tibble(value = e,
             type = k)

ggplot(p5, aes(x = value)) +
  geom_density(aes(fill = type), alpha = .5) +
  ggtitle('Real vs Predicted woba values') +
  theme(axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 25),
        title = element_text(size = 25))

#####
# glm garbo # 
#####

allglm <- glm(estimated_woba_using_speedangle ~ plate_z + plate_x + launch_angle + launch_speed, 
              data = trainall, family = binomial)

#####
# ba method #
#####


#####
# pbp #
#####
pbp <- read_csv('statcast_pbp.csv')
pbp <- as.data.table(pbp)

#####
# prune pbp #
#####

prune_pbp <- dplyr::select(pbp, 1:10, 15, 16, 18, 19, 24:31, 38, 39, 45:58, 69:76, 88, 89) 
prune_pbp <- prune_pbp[(prune_pbp$game_year > 2000 & prune_pbp$game_year < 2021),]

prune_pbp$hit_dir <- mapply(hit_dir, prune_pbp$hc_x, prune_pbp$hc_y)

for (i in 1:nrow(prune_pbp)) ifelse(prune_pbp[i,'events'] %in% ab_dic, 
                              ifelse(prune_pbp[i,'events'] %in% for_ab_dic, 
                                     prune_pbp[i, 'hit' := 1], 
                                     prune_pbp[i, 'hit' := 0]),
                              prune_pbp[i, 'hit' := NA])

ab_ends_pbp <- prune_pbp[prune_pbp$events %in% ab_dic,]

ends_2015 <- ab_ends_pbp[ab_ends_pbp$game_year == 2015,]
ends_2016 <- ab_ends_pbp[ab_ends_pbp$game_year == 2016,]
ends_2017 <- ab_ends_pbp[ab_ends_pbp$game_year == 2017,]
ends_2018 <- ab_ends_pbp[ab_ends_pbp$game_year == 2018,]
ends_2019 <- ab_ends_pbp[ab_ends_pbp$game_year == 2019,]
ends_2020 <- ab_ends_pbp[ab_ends_pbp$game_year == 2020,]

batIds <- unique(ab_ends_pbp$batter)

#####
# 2015 #
#####

rm(byBat15)
byBat15 <- list()
for (i in 1:length(batIds)) {
  byBat15[[i]] <- ends_2015[ends_2015$batter == batIds[i],]
}

rm(enoughAbs15)
enoughAbs15 <- c()
for (i in 1:length(byBat15)) {
  if (nrow(byBat15[[i]]) > 100) { 
    enoughAbs15[length(enoughAbs15) + 1] <- i
  }
}
enough15 <- byBat15[enoughAbs15]

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

ba15$xba <- lapply(enough15, xba)
ba15$xba <- as.numeric(ba15$xba)
ba15$diff <- ba15$ba - ba15$xba
ba15$game_year <- 2015
view(ba15)

#####
# 2016 # 
#####

rm(byBat16)
byBat16 <- list()
for (i in 1:length(batIds)) {
  byBat16[[i]] <- ends_2016[ends_2016$batter == batIds[i],]
}

rm(enoughAbs16)
enoughAbs16 <- c()
for (i in 1:length(byBat16)) {
  if (nrow(byBat16[[i]]) > 100) { 
    enoughAbs16[length(enoughAbs16)  + 1] <- i
  }
}
enough16 <- byBat16[enoughAbs16]

rm(ba16)
ba16 <- data.frame(player = numeric(),
                  id = numeric(),
                  ba = numeric())
for (x in 1:length(enough16)) {
  all_ab <- enough16[[x]]
  forab <- all_ab[all_ab$events %in% for_ab_dic,]
  ba16[x,1] <- enough16[[x]][1,'player_name']
  ba16[x,2] <- enough16[[x]][1,'batter']
  ba16[x,3] <- nrow(forab)/nrow(all_ab)
  rm(all_ab,forab)
}

ba16$xba <- lapply(enough16, xba)
ba16$xba <- as.numeric(ba16$xba)
ba16$diff <- ba16$ba - ba16$xba
ba16$game_year <- 2016
view(ba16)

#####
# 2017 #
#####

rm(byBat17)
byBat17 <- list()
for (i in 1:length(batIds)) {
  byBat17[[i]] <- ends_2017[ends_2017$batter == batIds[i],]
}

rm(enoughAbs17)
enoughAbs17 <- c()
for (i in 1:length(byBat17)) {
  if (nrow(byBat17[[i]]) > 100) { 
    enoughAbs17[length(enoughAbs17)  + 1] <- i
  }
}
enough17 <- byBat17[enoughAbs17]

rm(ba17)
ba17 <- data.frame(player = numeric(),
                   id = numeric(),
                   ba = numeric())
for (x in 1:length(enough17)) {
  all_ab <- enough17[[x]]
  forab <- all_ab[all_ab$events %in% for_ab_dic,]
  ba17[x,1] <- enough17[[x]][1,'player_name']
  ba17[x,2] <- enough17[[x]][1,'batter']
  ba17[x,3] <- nrow(forab)/nrow(all_ab)
  rm(all_ab,forab)
}

ba17$xba <- lapply(enough17, xba)
ba17$xba <- as.numeric(ba17$xba)
ba17$diff <- ba17$ba - ba17$xba
ba17$game_year <- 2017
view(ba17)

#####
# 2018 #
#####

rm(byBat18)
byBat18 <- list()
for (i in 1:length(batIds)) {
  byBat18[[i]] <- ends_2018[ends_2018$batter == batIds[i],]
}

rm(enoughAbs18)
enoughAbs18 <- c()
for (i in 1:length(byBat18)) {
  if (nrow(byBat18[[i]]) > 100) { 
    enoughAbs18[length(enoughAbs18)  + 1] <- i
  }
}
enough18 <- byBat18[enoughAbs18]

rm(ba18)
ba18 <- data.frame(player = numeric(),
                   id = numeric(),
                   ba = numeric())
for (x in 1:length(enough18)) {
  all_ab <- enough18[[x]]
  forab <- all_ab[all_ab$events %in% for_ab_dic,]
  ba18[x,1] <- enough18[[x]][1,'player_name']
  ba18[x,2] <- enough18[[x]][1,'batter']
  ba18[x,3] <- nrow(forab)/nrow(all_ab)
  rm(all_ab,forab)
}

ba18$xba <- lapply(enough18, xba)
ba18$xba <- as.numeric(ba18$xba)
ba18$diff <- ba18$ba - ba18$xba
ba18$game_year <- 2018
view(ba18)

#####
# 2019 #
#####

rm(byBat19)
byBat19 <- list()
for (i in 1:length(batIds)) {
  byBat19[[i]] <- ends_2019[ends_2019$batter == batIds[i],]
}

rm(enoughAbs19)
enoughAbs19 <- c()
for (i in 1:length(byBat19)) {
  if (nrow(byBat19[[i]]) > 100) { 
    enoughAbs19[length(enoughAbs19)  + 1] <- i
  }
}
enough19 <- byBat19[enoughAbs19]

rm(ba19)
ba19 <- data.frame(player = numeric(),
                   id = numeric(),
                   ba = numeric())
for (x in 1:length(enough19)) {
  all_ab <- enough19[[x]]
  forab <- all_ab[all_ab$events %in% for_ab_dic,]
  ba19[x,1] <- enough19[[x]][1,'player_name']
  ba19[x,2] <- enough19[[x]][1,'batter']
  ba19[x,3] <- nrow(forab)/nrow(all_ab)
  rm(all_ab,forab)
}

ba19$xba <- lapply(enough19, xba)
ba19$xba <- as.numeric(ba19$xba)
ba19$diff <- ba19$ba - ba19$xba
ba19$game_year <- 2019
view(ba19)

#####
# 2020 #
#####

rm(byBat20)
byBat20 <- list()
for (i in 1:length(batIds)) {
  byBat20[[i]] <- ends_2020[ends_2020$batter == batIds[i],]
}

rm(enoughAbs20)
enoughAbs20 <- c()
for (i in 1:length(byBat20)) {
  if (nrow(byBat20[[i]]) > 100) { 
    enoughAbs20[length(enoughAbs20)  + 1] <- i
  }
}
enough20 <- byBat20[enoughAbs20]

rm(ba20)
ba20 <- data.frame(player = numeric(),
                   id = numeric(),
                   ba = numeric())
for (x in 1:length(enough20)) {
  all_ab <- enough20[[x]]
  forab <- all_ab[all_ab$events %in% for_ab_dic,]
  ba20[x,1] <- enough20[[x]][1,'player_name']
  ba20[x,2] <- enough20[[x]][1,'batter']
  ba20[x,3] <- nrow(forab)/nrow(all_ab)
  rm(all_ab,forab)
}

ba20$xba <- lapply(enough20, xba)
ba20$xba <- as.numeric(ba20$xba)
ba20$diff <- ba20$ba - ba20$xba
ba20$game_year <- 2020
view(ba20)

#####
# plots #
#####

# we here #
# to do: create func to calc hit prob col #
#        calc ba on that column (xba) #
#        create plot to compare ba and xba 
#        create func to create xba col #
#        create func to compare ba to xba, maybe sort the dataframe #
#        create func to compile across years then graph
#        create function to do it all

ba <- bind_rows(ba15,ba16,ba17,ba18,ba19,ba20)
ba.melt <- melt(ba, c('player','id','game_year'), c('ba','xba'))

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

# guy that relies on the elemets we are considering # 
jgp <- plot.xba.ba(ba.melt, 'Joey Gallo')
jgp

# could do this analysis (will do worse) # 
mbp <- plot.xba.ba(ba.melt, 'Mookie Betts')
mbp

# overpredict positive #
jip <- plot.xba.ba(ba.melt, 'Jose Iglesias')
jip

# eh #
tpp <- plot.xba.ba(ba.melt, 'Tommy Pham')
tpp

# reasonalble positive prediction #
mcp <- plot.xba.ba(ba.melt, 'Miguel Cabrera')
mcp

# eh #
jpp <- plot.xba.ba(ba.melt, 'Joe Panik')
jpp

# example model doesn't account for speed #
bhp <- plot.xba.ba(ba.melt, 'Billy Hamilton')
bhp

#  #
ncp <- plot.xba.ba(ba.melt, 'Nelson Cruz')
ncp

#  # 
app <- plot.xba.ba(ba.melt, 'Albert Pujols')
app

#####
# accuracey #
#####

ac15 <- nrow(ba15[abs(ba15$diff) < .04,])/nrow(ba15)
ac16 <- nrow(ba16[abs(ba16$diff) < .04,])/nrow(ba16)
ac17 <- nrow(ba17[abs(ba17$diff) < .04,])/nrow(ba17)
ac18 <- nrow(ba18[abs(ba18$diff) < .04,])/nrow(ba18)
ac19 <- nrow(ba19[abs(ba19$diff) < .04,])/nrow(ba19)
ac20 <- nrow(ba20[abs(ba20$diff) < .04,])/nrow(ba20)

ac15
ac15
ac17
ac18
ac19
ac20