# bh_stats.R

# summary stats and models

# ----- stats for Wildfire 2015 data ----------



# get summary of the different opponent-date pairs (i.e., individual games)
bh_all.by.game <- bh_all %>%
  group_by(opponent, date) %>%
  distinct(date)
bh_all.by.game

# make that summary a tibble
bh_all.by.game <- as_tibble(bh_all.by.game)

# order bh_all.by.game by date
# bh_all.by.game <- bh_all.by.game[order(bh_all.by.game$date)]

# take just first three columns 
bh_all.by.game.trim <- bh_all.by.game[, 1:3]


# spread into sparse matrix by date
bh_all.by.game.trim <- bh_all.by.game.trim %>%
  ungroup %>%
  rename(
    dte = date
  ) %>%
  select(
    opponent, dte
  ) %>%
  mutate(row = 1:n()) %>%  # have to add this so don't get an Error: Duplicate identifiers for rows
  spread(
    key = opponent,
    value = dte) 
bh_all.by.game.trim


# vertical representation of bh_all.by.game.trim
# order by date
bh_nest <- bh_all %>%
  group_by(opponent, date) %>% 
  nest() %>%
  arrange(
    date
  )
bh_nest



# ---------------- action counts ----------------

# summary count of number of actions
count.actions <- bh_all %>%
  select(action) %>%
  group_by(
    action
  ) %>%
  summarise(
    num = n()
  )
count.actions


# summary of final scores in all games 
bh_all_scoreSum <- bh_all %>%
  group_by(opponent, game) %>%
  summarise(
    our_final_score = max(our_score),
    their_final_score = max(their_score)
  )
bh_all_scoreSum



# add window column for total number of each action per game
total.up.acts <- bh_all %>%
  select(
    elapsed_time, 
    ind.gme, action
  ) %>%
  group_by(ind.gme, action) %>%
  mutate(
    total.per.act = n()
  )
total.up.acts
tail(total.up.acts)


# who got the most Ds?
Ds_perPlayer <- bh_all %>%      
  group_by(defender) %>%
  filter(action == 'D', !is.na(defender), defender != '') %>%
  select(defender, action) %>%
  summarise(
    blocks = n() 
  ) %>%
  arrange(desc(blocks)) %>%
  print(n=30)

Ds_perPlayer

# most goals?
goals_perPlayer <- bh_all %>%      
  group_by(receiver) %>%
  filter(action == 'Goal', !is.na(receiver), receiver != '') %>%
  select(receiver, action) %>%
  summarise(
    goals = n() 
  ) %>%
  arrange(desc(goals)) %>%
  print(n=30)

goals_perPlayer


# calculate plus minus per player
plus_minus <- bh %>%
  select(
    name, action
  ) %>%
  group_by(
    name
  ) %>%
  summarise(
    goals = sum(action == 'Goal'),
    Ds = sum(action == 'D'),
    throwaways = sum(action == 'Throwaway'),
    drops = sum(action == 'Drop'),
    p_m = (goals + Ds) - (throwaways + drops)
  ) %>%
  arrange(desc(
    p_m
  )) %>%
  print(n = 50)


# plus minus per player per game      # doesn't work yet
plus_minus_per_game <- bh %>%
  select(
    name, action, ind.gme
  ) %>%
  group_by(
    name, ind.gme
  ) %>%
  summarise(
    goals = mean(action == 'Goal'),
    Ds = mean(action == 'D'),
    throwaways = mean(action == 'Throwaway'),
    drops = mean(action == 'Drop'),
    p_m = (goals + Ds) - (throwaways + drops)
  ) %>%
  ungroup() %>%
  nest(
    name
  ) %>%
  arrange(desc(
    p_m
  )) %>%
  print(n = 50)
  



# add cumulative columns per individual game
bh_all_cumsums <- bh_all %>%
  # select(action, opponent, ind.gme) %>%
  group_by(
    ind.gme
  ) %>%
  mutate(
    catch = ifelse(action=='Catch', 1, 0),
    cum.catch = cumsum(catch), 
    block = ifelse(action=='D', 1, 0),
    cum.block = cumsum(block), 
    throwaway = ifelse(action=='Throwaway', 1, 0),
    cum.throwaway = cumsum(throwaway), 
    drop = ifelse(action=='Drop', 1, 0),
    cum.drop = cumsum(drop), 
    goal = ifelse(action=='Goal', 1, 0),
    cum.goal = cumsum(goal)
  ) 
bh_all_cumsums
tail(bh_all_cumsums)




# scale elapsed time over game
# add to big tibble
bh_all_scale_time <- bh_all_cumsums %>%
  group_by(
    ind.gme
  ) %>%
  mutate(
    e_time_scale = scale(elapsed_time)
  )

# add scaled elapsed time column to bh
bh_scale_time <- bh %>%
  group_by(
    ind.gme
  ) %>%
  mutate(
    e_time_scale = scale(elapsed_time)
  )
bh_scale_time








# ----------------------------------------- models ------------------------------------------

# get a running point diff column
bh_pt.diff <- bh_all %>%
  mutate(
    pt_diff = our_score - their_score
  )

# linear model with opponent, elapsed_time, and their interaction predicting difference in 
# our_score and their_score
pt.diff.mod.e_time <- lm(pt_diff ~ opponent * elapsed_time, data = bh_pt.diff)

# take opponent out of the model
pt.diff.e_time.no.opponent <- update(pt.diff.mod.e_time, . ~ . - opponent)
  
library(lmtest)
# test whether model with opponent (pt.diff.mod) is better at predicting point diff than model without
lrtest(pt.diff.mod.e_time, pt.diff.e_time.no.opponent) # yes, opponent is a significant predictor of point diff






# model with just opponent
pt.diff.mod <- lm(pt_diff ~ opponent, data = bh_pt.diff)

# take opponent out of the model
pt.diff.no.opponent <- update(pt.diff.mod, . ~ . - opponent)

library(lmtest)
# test whether model with opponent (pt.diff.mod) is better at predicting point diff than model without
lrtest(pt.diff.mod, pt.diff.no.opponent) # yes, opponent is a significant predictor of point diff





 




