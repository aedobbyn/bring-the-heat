# bh_explore.R

# exploratory things related to Chicago Wildfire 2015 data

# ...... bh_orig ......

# ...... bh_all .......
# all variables

# ........ bh .........
# bh_all pared down to variables we care about most
# (opponent, our_score, their_score, action, elapsed_time, game, ind.gme, actor, name)
# gathers (passer, receiver, defender) into one column, actor
# with corresponding name column


# bh_all_cumsums
# cumulative sums of goals, Ds, etc.

# bh_all_scale_time
# adds column for elapsed time scaled




# ----------- plots ----------



# for time series, probably will have to
# a) standardize things
# b) bin time into intervals like 5 mins or 1 point

# tseries.game1 <- ggplot(bh_game1, aes(elapsed_time, count),
#                         fill=action)
# tseries.game1 + geom_histogram()


# time series for game 1
tseries.g1.acts <- ggplot(data = bh_all_cumsums_g1, aes(x = elapsed_time)) +
  geom_line(aes(y = cum.goal, colour='green')) +
  geom_line(aes(y = cum.block, colour = 'blue')) +
  geom_line(aes(y = cum.throwaway, colour = 'red')) +
  geom_line(aes(y = cum.drop, colour = 'orange')) +
  xlab('Elapsed Time') +
  ylab('Cumulative number of actions') + 
  title('Actions over first game')
tseries.g1.acts



# plot each player's actions (averaged)
# colour = their name, size = number of Ds
# facets = action
# maybe set a threshold (at least 4 Ds or something)


# plus minus plot
plus_minus_dropAnonymous <- plus_minus %>%
  filter(
    name != 'Anonymous'
  )
p_m_plot <- ggplot(data = plus_minus_dropAnonymous, aes(x = Ds, y = goals, colour = name)) 
p_m_plot + geom_point()

# get per game plus minus
plus_minus_by_game <- plus_minus %>%
  group_by(ind.gme)












# --------- models --------

# how well does the line (O or D) predict whether we will score?

# add a column for point number


bh_add.point <- bh_all %>%
  mutate(
    point_num = our_score + their_score
  )


bh_do_we_score <- bh %>%
  mutate(
    point_num = our_score + their_score
  ) %>%
  group_by(point_num) %>%
  mutate(
    we_score = ifelse(action == 'Goal', 'yes', 'no')
  )



# add we_score column
bh_add.point$we_score <- NULL



dowescore <- function(point) {
  for (row in point) {
    if (action == 'Goal') {
      we_score = 'yes'
    } else {
      we_score = 'no'
    }
  }
}

# run the function
dowescore(bh_add.point$point_num)

# model
line_mod <- lm(we_score ~ line, data = bh_add.point)


