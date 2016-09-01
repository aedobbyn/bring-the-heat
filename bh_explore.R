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


