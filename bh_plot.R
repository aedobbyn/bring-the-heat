# bh_plot.R


# ----- plots for Wildfire 2015 data ----------


#---#---# to do #---#---#
# time series plot of density of all actions over time of game / point
# scale point_elapsed_seconds and elapsed_time


# the schedule, graphically
plot_by_game <- ggplot(bh_all, aes(opponent, ind.game))
plot_by_game + geom_point()


# just take first game
bh_game1 <- bh_all %>%
  filter(
    ind.game == '1'
  )

# each individual action over the course of the game plotted as one point
tseries.game1 <- ggplot(bh_game1, aes(elapsed_time, action))
tseries.game1 + geom_point()


# get cumulative sums just for game 1
bh_all_cumsums_g1 <- bh_all_cumsums %>%
  filter(
    ind.gme == '1'
    ) 


# limit to games played against the Radicals
rad_games <- bh_all_cumsums %>%
  filter(
    opponent == 'Radicals'
  ) %>%
  group_by(
    game
  ) %>%
  ungroup()
rad_games


# plot number of Ds per game over time, smoothed
rad_plot_blocks <- ggplot(data = rad_games, aes(x = elapsed_time, colour = game))
rad_plot_blocks + geom_smooth(aes(y = cum.block)) 

# same but with faceting
rad_plot_blocks_facet <- ggplot(data = rad_games, aes(x = elapsed_time)) + facet_grid(. ~ game)
rad_plot_blocks_facet + geom_smooth(aes(y = cum.block)) 

# our score, their score, our total number of Ds
rad_plot <- ggplot(data = rad_games, aes(x = elapsed_time), group = game) + facet_grid(. ~ game)
rad_plot + geom_smooth(aes(y = cum.block, colour = "green")) +
  geom_smooth(aes(y = our_score, colour = "blue")) +   # different when you use cum.goal but it shouldn't be...
  geom_smooth(aes(y = their_score, colour = "red")) + 
  scale_colour_discrete(name = "Running Totals", 
                       labels = c("Their Score", "Our Ds", "Our Score"))  # also need to play around with order of these
  


# ~ experimenting
rad_plot_exp <- ggplot(data = rad_games, aes(x = elapsed_time)) + facet_grid(. ~ game)
rad_plot_exp + geom_point(aes(y = cum.block, colour = defender)) + 
  geom_line(aes(y = cum.goal, colour = receiver)) 


# plot each player's season blocks vs. their goals
mvp <- bh_all_cumsums %>%
  ungroup() %>%
  group_by(receiver) %>%
  summarise(
  season.blocks = max(cum.block),
  season.goals = max(cum.goal)
  ) %>%
  arrange(desc(
    season.blocks
  )) %>%
  print(n = 50)

mvp_plot <- ggplot(data = mvp, aes(x = season.blocks, y = season.goals, colour = receiver))
mvp_plot + geom_point()




