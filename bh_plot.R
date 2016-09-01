# bh_plot.R


# ----- plots for Wildfire 2015 data ----------


#---#---# to do #---#---#
# time series plot of density of all actions over time of game / point
# scale point_elapsed_seconds and elapsed_time


# plot each player's actions (averaged)
# colour = their name, size = number of Ds
# facets = action
# maybe set a threshold (at least 4 Ds or something)


plus_minus_dropAnonymous <- plus_minus %>%
  filter(
    name != 'Anonymous'
  )
p_m_plot <- ggplot(data = plus_minus_dropAnonymous, aes(x = Ds, y = goals, colour = name)) 
p_m_plot + geom_point()


plus_minus_by_game <- plus_minus %>%
  group_by(ind.gme)




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



# for time series, probably will have to
# a) standardize things
# b) bin time into intervals like 5 mins or 1 point

# tseries.game1 <- ggplot(bh_game1, aes(elapsed_time, count),
#                         fill=action)
# tseries.game1 + geom_histogram()


# getting cumsums just for game 1
bh_all_cumsums_g1 <- bh_all_cumsums %>%
  filter(
    ind.gme == '1'
    ) 

# 
tseries.g1.acts <- ggplot(data = bh_all_cumsums_g1, aes(x = elapsed_time)) +
  geom_line(aes(y = cum.goal, colour='green')) +
  geom_line(aes(y = cum.block, colour = 'blue')) +
  geom_line(aes(y = cum.throwaway, colour = 'red')) +
  geom_line(aes(y = cum.drop, colour = 'orange')) +
  xlab('Elapsed Time') +
  ylab('Cumulative number of actions') + 
  title('Actions over first game')
tseries.g1.acts







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


ggplot(df2, aes(x=cond1, y=yval)) + 
  geom_line(aes(colour=cond2, group=cond2)) + # colour, group both depend on cond2
  geom_point(aes(colour=cond2),               # colour depends on cond2
             size=3)                          # larger points, different shape
## Equivalent to above; but move "colour=cond2" into the global aes() mapping
# ggplot(df2, aes(x=cond1, y=yval, colour=cond2)) + 
#    geom_line(aes(group=cond2)) +
#    geom_point(size=3)




