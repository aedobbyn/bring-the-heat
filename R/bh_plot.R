# bh_plot.R

source("./R/bh_prep.R")

# ----- plots for Wildfire 2015 data ----------


#---#---# to do #---#---#
# time series plot of density of all actions over time of game / point
# scale point_elapsed_seconds and elapsed_time


# the schedule, graphically
plot_by_game <- ggplot(bh_all, aes(opponent, ind.gme))
plot_by_game + geom_point()


# just take first game
bh_game1 <- bh_all %>%
  filter(
    ind.gme == '1'
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
  geom_smooth(aes(y = our_score, colour = "blue")) +  
  geom_smooth(aes(y = their_score, colour = "red")) + 
  scale_colour_discrete(name = "Running Totals", 
                       labels = c("Our Score", "Our Ds", "Their Score"))  # need to play around with order of these
  


# in Radicals games, individual players' cumulative Ds and goals
rad_plot_exp <- ggplot(data = rad_games, aes(x = elapsed_time)) + facet_grid(. ~ game)
rad_plot_exp + geom_point(aes(y = cum.block, colour = defender)) + 
  geom_point(aes(y = cum.goal, colour = receiver)) 



# get measure of mvp as goals vs. blocks over the season
mvp <- bh_all_cumsums %>%
  ungroup() %>%
  group_by(receiver) %>%
  filter(
    receiver != "Anonymous"
  ) %>%
  summarise(
  season.blocks = max(cum.block),
  season.goals = max(cum.goal)
  ) %>%
  arrange(desc(
    season.blocks
  )) %>%
  print(n = 50)


# plot each player's season blocks vs. their goals
mvp_plot <- ggplot(data = mvp, aes(x = season.blocks, y = season.goals, 
                                   label = receiver, angle = 45)) # , colour = receiver
mvp_plot + geom_point() +
  # scale_colour_discrete(name = "Player Name") +
  ggtitle("MVP") +
  xlab("Season Blocks") + 
  ylab("Season Goals") +
  # geom_text(hjust = 0, nudge_x = 0.3)
  geom_text_repel(aes(label = receiver), 
                box.padding = unit(0.45, "lines"))



# take out Anonymous
mvplus_minus <- plus_minus %>% 
  filter(name != "Anonymous") 

mvplus_minus %>% print(n = 50)


mvp_plot_2 <- ggplot(data = mvplus_minus, aes(x = Ds, y = goals, 
                                   label = name, angle = 45)) # , colour = receiver
mvp_plot_2 + geom_point() +
  # scale_colour_discrete(name = "Player Name") +
  ggtitle("MVP") +
  xlab("Season Blocks") + 
  ylab("Season Goals") +
  # geom_text(hjust = 0, nudge_x = 0.3)
  geom_text_repel(aes(label = name), 
                  box.padding = unit(0.45, "lines"))

