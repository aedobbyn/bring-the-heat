# Wildfire 2015 data

# import packages
library(RPostgreSQL)
library(tibble)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(data.table)  # looks like dplyr and data.table became dtplyr, so
library(dtplyr)
library(ggplot2)

# ------- set wd ---------

setwd("/Users/amanda/Desktop/post_exCog/bring_heat")



# ---------------- to import from database ----------------

# # set up driver as postgres
# drv <- dbDriver("PostgreSQL")
# 
# # set connection to our db
# con <- dbConnect(drv, dbname="pullplay_db", host='localhost', port=5432, user="amanda")
# 
# # select all rows
# bring_heat_2 <- dbGetQuery(con, "SELECT * FROM bring_heat_2") 



# ----------- to import from local spreadsheet -------------

# import csv
bring_heat_2 <- read_csv('./ChicagoWildfire2015-stats.csv')
                         # , na='') # read in empty cells as NAs

# make tibble
bh <- as_tibble(bring_heat_2)

# see head
# bh

# check variable datatypes
# str(bh)



# ----------- change up variable names -----------

# get rid of players 8 through 27 because they will never exist
bh <- bh %>%
  select(
    -(`Player 8`:`Player 27`)
  )

# replace all spaces in column names with underscores
names(bh) <- str_replace_all(names(bh), " ", "_")
names(bh)

# take out the end of point stuff
names(bh) <- str_replace_all(names(bh), '_-_End_of_Point', '')
names(bh)

# take out the seconds unit at the end of hang time and elapsed time
# easier with gsub than with str_replace because of parentheses
names(bh) <- gsub('_(secs)', '', names(bh), fixed = TRUE)
names(bh)

# rename column headers to lowercase
names(bh) <- tolower(names(bh))


# rename date/time column to date_time
bh <- bh %>% 
  rename(
    date_time = `date/time`
  )

# check out our variable names
names(bh)

# looks good




# -------------------- manipulate variable types ------------------------ 

# check out what we have
# str(bh)

# # change date_time from double to date
# bh3 <- bh
# bh3 <- bh3 %>%
#   parse_datetime(
#     date_time, ''
#   )


# make vector of columns we want as factor
want.as.factor <- c('tournamemnt', 'opponent', 'line', 'event_type', 'action', 
                    'passer', 'receiver', 'defender',
                    'player_0', 'player_1', 'player_2', 'player_3', 'player_4', 'player_5',
                    'player_6', 'player_7')


# make these columns factors
bh[, want.as.factor] <- data.frame(apply(bh[, want.as.factor], 2, as.factor))

# make vector of columns we want as numeric
want.as.numeric <- c('point_elapsed_seconds', 'our_score', 'their_score', 
                     'hang_time', 'elapsed_time')

# make these columns numeric
bh[, want.as.numeric] <- data.frame(apply(bh[, want.as.numeric], 2, as.numeric))

# check out head
bh

# see if tournament is always NA
# careful of the typo -- variable name is tournamenmt
summary(bh$tournamemnt)

# it is so take it out
bh <- bh %>%
  select(
    -tournamemnt
  )

# clean up the opponent column
levels(bh$opponent)

# take vs in all its forms out of the opponents column
levels(bh$opponent) <- levels(bh$opponent) %>%
                                 str_replace_all(c('vs ', 'vs. ', 'Vs. '), '')

# check that our levels of opponent look right
levels(bh$opponent)

# we have two types of alleycats: Alleycats and AlleyCats. make them into one
bh$opponent[bh$opponent=='AlleyCats'] <- 'Alleycats'

# drop the unused level
bh <- bh %>%
  droplevels()

# check that it worked
levels(bh$opponent)



# ----------------- check out incomplete cases ------------
# do we have NAs?
num.NAs <- sum(!complete.cases(bh))
num.NAs
# yup, every row has at least one

# varies a lot by column
sum(!complete.cases(bh$player_1))
sum(!complete.cases(bh$player_7))



# keep all columns in a bh_all tibble
bh_all <- bh



# ---------------------------------

# compare average time of a point to average time of a game
summary(bh_all$point_elapsed_seconds)    # point_elapsed_seconds is total number of seconds a point took
summary(bh_all$elapsed_time)             # elapsed_time starts at 0 at beginning of game and gets a value every time an action happens

# big difference between median and mean of elapsed_time

# check for an outlier
qplot(data = bh_all, opponent, elapsed_time)
# outlier in one of the games against Revolution -- maybe a typo





# ---------- make column for individual game number and game number against each opponent ----------------

# split date_time into date and time columns
# so that we can differentiate different games played against the same opponent
# these will have the same oppoenent name an different dates
bh_all <- separate(bh_all, date_time, into = c("date", "time"), sep=" ") 

# get date in date format
bh_all$date <- ymd(bh_all$date)

# check that the class is date
class(bh_all$date)

# time column doesn't matter because it's the same for each date (gives us no 
# more information than date), so we can ignore / get rid of it

# add a column for ~~~~~~~~ game number against each opponent ~~~~~~~
bh_all <- bh_all %>%
  group_by(opponent) %>%
  mutate(
    game = as.numeric(factor(date))
  )

# make game a factor
bh_all$game <- factor(bh_all$game)

# check out
summary(bh_all$game)


# make a column for each ~~~~~~~ individual game ~~~~~~~ 
# (there are 13 this season) using data.table
library(data.table)
bh_all <- setDT(bh_all)[, ind.gme := .GRP, by = .(date, opponent), ]

# turn back into tibble
bh_all <- as_tibble(bh_all)
bh_all 

# check that our last ind.gme value is 13
tail(bh_all[, c(1, 3, 23:24)])





# ---------- pare down to columns we really care about -------
bh <- bh_all

bh <- bh %>%
  select (
    opponent, our_score, their_score,
    action, 
    passer, receiver, defender, 
    elapsed_time, game, ind.gme
  )




# ----------- gather player into one column ----------

# keep original version in orig tibble
bh.orig <- bh

# gather passer, reciever, and defender into one column, actor
# make corresponding name column to name each actor
bh <- bh %>% 
  tidyr::gather(
    key = actor,
    value = name,
    passer:defender,
    na.rm=T
  ) %>%
  print(n=20)

tail(bh)


# make actor and name factors
bh$actor <- factor(bh$actor)
bh$name <- factor(bh$name)











