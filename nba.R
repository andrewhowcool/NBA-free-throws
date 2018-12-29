library(tidyverse)

### import data ###
setwd("/Users/AndrewHowCool/school/Big_Data/nba")
raw_data <- fread("free_throws.csv", na.strings = "")
#View(r_data)


### Feature Construction : combine same-round free throws to one row(will combine some rows and delete the column "score") ###
# Create play_id, make_miss, technique
play_id <- paste(raw_data$game_id, raw_data$period, raw_data$time,sep = '-')
play_data <- mutate(raw_data, Play_id = play_id)
play_data$make_miss <- ifelse(grepl('make', play_data$play),1,0)
play_data$technical <- ifelse(grepl('technical', play_data$play), TRUE, FALSE)

# a function that creates binary status of freethrow status
generate_new_data<-function(z){
  if(length(z)==1){
    return(paste(z[1],-1,-1))
  }
  else if(length(z)==2){
    return(paste(z[1],z[2],-1))
  }
  else{
    return(paste(z[1],z[2],z[3]))
  }
} 

# group by  a column called binary_status
binary_data <- play_data %>%
  group_by(Play_id, end_result, game, game_id, period, player, playoffs, season, time, technical) %>%
  summarise(binary_status = generate_new_data(make_miss)) %>%
  as.data.frame()

# separate the binary_status to three individual free throw columns
separate_data <- separate(binary_data, binary_status, c("free_1", "free_2", "free_3"), " ")

# preprocessed data frame
df <- separate_data
View(df)


### which player is best or worst at free throws? ####
# best free throwers
accurate <- group_by(raw_data, player) %>%
  filter(n() > 150) %>%
  summarise(accuracy = sum(shot_made) / n()) %>%
  arrange(desc(accuracy))
accurate
# worst free throwers
inaccurate <- group_by(raw_data, player) %>%
  filter(n() > 150) %>%
  summarise(accuracy = sum(shot_made) / n()) %>%
  arrange(accuracy)  
inaccurate



### which player is the most inconsistancy? ###
# inconsistancy
inconsistancy <- group_by(raw_data, player) %>%
  filter(n() > 150) %>%
  summarise(sd = sd(shot_made)) %>%
  arrange(desc(sd))
inconsistancy
# consistancy
consistancy <- group_by(raw_data, player) %>%
  filter(n() > 150) %>%
  summarise(sd = sd(shot_made)) %>%
  arrange(sd)
consistancy


### combine consistancy and accuracy ###
cons_acc <- group_by(raw_data, player) %>%
  filter(n() > 150) %>%
  summarise(accuracy = sum(shot_made) / n(), sd = sd(shot_made))
cons_acc
qplot(cons_acc$accuracy, cons_acc$sd)
best <- filter(cons_acc, accuracy > 0.9 & sd < 0.3) %>%
  arrange(desc(accuracy))
best


### will the first free throw affect the accuracy of the second free throw? ###
# chi square -> p value < 0.05 -> there's a accuracy difference between the first shot and the second one
two_throws <- filter(df, free_2 != -1)
tbl <- table('first throw' = two_throws$free_1, 'second throw' = two_throws$free_2)
tbl
chisq.test(tbl)

# free throw accuracy for every single shot
total_accuracy <- dim(filter(play_data, make_miss == 1))[[1]] / dim(play_data)[[1]]
total_accuracy
# yy/y?
accuracy_yy <- dim(filter(df, free_1 == 1 & free_2 == 1))[[1]] / dim(filter(df, free_1 == 1 & free_2 != -1))[[1]]
accuracy_yy
# ny/n?
accuracy_ny <- dim(filter(df, free_1 == 0 & free_2 == 1))[[1]] / dim(filter(df, free_1 == 0 & free_2 != -1))[[1]]
accuracy_ny

# is winner effect for every player?
accurate <- group_by(raw_data, player) %>%
  filter(n() > 150) %>%
  summarise(accuracy = sum(shot_made) / n())
accurate

afterwin_accurate <- group_by(df, player) %>%
  filter(n() > 150 & free_1 == 1 & free_2 != -1) %>%
  summarise(after_accuracy = sum(as.numeric(free_2)) / n())
afterwin_accurate

winner_effect <- merge(accurate, afterwin_accurate)
head(winner_effect)

# plot
p <- qplot(data = winner_effect, accuracy, after_accuracy)
p + geom_abline(slope = 1, color = 'red')







### is free throw a key factor to win the game? ###

### how many free throws does a team need to win a game? ###


# testing playground

