install.packages("ggplot2")
install.packages("e1071")
install.packages("tidyverse")
install.packages("dplyr")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("ggplot2")
library(tidyverse)
library(lubridate)
library("dplyr")
library("ggpubr")

base_info = read.csv("./csv/carlsen_games.csv")
base_info$date_played <- as.Date(ymd(base_info$date_played))

won <- base_info[base_info$group_name_result == "won",]
draw <- base_info[base_info$group_name_result == "draw",]
lost <- base_info[base_info$group_name_result == "lost",]
black_games <- base_info[base_info$group_name_color == "black",]
white_games <- base_info[base_info$group_name_color == "white",]


#liczba wszystkich partii
nrow(base_info)
#liczba wygrnaych
nrow(won)
#liczba remisów
nrow(draw)
#liczba przegranych
nrow(lost)
#liczba partii białymi
nrow(white_games)
#liczba partii czarnymi
nrow(black_games)

#wyciągniecie elo przeciwników i Carlsena z bazy
opponent_elo = 1:(nrow(base_info))
carlsen_elo = 1:(nrow(base_info))

for(i in 1:nrow(base_info)) {
  if(paste(base_info$white[i],".pgn",sep="") == base_info$file_name[i]){
    opponent_elo[i] <- base_info$black_elo[i]
    carlsen_elo[i] <- base_info$white_elo[i]
  }
  else {
    opponent_elo[i] <- base_info$white_elo[i]
    carlsen_elo[i] <- base_info$black_elo[i]
  }
}
mean(opponent_elo)
mean(carlsen_elo)

#przedziały ufności dla ELo Carlsena z alfa = 5%
mean(carlsen_elo) - qnorm(1-0.001/2)*sd(carlsen_elo)/sqrt(length(carlsen_elo))
mean(carlsen_elo) + qnorm(1-0.001/2)*sd(carlsen_elo)/sqrt(length(carlsen_elo))

#liczenie średniego elo przecinika dla wyganrej partii
opponent_elo_win_sum = 0
opponent_elo_win_count = 0
for(i in 1:nrow(won)) {
  if(won$game_id[i] %in% white_games$game_id){
    opponent_elo_win_sum = opponent_elo_win_sum + 
      white_games$black_elo[match(won$game_id[i], white_games$game_id)]
    opponent_elo_win_count = opponent_elo_win_count + 1
  }
  if(won$game_id[i] %in% black_games$game_id){
    opponent_elo_win_sum = opponent_elo_win_sum + 
      black_games$white_elo[match(won$game_id[i], black_games$game_id)]
    opponent_elo_win_count = opponent_elo_win_count + 1
  }
}
print(opponent_elo_win_sum/opponent_elo_win_count)

#średnie elo przeciwnika dl remisu
opponent_elo_draw_sum = 0
opponent_elo_draw_count = 0
for(i in 1:nrow(draw)) {
  if(draw$game_id[i] %in% white_games$game_id){
    opponent_elo_draw_sum = opponent_elo_draw_sum + 
      white_games$black_elo[match(draw$game_id[i], white_games$game_id)]
    opponent_elo_draw_count = opponent_elo_draw_count + 1
  }
  if(draw$game_id[i] %in% black_games$game_id){
    opponent_elo_draw_sum = opponent_elo_draw_sum + 
      black_games$white_elo[match(draw$game_id[i], black_games$game_id)]
    opponent_elo_draw_count = opponent_elo_draw_count + 1
  }
}
print(opponent_elo_draw_sum/opponent_elo_draw_count)

#średnie elo przeciwnika dl przegranej
opponent_elo_lost_sum = 0
opponent_elo_lost_count = 0
for(i in 1:nrow(lost)) {
  if(lost$game_id[i] %in% white_games$game_id){
    opponent_elo_lost_sum = opponent_elo_lost_sum + 
      white_games$black_elo[match(lost$game_id[i], white_games$game_id)]
    opponent_elo_lost_count = opponent_elo_lost_count + 1
  }
  if(lost$game_id[i] %in% black_games$game_id){
    opponent_elo_lost_sum = opponent_elo_lost_sum + 
      black_games$white_elo[match(lost$game_id[i], black_games$game_id)]
    opponent_elo_lost_count = opponent_elo_lost_count + 1
  }
}
print(opponent_elo_lost_sum/opponent_elo_lost_count)


game_result <- 1:nrow(base_info)
for(i in 1:nrow(base_info)){
  if(base_info$group_name_result[i] == "won"){
    game_result[i] <- 2
  }
  if(base_info$group_name_result[i] == "draw"){
    game_result[i] <- 1
  }
  if(base_info$group_name_result[i] == "lost"){
    game_result[i] <- 0
  }
}

#korealcja między elo przecinika a rezulatatem partii
cor(opponent_elo,game_result)

#średnia różnica elo między Carlsenem a przeicnikiem
mean(carlsen_elo-opponent_elo)

#wariancje i odchylenia standardowe
var(opponent_elo)
sd(opponent_elo)
var(carlsen_elo)
sd(carlsen_elo)

#%wygrancyh
nrow(won)/nrow(base_info) * 100




#painting plot of win% per nick
carlsen_nickname = substr(base_info$file_name,1,nchar(base_info$file_name)-4)
carlsen_nickname = data.frame(carlsen_nickname)
carlsen_nickname = distinct(carlsen_nickname)
carlsen_nickname$carlsen_nickname[2] <- "DannyTheDonkey"
wins_for_nickname = vector("numeric",nrow(carlsen_nickname))
total_for_nickname = vector("numeric",nrow(carlsen_nickname))
for(i in 1:nrow(base_info)){
  if(base_info$white[i] %in% carlsen_nickname$carlsen_nickname) {
    if(base_info$result[i] == "1-0"){
      wins_for_nickname[match(base_info$white[i], carlsen_nickname$carlsen_nickname)] =
        wins_for_nickname[match(base_info$white[i], carlsen_nickname$carlsen_nickname)] + 1  
    }
    total_for_nickname[match(base_info$white[i], carlsen_nickname$carlsen_nickname)] =
      total_for_nickname[match(base_info$white[i], carlsen_nickname$carlsen_nickname)] + 1
  }
  else{
    if(base_info$result[i] == "0-1"){
      wins_for_nickname[match(base_info$black[i], carlsen_nickname$carlsen_nickname)] =
        wins_for_nickname[match(base_info$black[i], carlsen_nickname$carlsen_nickname)] + 1  
    }
    total_for_nickname[match(base_info$black[i], carlsen_nickname$carlsen_nickname)] =
      total_for_nickname[match(base_info$black[i], carlsen_nickname$carlsen_nickname)] + 1
  }
}
wins_for_nickname
total_for_nickname
win_ratio_nickname = wins_for_nickname / total_for_nickname * 100
win_ratio_nickname = data.frame(carlsen_nickname,win_ratio_nickname)
ggplot(data=win_ratio_nickname, aes(x=carlsen_nickname, y=win_ratio_nickname)) +
  geom_bar(stat="identity", fill="steelblue")+ 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.title.x=element_blank(),
    axis.title.y=element_blank()) + ggtitle("Average win ratio per nick") +
  theme(plot.title = element_text(hjust = 0.5))

#winratio per month plot creation
win_ratio_per_month = vector("numeric",12)
sum_of_games_pet_month = vector("numeric",12)
win_ratio_per_month
for(i in 1:nrow(base_info)){
  #print(i)
  if(game_result[i] == 2){
    win_ratio_per_month[month(base_info$date_played[i])] <- win_ratio_per_month[month(base_info$date_played[i])] + 1
  }
  sum_of_games_pet_month[month(base_info$date_played[i])] <- sum_of_games_pet_month[month(base_info$date_played[i])] + 1
}
month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
tmp_ratio = win_ratio_per_month/sum_of_games_pet_month * 100
win_ratio_per_month = data.frame(month, tmp_ratio)
win_ratio_per_month
ggplot(data=win_ratio_per_month, aes(x=month, y=tmp_ratio, group=1)) +
  geom_line(color="red", size = 1.3)+
  geom_point(color = "red", size = 2)+
  labs(y = "win ratio[%]") +
  ylim(0,100) + 
  scale_x_discrete(name = "Month", limits=month) + ggtitle("Average win ratio per month") +
  theme(plot.title = element_text(hjust = 0.5))



#badanie różnicy w rankigach
carlen_elo_diff = vector("numeric",nrow(base_info))
for(i in 1:nrow(base_info)) {
  if(base_info$group_name_color[i] == "black") {
    carlen_elo_diff[i] <- base_info$black_rating_diff[i]
  }
  else{
    carlen_elo_diff[i] <- base_info$white_rating_diff[i]
  }
}

carlen_elo_diff <- data.frame(carlen_elo_diff)
ggplot(data = carlen_elo_diff, aes(x = carlen_elo_diff)) +
  geom_histogram(colour="black", fill="aquamarine") + xlim(-30,30) +
  labs(x = "Carlsen elo diff") + ggtitle("Hisotrgam of Carlen's elo difference") +
  theme(plot.title = element_text(hjust = 0.5))


library(e1071)

#elo przez cały czas
base_info$carlsen_elo <- carlsen_elo
ggplot(data = base_info, aes(x=carlsen_elo)) + 
  geom_histogram(colour="black", fill="aquamarine")+
  labs(x = "Carlsen elo") + ggtitle("Historgram of Carlen's elo overtime") +
  theme(plot.title = element_text(hjust = 0.5))


#współczynnik asymetri elo carlsena
moment_rzedu_3 <- moment(base_info$carlsen_elo, order = 3, center = TRUE)
wspolcz_sym <- moment(base_info$carlsen_elo, order = 3, center = TRUE)/(sd(base_info$carlsen_elo)^3)
wspolcz_sym
#kurtoza elo carlsena
kurtoza = moment(base_info$carlsen_elo, order = 4, center = TRUE)/sd(base_info$carlsen_elo)^4
kurtoza

#tesotwanie naturaloności rozkąldu 
set.seed(1234)
carlsen_elo = data.frame(carlsen_elo)
shapiro.test(sort(carlsen_elo[sample(nrow(carlsen_elo), 5000), ]))
res = c(0.80161, 0.78163,0.79725 , 0.77146 ,0.78759)
mean(res)
library(ggpubr)
ggqqplot(carlsen_elo$carlsen_elo, main="Q-Q plot of Carlsen's elo")

shapiro.test(sort(carlen_elo_diff[sample(nrow(carlen_elo_diff), 5000), ]))
res = c(0.27506,0.24222,0.25567,0.28048,0.53423)
mean(res)
library(ggpubr)
ggqqplot(carlen_elo_diff$carlen_elo_diff, main="Q-Q plot of difference in Carlsen's elo")

