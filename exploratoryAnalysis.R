source("importationAndCleanup.R")

library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(scales)

gr_by_year <- bikeSharing %>% group_by(yr) %>%
  summarise(
    total_casual = sum(casual),
    total_registered = sum(registered),
    total_cnt = sum(cnt)
  )


########## for each year, for each month, show total bike rentals and show mean teamp ########## 
gr_by_month <- select(bikeSharing, yr, mnth, cnt) %>% group_by(yr, mnth) %>%
  summarise(total_cnt = sum(cnt))

mean_temp_2011 <- bikeSharing %>% filter(yr == 2011) %>% group_by(mnth) %>% summarise(mean_temperature = mean(real_temp))
mean_temp_2012 <- bikeSharing %>% filter(yr == 2012) %>% group_by(mnth) %>% summarise(mean_temperature = mean(real_temp))


gp1 <- gr_by_month %>% ggplot() + geom_bar(aes(x = mnth, y = total_cnt * 35 / 220000, fill = yr), stat = "identity", position = "dodge") +
  geom_line(data = mean_temp_2011, aes(x = mnth, y = mean_temperature, group = 1,color="yellow"), show.legend = FALSE) +
  geom_line(data = mean_temp_2012, aes(x = mnth, y = mean_temperature, group = 1,color="orange"), show.legend = FALSE) +
  scale_y_continuous(name = expression("Temperatura ("~degree~"C)"),
                     sec.axis = sec_axis(~ . * 220000 / 35, name = "Nº de alugueres"),
                     limits = c(0, 35)
                     ) +
  labs(x = "Mês",fill = "Ano") +
  ggtitle("Número de alugueres totais em cada mês e ano")

gp1

####### casuals vs registered over the months  (and years) ########## 

gr_by_tudo <-
  select(bikeSharing, yr, mnth, casual, registered) %>% group_by(mnth) %>%
  summarise(casuals = sum(casual), registered = sum(registered))

gr_by_tudo2 <- gather(gr_by_tudo, type, count, casuals:registered)

gp2 <- ggplot(gr_by_tudo2, aes(mnth, count / 1000, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) + labs(fill = "Tipo:", x = "Mês", y = "Nº de alugueres (x1000)")

gp2

gp12 <-
  ggplot(gr_by_tudo2, aes(x = mnth, y = count / 1000, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = comma)

gp12


#casuals/regist por dia da semana
gr_by_weekd <-
  select(bikeSharing, weekday, casual, registered) %>% group_by(weekday) %>%
  summarise(Casuals = sum(casual),
            Registered = sum(registered))

#converter casual/registered em colunas tipo/contagem
gr_by_weekd2 <- gather(gr_by_weekd, type, count, Casuals:Registered)


#gráfico weekday vs tipo/contagem
#casuals alugam mais ao fim de semana, registered alugam mais durante a semana útil (talvez ate como meio de transporte para o trabalho)
gp3 <- ggplot(gr_by_weekd2, aes(weekday, count / 1000, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) + 
  labs(fill = "Tipo:", x = "Dia da semana", y = "Nº de alugueres (x1000)") +
  ggtitle("Número de alugueres casuais/registados em cada dia da semana")

gp3

# group by workingday vs. casual/registered
gr_by_workd <-
  select(bikeSharing, workingday, casual, registered) %>% group_by(workingday) %>%
  summarise(casual = sum(casual), registered = sum(registered))

# calculate mean of rentals by workingday type
workingday_count <- count(bikeSharing, bikeSharing$workingday)
gr_by_workd$n <- workingday_count$n
gr_by_workd$mean_casual <- gr_by_workd$casual / gr_by_workd$n
gr_by_workd$mean_reg <- gr_by_workd$registered / gr_by_workd$n

# gather columns by type/cont
gr_by_workd2 <- gather(gr_by_workd, type, cont, mean_casual:mean_reg)

#plot workingday by casual/registerd count
gp4 <- ggplot(gr_by_workd2, aes(type, cont, fill = workingday)) +
  geom_bar(stat = "identity", position = "dodge")

gp4


# group by holiday vs. casual/registered
gr_by_holyd <-
  select(bikeSharing, holiday, casual, registered) %>% group_by(holiday) %>%
  summarise(casual = sum(casual), registered = sum(registered))

# calculate mean of rentals by workingday type
holiday_count <- count(bikeSharing, bikeSharing$holiday)
gr_by_holyd$n <- holiday_count$n
gr_by_holyd$mean_casual <- gr_by_holyd$casual / gr_by_holyd$n
gr_by_holyd$mean_reg <- gr_by_holyd$registered / gr_by_holyd$n

# gather columns by type/cont
gr_by_holyd2 <- gather(gr_by_holyd, type, cont, mean_casual:mean_reg)

#plot workingday by casual/registerd count
gp9 <- ggplot(gr_by_holyd2, aes(type, cont, fill = holiday)) +
  geom_bar(stat = "identity", position = "dodge")

gp9

#so para ter um grafico de pontos
#temperatura vs. humidade por estação

gr_by_seas <-
  select(bikeSharing,
         real_temp,
         real_hum,
         real_windspeed,
         season,
         casual,
         registered) %>% group_by(season)

gp5 <-
  ggplot(gr_by_seas, aes(real_temp, real_hum, color = season)) + geom_point()

gp5

#season vs weather situation vs count casual/registered
#nota-se que para ambos os tipos, os alugueres são feitos mais de acordo com o clima do que com as estações

gr_by_sit <- select(bikeSharing, season, weathersit, casual, registered,seasonday) %>% group_by(season)
gr_by_sit2 <- gather(gr_by_sit, tipo, cont, casual:registered)


gp6 <-
  ggplot(gr_by_sit2, aes(x = seasonday, y = cont, fill = tipo)) +
  geom_bar(stat = "identity") + facet_grid(weathersit ~ season) +
  ggtitle('Distribution of casual/registered as a function of weather situation and Season')

gp6

#Relationship between the frequency of casual and registered clients
gp7 <-
  ggplot(bikeSharing, aes(x = casual, y = registered)) +
  geom_point() + geom_smooth() +
  ggtitle('Relationship between the frequency of casual and registered clients')

gp7

gp10 <-
  ggplot(bikeSharing, aes(x = casual, y = registered, color = season)) +
  geom_point() + geom_smooth() +
  ggtitle('Relationship between the frequency of casual and registered clients')

gp10


#frequencia de casual e registered em funçao de windspeed
gr_by_wind <- select(bikeSharing, real_windspeed, casual, registered)
gr_by_wind2 <- gather(gr_by_wind, tipo, cont, casual:registered)
gp8 <- ggplot(gr_by_wind2, aes(real_windspeed, cont)) + geom_point()

gp8
