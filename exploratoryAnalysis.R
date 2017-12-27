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

mean_temp <- bikeSharing %>% group_by(mnth) %>% 
  summarise(mean_temperature = mean(real_temp))


gp1 <- gr_by_month %>% ggplot() + geom_histogram(aes(x = mnth, y = total_cnt * 35 / 220000, fill = yr), stat = "identity", position = "dodge") +
  geom_line(data = mean_temp, aes(x = mnth, y = mean_temperature, group = 1), show.legend = FALSE) +
  scale_y_continuous(name = expression("Temperatura ("~degree~"C)"),
                     sec.axis = sec_axis(~ . * 220000 / 35, name = "Nº de alugueres"),
                     limits = c(0, 35)
                     ) +
  labs(x = "Mês")

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
