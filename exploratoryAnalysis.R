source("importationAndCleanup.R")
library(ggplot2)
library(dplyr)
library(grid)
require(scales)

gr_by_year <- bikeSharing %>% group_by(yr) %>%
  summarise(total_casual = sum(casual),
  total_registered = sum(registered),
  total_cnt = sum(cnt))


# something <- select(bikeSharing, mnth, day, cnt) %>% group_by(yr, mnth) %>%
#   summarise(total_cnt = sum(cnt))
# percentages <- total_values2[2, 2:4] / total_values2[1, 2:4]

########## para cada ano, para cada mês, e temperatura média
gr_by_month <- select(bikeSharing, yr, mnth, cnt) %>% group_by(yr, mnth) %>%
  summarise(total_cnt = sum(cnt))

mean_temp <- bikeSharing %>% group_by(mnth) %>% 
  summarise(mean_temperature = mean(temperature))

# mean_temp_plot <- ggplot(mean_temp, aes(x=mnth, y=mean_temperature)) + geom_point() + geom_line()

gp1 <- gr_by_month %>% ggplot() + geom_histogram(aes(x=mnth, y=total_cnt * 35 / 250000, fill=yr), stat = "identity", position = "dodge") +
  geom_line(data = mean_temp, aes(x=mnth, y=mean_temperature, group=1), show.legend = FALSE) +
  scale_y_continuous(name = expression("Temperature ("~degree~"C)"), sec.axis = sec_axis(~ . * 250000 / 35, name = "some shit"), limits=c(0, 35))

# by_month_plot + ggplot(mean_temp, aes(x=mnth, y=mean_temperature)) + geom_point() + geom_line()



####### casuals vs registered

gr_by_casuals <- select(bikeSharing, yr, mnth, casual) %>% group_by(mnth) %>% 
  summarise(total_casuals = sum(casual))

plot_casuals <- ggplot(gr_by_casuals) + geom_bar(aes(x=mnth, y = total_casuals), stat = "identity")


gr_by_registered <- select(bikeSharing, yr, mnth, registered) %>% group_by(mnth) %>% 
  summarise(total_registered = sum(registered))

plot_registered <- ggplot(gr_by_registered) + geom_bar(aes(x=mnth, y = total_registered), stat = "identity")


grid.newpage()
grid.draw(rbind(ggplotGrob(as.data.frame(gr_by_casuals)), ggplotGrob(as.data.frame(gr_by_registered)), size = "last"))


gp2 <- gr_by_types %>% ggplot() + geom_histogram(aes(x=mnth, y = total_casuals), stat = "identity", position = "dodge") +
  geom_histogram(aes(x=mnth, y = total_registered, fill=registered), stat = "identity")





gr_by_tudo <- select(bikeSharing, yr, mnth, casual, registered) %>% group_by(mnth) %>% 
  summarise(total_casuals = sum(casual), total_registered = sum(registered))

meses = c(rep("jan", 2), rep("feb", 2), rep("mar", 2), rep("abr", 2), rep("maio", 2), rep("jun", 2), rep("jul", 2), rep("ago", 2), rep("set", 2), rep("out", 2), rep("nov", 2), rep("dez", 2))
condition = rep(c("casual", "registered"), 12)
value = c(gr_by_tudo$total_casuals, gr_by_tudo$total_registered)
data = data.frame(meses, condition, value)

ggplot(data, aes(fill=condition, y= value, x=meses)) +
  geom_bar(position = "dodge", stat = "identity")

lixo = abs(rnorm(12,0,15))
