library(tidyverse)
library(lubridate)

clima = read_csv(
    "https://github.com/nazareno/eda-clima/raw/master/data/clima_cg_jp-semanal.csv",
    col_types = "cTdddd"
)

clima %>%
    mutate(mes = lubridate::month(semana), ano = lubridate::year(semana)) %>%
    write_csv(here::here("data/clima_cg_jp-semanal.csv"))

city_temp <- clima[c(1,3)]

city_temp %>% 
    ggplot(aes(x = cidade, y = tmedia)) + 
    geom_boxplot(coef = 1000, width = .4) + 
    coord_flip() + 
    labs(
        x = "Cidade",
        y = "Temperatura"
    )

city_temp_cg <- filter(clima[c(1:3)], cidade == "Campina Grande")
city_temp_jp <- filter(clima[c(1:3)], cidade == "João Pessoa")

city_temp_cg$semana<-factor(as.factor(months(city_temp_cg$semana)), levels = month.name)
city_temp_jp$semana<-factor(as.factor(months(city_temp_jp$semana)), levels = month.name)

colnames(city_temp_cg)[2] <- "month"
colnames(city_temp_jp)[2] <- "month"

library(dplyr)
temp_month_cg <- group_by(city_temp_cg, month)%>%summarise(media=sum(tmedia)/length(tmedia))

temp_month_cg %>% 
    ggplot(aes(x = month, y=media)) + 
    geom_col(width = .3, fill = "blue") + 
    coord_flip() + 
    labs(
        x = "month",
        y = "Temperature CG"
    )

temp_month_jp <- group_by(city_temp_jp, month)%>%summarise(media=sum(tmedia)/length(tmedia))

temp_month_jp %>% 
    ggplot(aes(x = month, y=media)) + 
    geom_col(width = .3, fill = "dark green") + 
    coord_flip() + 
    labs(
        x = "month",
        y = "Temperature JP"
    )






