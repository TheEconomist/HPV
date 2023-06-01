library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(stringr)

df = read_csv("source-data/2019_2021_all_vaccine_rates.csv")

df$change = as.numeric(df$change)

df %>% ggplot(aes(x = reorder(`vaccine name`, change), 
                  y = change)) + 
  geom_bar(stat = "identity") +
  xlab("Vaccine Name") +
  ylab("Percent change between 2019 and 2021") +
  coord_flip()

ggsave("plots/other_vaccines.png", width = 10, height = 5)


