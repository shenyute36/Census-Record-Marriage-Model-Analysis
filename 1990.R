library(tidyverse)
library(ggplot2)

# read rds
df2 <- readRDS("dt1990.rds")
typeof(df2)
# drop col 4
df2 <- df2[,-4]
head(df2)
as_tibble(df2)
df2 <- df2 %>%
    group_by(HHID) %>%
    mutate(ID = cur_group_id())
df2 <- ungroup(df2)
# print all the variables when call head

options(dplyr.width = Inf)

head(df2)
print(filter(df2, ID == 1))
