library(tidyverse)
library(ggplot2)

# read rds
df1 <- readRDS("dt2000.rds")
typeof(df1)

head(df1)
as_tibble(df1)
df1 <- df1 %>%
    group_by(HHID) %>%
    mutate(ID = cur_group_id())
df1 <- ungroup(df1)
# print all the variables when call head
options(dplyr.width = Inf)

head(df1)
tail(df1)

df1
count(df1, ID)

# separate married and unmarried
unmarried <- filter(df1, marriage != "有配偶或同居")
married <- filter(df1, marriage == "有配偶或同居")
unmarried
married
count(df1, marriage)

temp <- df1 %>%
    filter(marriage == "有配偶或同居") %>%
    filter(relation %in% c("配偶(含同居人)", "戶長"))
temp
trouble <- temp %>%
    count(ID)
trouble
count(trouble, n)
clean_family <- filter(trouble, n == 2)
clean_family <- filter(temp, ID %in% clean_family$ID)
clean_family
women_id <- clean_family %>%
    filter(sex == 2)
clean_family <- filter(clean_family, ID %in% women_id$ID)
count(clean_family, sex)
man_id <- clean_family %>%
    filter(sex == 1)
clean_family <- filter(clean_family, ID %in% man_id$ID)
count(clean_family, sex)
clean_family
count(clean_family, ID, sex) %>%
    filter(n != 1)

# now clean_family only comtains 1-1 couple
# we can combine man and woman data
filter(clean_family, relation == "戶長") %>%
    count(sex)

long_to_couple <- function(df){
    df <- df[, c("ID", "sex", "age", "relation", "edu", "work")]
    man <- filter(df, sex == 1)
    woman <- filter(df, sex == 2)
    # rename column as column_man and column_woman
    man <- man %>%
        rename("man_sex" = "sex",
                "man_age" = "age",
                "man_relation" = "relation",
                "man_edu" = "edu",
                "man_work" = "work")
    woman <- woman %>%
        rename("woman_sex" = "sex",
                "woman_age" = "age",
                "woman_relation" = "relation",
                "woman_edu" = "edu",
                "woman_work" = "work")
    # merge two df by ID
    couple <- merge(man, woman, "ID")
    return(as_tibble(couple))
}

couple <- long_to_couple(clean_family)

parents <- readRDS("parents.rds")
count(df2, ID, sex) %>%
    filter(n != 1)
parents <- long_to_couple(parents)

grangparents <- readRDS("grandparents.rds")
count(grangparents, ID, sex) %>%
    filter(n != 1)
grangparents <- long_to_couple(grangparents)
count(grangparents, man_edu)

children <- readRDS("children.rds")
count(children, ID, sex) %>%
    filter(n != 1)
count(children, ID, relation) %>%
    filter(n != 1)
children <- long_to_couple(children)
count(children, man_edu)

couple <- rbind(couple, parents, grangparents, children)
couple <- rbind(couple, children)

install.packages("plotly")
library(plotly)
couple_points <- count(couple, man_age, woman_age)
# plot a surface with man_age as x, woman_age as y, and n as z
n <- xtabs(n ~ woman_age + man_age, data = couple_points)
# check maximum and minimum of woman_age and man_age
fig <- plot_ly(x = dimnames(n)$man_age, y = dimnames(n)$woman_age,z = ~n) %>% add_surface(
    contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
      )
    )
  )
# add labels
fig <- fig %>% layout(title = "number of couples by age",
                    scene = list(
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "n")))
fig
# cheching man = 46, woman = 44
filter(couple_points, man_age == 46, woman_age == 44)
filter(couple_points, man_age == 44, woman_age == 42)

fig_c <- plot_ly(x = dimnames(n)$man_age, y = dimnames(n)$woman_age,z = ~n, type = "contour")
fig_c

# plot a surface with man_age as x, woman_age as y, and n as z
nlg <- xtabs(n ~ woman_age + man_age, data = couple_points)
# change all 0 to 1
nlg[nlg == 0] <- 1
figln <- plot_ly(z = ~log(nlg)) %>% add_surface(
    contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
      )
    )
  )
# add labels
figln <- figln %>% layout(title = "number of couples by age",
                    scene = list(
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "n")))
figln
# draw a scatter plot, use smaller point size
fig <- plot_ly(couple_points, x = ~man_age, y = ~woman_age, z = ~log(n), type = "scatter3d", mode = "markers", marker = list(size = 1))
fig
# output couple to csv
write.csv(couple, "couple.csv")
# output couple to rds
saveRDS(couple, "couple.rds")

# now we move on to clean unmarried
unmarried
count(unmarried, marriage)
# drop 0
unmarried <- filter(unmarried, marriage != "0")
count(unmarried, marriage)
unmarried_man <- filter(unmarried, sex == 1)[, c("ID", "sex", "age", "relation", "marriage", "edu", "work")]
unmarried_woman <- filter(unmarried, sex == 2)[, c("ID", "sex", "age", "relation", "marriage", "edu", "work")]
options(scipen=10)
# draw histogram of age
g1 <- ggplot(unmarried_man, aes(x=age)) + geom_histogram(binwidth=1)
# save plot
ggsave("g1.png", plot = g1, width = 5, height = 5, dpi = 500)
# change sex in unmarried to factor 1 as M and 2 as F
unmarried$sex <- factor(unmarried$sex, levels = c(2, 1), labels = c("F", "M"))
# group by sex
g2 <- ggplot(count(unmarried, age, sex), aes(x=age, y=n)) +
    geom_line(aes(group=sex, color=sex)) +
    labs(title="Available Man/Woman (2000)",
        x ="Age", y = "Population")
ggsave("g2.png", plot = g2, width = 5, height = 5, dpi = 500)


pi_points <- couple_points
# take log of n
pi_points$n <- log(pi_points$n)
# calculate unmarried
unmarried_man_count <- count(unmarried_man, age)
unmarried_man_count$n <- log(unmarried_man_count$n)
unmarried_woman_count <- count(unmarried_woman, age)
unmarried_woman_count$n <- log(unmarried_woman_count$n)
# left join to pi_points
pi_points <- left_join(pi_points, unmarried_man_count, by = c("man_age" = "age"))
pi_points <- left_join(pi_points, unmarried_woman_count, by = c("woman_age" = "age"))
pi_points <- rename(pi_points, "ln(mu_ij)" = "n.x", "ln(mu_i0)" = "n.y", "ln(mu_0j)" = "n")
pi_points$pi <- pi_points$`ln(mu_ij)` - (pi_points$`ln(mu_i0)` + pi_points$`ln(mu_0j)`) / 2
pi_points
# draw the pi surface
pi <- xtabs(pi ~ woman_age + man_age, data = pi_points)
# change all 0 to -13
pi[pi == 0] <- -13
# check maximum and minimum of woman_age and man_age
figpi <- plot_ly(x = dimnames(pi)$man_age, y = dimnames(pi)$woman_age, z = ~pi) %>% add_surface(
    contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
      )
    )
  )
# add labels
figpi <- figpi %>% layout(title = "pi value by age",
                    scene = list(
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "pi")))
figpi

figpi_c <- plot_ly(x = dimnames(pi)$man_age, y = dimnames(pi)$woman_age, z = ~pi, type = "contour")
figpi_c <- figpi_c %>% layout(title = "pi value by age",
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "pi"))
figpi_c

pi_points <- couple_points
# take log of n
pi_points$n <- log(pi_points$n)
# calculate unmarried
unmarried_man_count <- count(unmarried_man, age)
unmarried_man_count$n <- log(unmarried_man_count$n * .753)
unmarried_woman_count <- count(unmarried_woman, age)
unmarried_woman_count$n <- log(unmarried_woman_count$n * .753)
# left join to pi_points
pi_points <- left_join(pi_points, unmarried_man_count, by = c("man_age" = "age"))
pi_points <- left_join(pi_points, unmarried_woman_count, by = c("woman_age" = "age"))
pi_points <- rename(pi_points, "ln(mu_ij)" = "n.x", "ln(mu_i0)" = "n.y", "ln(mu_0j)" = "n")
pi_points$pi <- pi_points$`ln(mu_ij)` - (pi_points$`ln(mu_i0)` + pi_points$`ln(mu_0j)`) / 2
pi_points
# draw the pi surface
pi <- xtabs(pi ~ woman_age + man_age, data = pi_points)
# change all 0 to -13
pi[pi == 0] <- -13
# check maximum and minimum of woman_age and man_age
figpi <- plot_ly(x = dimnames(pi)$man_age, y = dimnames(pi)$woman_age, z = ~pi) %>% add_surface(
    contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
      )
    )
  )
# add labels
figpi <- figpi %>% layout(title = "pi value by age",
                    scene = list(
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "pi")))
figpi

figpi_c <- plot_ly(x = dimnames(pi)$man_age, y = dimnames(pi)$woman_age, z = ~pi, type = "contour")
figpi_c <- figpi_c %>% layout(title = "pi value by age",
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "pi"))
figpi_c

# calculate n_ij and N_ij
pi_points
pi_points$n_ij <- pi_points$`ln(mu_ij)` - pi_points$`ln(mu_i0)`
pi_points$N_ij <- pi_points$`ln(mu_ij)` - pi_points$`ln(mu_0j)`
pi_points
m20 <- filter(pi_points, man_age == 20)
m20$group <- "20 yo man"
m20 <- m20 %>%
    rename("spouse_age" = "woman_age",
            "value" = "n_ij")
m20 <- m20[, c("group", "spouse_age", "value")]
m40 <- filter(pi_points, man_age == 40)
m40$group <- "40 yo man"
m40 <- m40 %>%
    rename("spouse_age" = "woman_age",
            "value" = "n_ij")
m40 <- m40[, c("group", "spouse_age", "value")]
w20 <- filter(pi_points, woman_age == 20)
w20$group <- "20 yo woman"
w20 <- w20 %>%
    rename("spouse_age" = "man_age",
            "value" = "N_ij")
w20 <- w20[, c("group", "spouse_age", "value")]
w40 <- filter(pi_points, woman_age == 40)
w40$group <- "40 yo woman"
w40 <- w40 %>%
    rename("spouse_age" = "man_age",
            "value" = "N_ij")
w40 <- w40[, c("group", "spouse_age", "value")]
net_gain <- rbind(m20, m40, w20, w40)
net_gain
g3 <- ggplot(net_gain, aes(x=spouse_age, y=value)) +
    geom_line(aes(group=group, color=group)) +
    labs(title="Systematic net gain (2000)",
        x ="Age of spouse", y = "Net gain")
ggsave("g3.png", plot = g3, width = 5, height = 5, dpi = 500)


g4 <- ggplot(filter(pi_points, woman_age == 40), aes(x=man_age, y=`ln(mu_ij)`)) +
    geom_line()
ggsave("g4.png", plot = g4, width = 5, height = 5, dpi = 500)

m60 <- filter(pi_points, man_age == 60)
m60$group <- "60 yo man"
m60 <- m60 %>%
    rename("spouse_age" = "woman_age",
            "value" = "n_ij")
m60 <- m60[, c("group", "spouse_age", "value")]
m70 <- filter(pi_points, man_age == 70)
m70$group <- "70 yo man"
m70 <- m70 %>%
    rename("spouse_age" = "woman_age",
            "value" = "n_ij")
m70 <- m70[, c("group", "spouse_age", "value")]
w60 <- filter(pi_points, woman_age == 60)
w60$group <- "60 yo woman"
w60 <- w60 %>%
    rename("spouse_age" = "man_age",
            "value" = "N_ij")
w60 <- w60[, c("group", "spouse_age", "value")]
w70 <- filter(pi_points, woman_age == 70)
w70$group <- "70 yo woman"
w70 <- w70 %>%
    rename("spouse_age" = "man_age",
            "value" = "N_ij")
w70 <- w70[, c("group", "spouse_age", "value")]
net_gain_old <- rbind(m60, m70, w60, w70)
g5 <- ggplot(filter(net_gain_old, group %in% c("60 yo man", "70 yo man")), aes(x=spouse_age, y=value)) +
    geom_line(aes(group=group, color=group)) +
    labs(title="Systematic net gain (2000)",
        x ="Age of spouse", y = "Net gain")
ggsave("g5.png", plot = g5, width = 5, height = 5, dpi = 500)

couple$man_edu <- factor(couple$man_edu, levels = count(couple, man_edu)$man_edu)
couple$woman_edu <- factor(couple$woman_edu, levels = count(couple, man_edu)$man_edu)
couple_edu <- count(couple, man_edu, woman_edu)
n_edu <- xtabs(n ~ woman_edu + man_edu, data = couple_edu)
fig <- plot_ly(x = dimnames(n_edu)$man_edu, y = dimnames(n_edu)$woman_edu,z = ~n_edu) %>% add_surface(
    contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
      )
    )
  )
# add labels
fig <- fig %>% layout(title = "number of couples by age",
                    scene = list(
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "n")))
fig
# drop 00
unmarried_man <- filter(unmarried_man, edu!="00")
unmarried_woman <- filter(unmarried_woman, edu!="00")
unmarried_man$edu <- factor(unmarried_man$edu, levels = count(unmarried_man, edu)$edu)
unmarried_woman$edu <- factor(unmarried_woman$edu, levels = count(unmarried_man, edu)$edu)
couple_edu$n <- log(couple_edu$n)
# calculate unmarried
unmarried_man_count <- count(unmarried_man, edu)
unmarried_man_count$n <- log(unmarried_man_count$n * .753)
unmarried_woman_count <- count(unmarried_woman, edu)
unmarried_woman_count$n <- log(unmarried_woman_count$n * .753)
couple_edu <- left_join(couple_edu, unmarried_man_count, by = c("man_edu" = "edu"))
couple_edu <- left_join(couple_edu, unmarried_woman_count, by = c("woman_edu" = "edu"))
couple_edu <- rename(couple_edu, "ln(mu_ij)" = "n.x", "ln(mu_i0)" = "n.y", "ln(mu_0j)" = "n")
couple_edu$pi <- couple_edu$`ln(mu_ij)` - (couple_edu$`ln(mu_i0)` + couple_edu$`ln(mu_0j)`) / 2
couple_edu
pi <- xtabs(pi ~ woman_edu + man_edu, data = couple_edu)
# change all 0 to -13
pi[pi == 0] <- -13
# check maximum and minimum of woman_age and man_age
figpi <- plot_ly(x = dimnames(pi)$man_edu, y = dimnames(pi)$woman_edu, z = ~pi) %>% add_surface(
    contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
      )
    )
  )
# add labels
figpi <- figpi %>% layout(title = "pi value by age",
                    scene = list(
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "pi")))
figpi

figpi_c <- plot_ly(x = dimnames(pi)$man_age, y = dimnames(pi)$woman_age, z = ~pi, type = "contour")
figpi_c <- figpi_c %>% layout(title = "pi value by age",
                    xaxis = list(title = "x: man age"), 
                    yaxis = list(title = "y: woman age"),
                    zaxis = list(title = "pi"))
figpi_c


man <- filter(df1, sex == 1, marriage == "有配偶或同居")
man <- filter(man, edu != "00")
#if edu %in% c("不識字", "國小", "學齡前兒童", "自修", "國(初)中") man$good_edu <- 0
man$good_edu <- 0
man$good_edu[man$edu %in% c("高中", "高職", "專科", "大學", "碩士", "博士")] <- 1

good <- count(man, age, good_edu) %>% filter(good_edu == 1)
good <- rename(good, "good_edu_n" = "n")
good <- left_join(count(man, age), good, by = "age")
# drop good_edu and replace NA with 0
good <- good[, c("age", "n", "good_edu_n")]
good[is.na(good)] <- 0
good$p <- good$good_edu_n / good$n
good
# plot with age from 20 to 100
g6 <- ggplot(good, aes(x=age, y=p)) +
    geom_line()
# adjust x axis
g6 <- g6 + scale_x_continuous(breaks = seq(20, 100, 10), limits = c(20, 100))
ggsave("g6.png", plot = g6, width = 5, height = 5, dpi = 500)
