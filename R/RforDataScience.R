library(ggplot2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = trans, shape = trans,
                           size = year))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(. ~ hwy, nrow = 1)



ggplot(data = mpg) + 
  facet_wrap(~cyl)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)


# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv)) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))


ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = dplyr::filter(mpg, class == "subcompact"), se = FALSE)



ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))


temp <- as.character(c(1:5))
temp <- data.frame("name" = c(1:5),temp, stringsAsFactors = FALSE)

ggplot(data = temp) + 
  geom_bar(mapping = aes(x = name, y = temp), stat = "identity")

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))


demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))



ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))


ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5)

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity") +
  theme_classic()


ggplot(data = diamonds,
       mapping = aes(x = cut, fill = clarity)) + 
  geom_bar( position = "fill")


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()


nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")


ggplot(nz, aes(long, lat, group = group)) +
  geom_line(fill = "white", colour = "black")


ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()


bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


library(dplyr)
library(nycflights13)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))



not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


install.packages("nycflights13")


library(nycflights13)
library(tidyverse)


temp <- filter(flights, month == 1, day == 1)


(dec25 <- filter(flights, month == 12, day == 25))


df <- tibble(x = c(1, NA, 3))

filter(df, x > 1)



temp <-arrange(flights, year, month, day)

df <- tibble(x = c(5, 2, NA))
arrange(df, x)

arrange(df, desc(x))



temp <- select(flights, year, month, day)

temp <- select(flights, year:day)

temp <- select(flights, -(year:day))


rename(flights, tail_num = tailnum)


flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)


mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)


mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)


transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)


summarise(flights, delay = mean(dep_delay, na.rm = TRUE))


by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")


ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)


delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))


not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))



delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )


ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


install.packages("Lahman")
# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'


temp1 <- batters %>% 
  arrange(desc(ba))

temp2 <- arrange(batters, desc(ba))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )


# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))


# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )


not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))


# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))


not_cancelled %>% 
  count(dest)

count(not_cancelled, dest)

# count(c("a", "b", "c"))


not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  count(tailnum)


# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))


# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))


library(nycflights13)
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))


daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights


flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)


popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests




popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)



library(tidyverse)


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))


temp <- diamonds %>% 
  count(cut)


ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)


diamonds %>% 
  count(cut_width(carat, 0.5))

# 
# diamonds %>%
#   cut_width(carat, 0.5) %>%
#   count()


smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)


ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)



ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)



ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))


unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual


diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))


diamonds3 <- diamonds %>%
  filter(y > 3 & y < 20)


diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))


ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()


ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)


nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)


ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()


ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))



diamonds %>% 
  count(color, cut)


diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n), color = "white")


ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))


ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)


ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))


install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price)) +
  theme_bw()


ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))


ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))


ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))


library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))


ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)



diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()



library(tidyverse)
vignette("tibble")
temp <- as_tibble(iris)

temp <- tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)


tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)

tb


tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)
#> #

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)


nycflights13::flights %>% 
  print(n = 10, width = Inf)

print(nycflights13::flights, n =15)



options(tibble.print_max = 15, 
        tibble.print_min = 3)


nycflights13::flights %>% 
  View()


df <- tibble(
  x = runif(5),
  y = rnorm(5)
)


df$x
#> [1] 0.434 0.395 0.548 0.762 0.254
df[["x"]]
#> [1] 0.434 0.395 0.548 0.762 0.254

# Extract by position
df[[1]]


df %>% .$x

class(as.data.frame(tb))


library(tidyverse)


read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3")


read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)


read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

read_csv("a,b,c\n1,2,.", na = ".")


str(parse_logical(c("TRUE", "FALSE", "NA")))

str(parse_date(c("2010-01-01", "1979-10-14")))


x <- parse_integer(c("123", "345", "abc", "123.45"))


parse_number("$100")
#> [1] 100
parse_number("20%")
#> [1] 20
parse_number("It cost $123.45")


# Used in America
parse_number("$123,456,789")

# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))
#> [1] 1.23e+08

# Used in Switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))


charToRaw("Hadley")


x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"


parse_character(x1, locale = locale(encoding = "Latin1"))

parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))

fruit <- c("apple", "banana")

parse_factor(c("apple", "banana", "bananana"), levels = fruit)

challenge <- read_csv(readr_example("challenge.csv"))

problems(challenge)


#fromList
listinput <- list(one = c(1, 2, 3, 5, 7, 8, 11, 12, 13), two = c(1, 2, 4, 5, 10), three = c(1, 5, 6, 7, 8, 9, 10, 12, 13))
#fromExpression
expressionInput <- c(one = 2, two = 1, three = 2, `one&two` = 1, `one&three` = 4, `two&three` = 1, `one&two&three` = 2)


library(UpSetR)
upset(fromList(listinput), order.by = "freq")
#下面绘制的图形等同于上图
upset(fromExpression(expressionInput), order.by = "freq")



#导入数据
movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), header = TRUE, sep = ";")
#先大致浏览一下该数据集,数据集太长，就只看前几列
knitr::kable(head(movies[,1:10]))

upset(movies, nsets = 6, number.angles = 30, 
      point.size = 2, line.size = 1, 
      mainbar.y.label = "Genre Intersections", 
      sets.x.label = "Movies Per Genre", 
      text.scale = c(1.3, 1.3, 1, 1, 1.5, 1))


upset(movies, 
      sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"),
      mb.ratio = c(0.55, 0.45),
      order.by = "freq",
      keep.order = TRUE)
upset(movies, nintersects = 60, group.by = "sets", cutoff = 6)


upset(movies, queries = list(list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T), 
                             list(query = intersects, params = list("Drama"), color = "red", active = F), 
                             list(query = intersects, params = list("Action", "Drama"), active = T)))


upset(movies, main.bar.color = "black", queries = list(list(query = intersects, 
                                                            params = list("Drama"), active = T)), attribute.plots = list(gridrows = 50, 
                                                                                                                         plots = list(list(plot = histogram, x = "ReleaseDate", queries = F), list(plot = histogram, 
                                                                                                                                                                                                   x = "AvgRating", queries = T)), ncols = 2))

upset(movies, main.bar.color = "black", queries = list(list(query = intersects, params = list("Drama"), color = "red", active = F),
                                                       list(query = intersects, params = list("Action", "Drama"), active = T),
                                                       list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T)), 
      attribute.plots = list(gridrows = 45, plots = list(list(plot = scatter_plot, x = "ReleaseDate", y = "AvgRating", queries = T),
                                                         list(plot = scatter_plot, x = "AvgRating", y = "Watches", queries = F)), ncols = 2), 
      query.legend = "bottom")




table1
table2
table3
table4a


# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)


# Compute cases per year
table1 %>% 
  dplyr::count(year, wt = cases)

dplyr::count(table1, year, wt = cases)


# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")


table4b

table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")



tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

left_join(tidy4a, tidy4b)


table2

table2 %>%
    spread(key = type, value = count)
