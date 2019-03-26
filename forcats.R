# Factors using the forcats package

# install.packages("forcats")
library(tidyverse)
library(forcats)

#### Create factors
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")

sort(x1) #doesn't work

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

y1 <- factor(x1, levels = month_levels)
sort(y1) #now it is okey

y2 <- factor(x2, levels = month_levels)
sort(y2) #"Jam" level doesn't exist

y2 <- parse_factor(x2, levels = month_levels) 
#using function from the reader package we've got the error message

factor(x1) #without levels we have alfabetical order

f1 <- factor(x1, levels = unique(x1))
f2 <- x1 %>% factor() %>% fct_inorder()
# the same result

levels(f2)

#### General Social Survey Research

head(gss_cat)

gss_cat %>%
  count(race)

ggplot(gss_cat, aes(race)) + 
  geom_bar()

ggplot(gss_cat, aes(race)) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE)

# Exercises

# Ex 1
ggplot(gss_cat, aes(rincome)) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE)
# Suggestion: Changing the levels names

# Ex 2
gss_cat %>%
  count(relig) %>%
  arrange(desc(n))
# Answer: Protestant

gss_cat %>%
  count(partyid) %>%
  arrange(desc(n))
# Answer: Independent

# Ex 3
gss_cat %>% 
  distinct(relig, denom)

ggplot(gss_cat, mapping = aes(x = relig, y = denom)) +
  geom_point()

#### Modification of factors order 
relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig, aes(tvhours, relig)) +
  geom_point()

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

relig %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome <- gss_cat %>%
  group_by(rincome) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(rincome, aes(age, fct_reorder(rincome, age))) +
  geom_point()

# fct_relevel - changing chosen level place for first
ggplot(rincome,aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = n/sum(n))

ggplot(by_age, aes(age, prop, color = marital)) +
  geom_line(na.rm = TRUE)

# fct_reorder2 - ??
ggplot(by_age, aes(age, prop, color = fct_reorder2(marital, age, prop))) + 
  geom_line() +
  labs(color = "marital")

# fct_infreq - changing factors levels depends on frequency
# fct_rev - reverse order
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

# Ex 1
# Ex 2
# Ex 3

#### Modification of factors levels
