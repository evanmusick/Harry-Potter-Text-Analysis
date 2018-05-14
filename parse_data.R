library(gutenbergr)
library(harrypotter)
library(tidyverse)
library(tidytext)
library(stringr)

deathly_hallows_raw <- tibble(deathly_hallows) %>% 
  mutate(Chapter = factor(row_number())) %>%
  rename(Text = deathly_hallows)

deathly_hallows_tidy <- deathly_hallows_raw %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)

deathly_hallows_tidy %>% 
  count(word, sort = TRUE)

deathly_hallows_tf_idf <- deathly_hallows_tidy %>%
  count(Chapter, word, sort = TRUE) %>%
  bind_tf_idf(word, Chapter, n) %>% 
  group_by(Chapter) %>%
  top_n(10, tf_idf) %>%
  ungroup()

# evan's change

ggplot(deathly_hallows_tf_idf, aes(x = reorder(word, tf_idf), y = tf_idf, fill = Chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Chapter, scales = "free") +
  coord_flip()
