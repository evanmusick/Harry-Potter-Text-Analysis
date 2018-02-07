A Novice Analysis of the Words in Harry Potter
================
Evan Musick
January 30, 2018

Reading in the Text
-------------------

I found someone who has kindly provided access to the entire text of each Harry Potter book. The data can be accessed [here](https://github.com/bradleyboehmke/harrypotter). Each book is a character vector - one for each chapter.

``` r
library(harrypotter)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggthemes)
library(knitr)

books <- list("Sorcerer's Stone" = philosophers_stone, "Chamber of Secrets" = chamber_of_secrets, 
              "Prisoner of Azkaban" = prisoner_of_azkaban, "Goblet of Fire" = goblet_of_fire, 
              "Order of the Phoenix" = order_of_the_phoenix, "Half-Blood Prince" = half_blood_prince,
              "Deathly Hallows" = deathly_hallows)

process_text <- function(book_title, books){
  book_text_raw <- tibble(books[[book_title]]) %>%
    select(text = 1) %>%
    mutate(chapter = row_number(),
           book = book_title) %>%
  unnest_tokens(word, text)
}

all_books_raw <- bind_rows(lapply(names(books), process_text, books)) %>%
  mutate(book = factor(book, levels = unique(book)))
```

This gives us one word per row with identifiers for the book and chapter.

    ## # A tibble: 1,089,386 x 3
    ##    chapter book             word   
    ##      <int> <fct>            <chr>  
    ##  1       1 Sorcerer's Stone the    
    ##  2       1 Sorcerer's Stone boy    
    ##  3       1 Sorcerer's Stone who    
    ##  4       1 Sorcerer's Stone lived  
    ##  5       1 Sorcerer's Stone mr     
    ##  6       1 Sorcerer's Stone and    
    ##  7       1 Sorcerer's Stone mrs    
    ##  8       1 Sorcerer's Stone dursley
    ##  9       1 Sorcerer's Stone of     
    ## 10       1 Sorcerer's Stone number 
    ## # ... with 1,089,376 more rows

Character Mentions
------------------

I've identified the 3 main characters as our characters of interest. Let's find out which book has the most (by percentage of words) mentions of each character.

``` r
characters_of_interest <- c("harry" = "Harry", "ron" = "Ron", "hermione" = "Hermione")

all_books_raw %>%
  count(book, word) %>%
  mutate(freq_by_book = n/sum(n)) %>%
  filter(word %in% names(characters_of_interest)) %>%
  ggplot(aes(x = book, y = freq_by_book, fill = book)) + 
  geom_bar(stat = "identity", alpha = .5) +
  facet_grid(.~word,  scales = "free", labeller = as_labeller(characters_of_interest)) +
  scale_x_discrete(limits = rev(levels(all_books_raw$book))) + 
  scale_fill_brewer(guide = FALSE, palette = "Set1") +
  coord_flip() +
  labs(title = "Proportion of Character Mentions", x = "", y = "Proportion of All Words") +
  theme_few() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11))
```

![](Harry_Potter_Text_Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

Now let's find out the popularity of each character within each book. For example, we can look at how many times Ron was mentioned compared to the total mentions of all three characters.

``` r
all_books_raw %>%
  filter(word %in% names(characters_of_interest)) %>%
  count(book, word) %>%
  group_by(book) %>%
  mutate(char_freq_by_book = n/sum(n)) %>%
  ggplot(aes(x = book, y = char_freq_by_book, fill = word)) + 
  geom_bar(stat = "identity", alpha = .7, position = "dodge") +
  scale_fill_brewer(palette = "Dark2", labels = c("Harry", "Hermione", "Ron")) +
  scale_x_discrete(limits = rev(levels(all_books_raw$book))) + 
  coord_flip() +
  labs(title = "Character Mentions Relative to Each Other", x = "", 
       y = "Proportion", fill = "") +
  theme_few() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11))
```

![](Harry_Potter_Text_Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)
