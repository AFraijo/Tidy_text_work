library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

## Section 1.3
## Create Books
original_books <- austen_books() %>% group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% 
  ungroup()

original_books

## Place into tidy format
tidy_books <- original_books %>% unnest_tokens(word, text)

tidy_books

## Remove Stop Words
data("stop_words")
tidy_books <- tidy_books %>% anti_join(stop_words)

## Quick count of words
tidy_books %>% count(word, sort = TRUE)

## Plot Count
library(ggplot2)

tidy_books %>% count(word, sort = TRUE) %>% filter(n > 599) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) + 
  coord_flip()
#### Section 1.5

## Download HGwells corpus
library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>% unnest_tokens(word, text) %>% anti_join(stop_words)
tidy_hgwells %>% count(word, sort = TRUE)

##Bronte works
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)
tidy_bronte %>% count(word, sort = TRUE)

##Bind all together and calculate frequencies
library(tidyr)

##Uses str_extract because gutenbergr has characters to indicate emphasis, e.g. "any" and "_any_" 
##Should be the same
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

## And plot
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

##Now a correlation test
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)




