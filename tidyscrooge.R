library(rprojroot)
library(hrbrthemes)
library(stringi)
library(tidytext)
library(tidyverse)

rt <- find_rstudio_root_file()

#' Read in the book text (NOTE: Book data from Project Gutenberg)
carol_txt <- stri_read_lines(file.path(rt, "data", "46-8.txt"))

#' We'll need this later to make prettier facet titles
data_frame(
  stave = 1:5,
  title = sprintf("Stave %s: %s", stave, carol_txt[stri_detect_fixed(carol_txt, "STAVE")] %>%
    stri_replace_first_regex("STAVE [[:alpha:]]{1,3}: ", "") %>%
    stri_trans_totitle())
) -> stave_titles

#' Break the text up into chapters, paragraphs, sentences, and words,
#' preserving the hierarchy so we can use it later.
data_frame(txt = carol_txt) %>%
  unnest_tokens(chapter, txt, token="regex", pattern="STAVE [[:alpha:]]{1,3}: [[:alpha:] [:punct:]]+") %>%
  mutate(stave = 1:n()) %>%
  unnest_tokens(paragraph, chapter, token = "paragraphs") %>%
  group_by(stave) %>%
  mutate(para = 1:n()) %>%
  ungroup() %>%
  unnest_tokens(sentence, paragraph, token="sentences") %>%
  group_by(stave, para) %>%
  mutate(sent = 1:n()) %>%
  ungroup() %>%
  unnest_tokens(word, sentence) -> carol_tokens

#' Retrieve sentiments and compute them.
#'
#' I left the `index` in vs just use `paragraph` since it'll make this easier to reuse
#' this block (which I'm not doing but thought I might).
inner_join(carol_tokens, get_sentiments("nrc"), "word") %>%
  count(stave, index = para, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  left_join(stave_titles, "stave") -> carol_with_sent

#' Make the plot
ggplot(carol_with_sent) +
  geom_segment(aes(index, sentiment, xend=index, yend=0, color=title), size=0.33) +
  scale_x_comma(limits=range(carol_with_sent$index)) +
  scale_y_comma() +
  scale_color_ipsum() +
  facet_wrap(~title, scales="free_x", ncol=5) +
  labs(x=NULL, y="Sentiment",
       title="Sentiment Analysis of A Christmas Carol",
       subtitle="By stave & ¶",
       caption="Humbug!") +
  theme_ipsum_rc(grid="Y", axis_text_size = 8, strip_text_face = "italic", strip_text_size = 10.5) +
  theme(legend.position="none")

