setwd('C:/Users/U209-18/Desktop/R')

raw_Mermaid <- readLines("Mermaid.txt", encoding = "UTF-8")
head(raw_Mermaid)

library(stringr)
new = raw_Mermaid %>% str_replace_all("[^가-힣]", replacement = " ")
head(new)

new = new %>% str_squish()
head(new)

library(dplyr)
new = new %>% as_tibble()
new

library(tidytext)
word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")
word_space

word_space <- word_space %>% count(word, sort = T)
word_space

word_space = word_space %>% filter(str_count(word)>1)
word_space

top20 <- word_space %>% head(20)
top20

library(ggplot2)
ggplot(top20, aes(x=reorder(word, n), y= n)) +
  geom_col() + coord_flip()

ggplot(top20, aes(x=reorder(word, n), y= n)) +
  geom_col() + coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(title = "영화 인어공주 댓글 단어 빈도", x = NULL, y = NULL) +
  theme(title = element_text(size = 20))

install.packages("ggwordcloud")
library(ggwordcloud)
ggplot(word_space, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3,NA), range = c(3, 30))

ggplot(word_space, aes(label = word, size = n, col=n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3,NA), range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#003EA1") +
  theme_minimal()

library(KoNLP)
useNIADic()

raw = readLines("Mermaid.txt", encoding = "UTF-8")
Mermaid <- raw %>% str_replace_all("[^가-힣]", " ") %>%
  str_squish() %>% as_tibble()
Mermaid

Mermaid %>% unnest_tokens(input = value, output = word, token = extractNoun)

word_noun <- Mermaid %>% unnest_tokens(input = value, output = word, token = extractNoun)
word_noun

word_noun <- word_noun %>% count(word, sort = T) %>% filter(str_count(word) > 1)
word_noun

library(showtext)
font_add_google(name = "Gamja Flower", family = "gf")
showtext_auto()
ggplot(top20, aes(x=reorder(word, n), y=n)) + geom_col() + coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + labs(x=NULL) + theme(text = element_text(family="gf"))

library(stringr)
raw_Mermaid = readLines("Mermaid.txt", encoding = "UTF-8")
sentences_M <- raw_Mermaid %>% str_squish() %>% as_tibble %>% 
  unnest_tokens(input = value, output = sentence, token = "sentences")
sentences_M

sentences_M %>% filter(str_detect(sentence, "흑인"))

sentences_M %>% filter(str_detect(sentence, "원작"))

raw_Mone <- readLines("Mone.txt", encoding = "UTF-8")
Mone <- raw_Mone %>% as_tibble()
raw_Mtwo <- readLines("Mtwo.txt", encoding = "UTF-8")
Mtwo <- raw_Mtwo %>% as_tibble()

bind_M <- bind_rows(Mone, Mtwo)
head(bind_M)

library(stringr)
M_reply <- bind_M %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))
M_reply

M_reply <- M_reply %>% unnest_tokens(input = value, output = word, token = extractNoun)
M_reply

freq <- M_reply %>% count(word) %>% filter(str_count(word)>1)
freq

top10 <- freq %>% slice_max(n, n=10, with_ties = F)
top10

ggplot(top10, aes(x=reorder(word, n), y=n)) + geom_col() + coord_flip()

library(dplyr)
df_long <- freq %>% slice_max(n, n=10) %>% filter(word %in% c("흑인", "원작", "인어공주", "배우"))
df_long

library(tidyr)
df_wide <- df_long %>% pivot_wider(names_from = word, values_from = n)
df_wide

df_wide <- df_long %>% pivot_wider(names_from = word, values_from = n, values_fill = list(n=0))
df_wide

fre_wide <- freq %>% pivot_wider(names_from = word, values_from = n, values_fill = list(n=0))
fre_wide

library(readr)
raw_Mermaid <- read_csv("Mermaid.csv")
raw_Mermaid

Reply <- raw_Mermaid %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))

raw_m_comment <- read_csv("Mermaid.csv")
m_comment <- raw_m_comment %>% select(reply) %>% mutate(reply = str_replace_all(reply, "[^가-힣]", " "), reply = str_squish(reply), id=row_number())
comment_pos <- m_comment %>% unnest_tokens(input=reply, output=word, token=SimplePos22, drop=F)
comment_pos %>% select(word, reply)
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
comment_pos %>% select(word, reply)
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word, "/.*$"))
noun %>% select(word, reply)
noun %>% count(word, sort = T)
pvpa <- comment_pos %>% filter(str_detect(word, "/pa|/pv")) %>% mutate(word=str_replace(word, "/.*$", "다"))
pvpa %>% select(word, reply)
pvpa %>% count(word, sort = T)
comment <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)
comment %>% select(word, reply)
pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
pair
graph_comment <-pair %>% filter(n>=3) %>% as_tbl_graph()
graph_comment
ggraph(graph_comment) + geom_node_point() + geom_edge_link() + geom_node_text(aes(label=name))
font_add_google(name = "Nanum Gothic", family = "ng")
showtext.auto()

set.seed(1234)
ggraph(graph_comment, layout = "fr") + 
  geom_edge_link(color="gray50", alpha = 0.5) +
  geom_node_point(color = "lightcoral", size=5) +
  geom_node_text(aes(label = name), rapel = T, size = 5, family = "ng") +
  theme_graph()

word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50", alpha = 0.5) +
    geom_node_point(color = "lightcoral", size = 5) +
    geom_node_text(aes(label=name), repel = T, size = 5, family = "ng")+
    theme_graph()
}
set.seed(1234)

word_network(graph_comment)

comment <- comment %>% mutate(word = ifelse(str_detect(word, "공주") & !str_detect(word, "인어공주"), "인어공주", word), word = ifelse(word == "차별", "인종차별", word), word = ifelse(str_detect(word, "인어"), "인어공주", word))
pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
graph_comment <- pair %>% filter(n>=25) %>% as_tbl_graph()
set.seed(1234)
word_network(graph_comment)

setwd('C:/Users/U209-18/Desktop/R')
dic <- read_csv("knu_sentiment_lexicon.csv")
raw_M_R <- read_csv("Mermaid.csv")
M_comment <- raw_M_R %>% mutate(id=row_number(), reply=str_squish(replace_html(reply)))
glimpse(M_comment)
word_comment <- M_comment %>% unnest_tokens(input=reply, output=word, token="words", drop=F)
word_comment %>% select(word, reply)
dic <- read_csv("knu_sentiment_lexicon.csv")
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity=ifelse(is.na(polarity), 0, polarity))
word_comment %>% select(word, polarity)
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity==2, "pos", ifelse(polarity == -2, "neg", "neu")))
word_comment %>% count(sentiment)
top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
top10_sentiment

score_comment <- word_comment %>% summarise(score = sum(polarity)) 
score_comment %>% select(score) %>% arrange(-score)
score_comment %>% select(score) %>% arrange(score)
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >=1, "pos", ifelse(score <= -1, "neg", "neu")))
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio=n/sum(n)*100)

score_comment %>% filter(str_detect(reply, "소름")) %>% select(reply)
score_comment %>% filter(str_detect(reply, "미친")) %>% select(reply)
dic %>% filter(word %in% c("소름", "소름이", "미친"))
new_dic <- dic %>% mutate(polarity=ifelse(word %in% c("소름", "소름이", "미친"), 2, polarity))
new_dic %>% filter(word %in% c("소름", "소름이", "미친"))
new_word_comment <- word_comment %>% select(-polarity) %>% left_join(new_dic, by="word") %>% mutate(polarity=ifelse(is.na(polarity), 0, polarity))

new_score_comment <- new_word_comment %>% group_by(id, reply) %>% summarise(score=sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, reply) %>% arrange(-score)

raw_m_comment <- read_csv("Mermaid.csv")
m_comment <- raw_m_comment %>% select(reply) %>% mutate(reply = str_replace_all(reply, "[^가-힣]", " "), reply = str_squish(reply), id=row_number())
comment_pos <- m_comment %>% unnest_tokens(input=reply, output=word, token=SimplePos22, drop=F)
comment_pos %>% select(word, reply)
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
comment_pos %>% select(word, reply)
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word, "/.*$"))
noun %>% select(word, reply)
noun %>% count(word, sort = T)
pvpa <- comment_pos %>% filter(str_detect(word, "/pa|/pv")) %>% mutate(word=str_replace(word, "/.*$", "다"))
pvpa %>% select(word, reply)
pvpa %>% count(word, sort = T)
comment <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)
comment %>% select(word, reply)
pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
pair
graph_comment <-pair %>% filter(n>=3) %>% as_tbl_graph()
graph_comment
ggraph(graph_comment) + geom_node_point() + geom_edge_link() + geom_node_text(aes(label=name))
font_add_google(name = "Nanum Gothic", family = "ng")
showtext.auto()

set.seed(1234)
ggraph(graph_comment, layout = "fr") + 
  geom_edge_link(color="gray50", alpha = 0.5) +
  geom_node_point(color = "lightcoral", size=5) +
  geom_node_text(aes(label = name), rapel = T, size = 5, family = "ng") +
  theme_graph()


word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50", alpha = 0.5) +
    geom_node_point(color = "lightcoral", size = 5) +
    geom_node_text(aes(label=name), repel = T, size = 5, family = "ng")+
    theme_graph()
}
set.seed(1234)

word_network(graph_comment)

comment <- comment %>% mutate(word = ifelse(str_detect(word, "공주") & !str_detect(word, "인어공주"), "인어공주", word), word = ifelse(word == "차별", "인종차별", word), word = ifelse(str_detect(word, "인어"), "인어공주", word))
pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
graph_comment <- pair %>% filter(n>=3) %>% as_tbl_graph()
set.seed(1234)
word_network(graph_comment)

set.seed(1234)
graph_comment <- pair %>% filter(n>=3) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))
graph_comment

set.seed(1234)
ggraph(graph_comment, layout = "fr") +
  geom_edge_link(color = "gray50", alpha = 0.5) +
  geom_node_point(aes(size = centrality, color = group), show.legend = F) +
  scale_size(range = c(5,15))+
  geom_node_text(aes(label = name), repel = T, size = 5, family = "ng") +
  theme_graph()

graph_comment %>% filter(name == "흑인")
graph_comment %>% filter(group == 2) %>% arrange(-centrality)

graph_comment %>% arrange(-centrality)
graph_comment %>% filter(group == 2) %>% arrange(-centrality)

raw_m_comment <- read_csv("Mermaid.csv") %>%
  mutate(id = row_number())

m_comment <- raw_m_comment %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply)) %>%
  distinct(reply, .keep_all = T) %>%
  filter(str_count(reply, boundary("word")) >= 3)

comment <- m_comment %>%
  unnest_tokens(input = reply, output = word,
                token = extractNoun, drop = F) %>%
  filter(str_count(word) > 1) %>%
  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  select(id, word)
comment

count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)
count_word  

count_word %>% count(word, sort = T) %>% print(n = 200)

stopword <- c("갑분", "때문", "깝쳐서", "나발", "난리칠꺼아닌", "누가봐도", "니들",
              "다음", "니라", "다양한", "다양", "당신", "딸딸이", "만드",
              "모르겠", "마빡")

count_word <- count_word %>%
  filter(!word %in% stopword) 

count_word_doc <- count_word %>% count(id, word, sort = T)
count_word_doc

install.packages("tm")
library(tm)

dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)
dtm_comment

as.matrix(dtm_comment[1:7, 1:7])

install.packages("topicmodels")
library(topicmodels)

lda_model <- LDA(dtm_comment, k =8, method = "Gibbs",
                 control = list(seed = 1234))
lda_model

term_topic <- tidy(lda_model, matrix = "beta")
term_topic

term_topic %>% count(topic)

term_topic %>% filter(topic == 1) %>%
  summarise(sum_beta = sum(beta))

term_topic %>% filter(term == "개연성")

term_topic %>% filter(topic == 6) %>%
  arrange(-beta)

terms(lda_model, 10) %>% data.frame()

library(tidytext)
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic

lda_model@gamma

doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n=1)
doc_class

doc_class$document <- as.integer(doc_class$document)
doc_class %>% arrange(document)

raw_m_comment <- read_csv("Mermaid.csv") %>% mutate(id=row_number())
m_comment_topic <- raw_m_comment %>%
  left_join(doc_class, by=c("id" = "document"))
m_comment_topic %>% select(id, topic)

m_comment_topic %>% count(topic)
m_comment_topic <- m_comment_topic %>% na.omit()

top_terms <- term_topic %>% group_by(topic) %>% 
  slice_max(beta, n=6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))
top_terms

count_topic <- m_comment_topic %>% count(topic)
count_topic

count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("TOPIC", topic))
count_topic_word

library(ggplot2)
ggplot(count_topic_word, aes(x=reorder(topic_name, n), y=n, fill=topic_name))+
  geom_col(show.legend = F)+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.2)+
  geom_text(aes(label=term),
            hjust=1.04,
            col="white",
            fontface = "bold") +
  scale_y_continuous(limits = c(0, 20)) +
  labs(x=NULL)

comment_topic <- m_comment_topic %>%
  mutate(reply=str_squish(replace_html(reply))) %>%
  arrange(-gamma)
comment_topic

comment_topic %>% filter(topic == 1 & str_detect(reply, "흑인")) %>%
  head(50) %>% pull(reply)

name_topic <- tibble(topic = 1:8,
                     name = c("1. 인종차별인가 역차별인가",
                              "2. 역으로 당한 인종차별",
                              "3. 개연성이 없어서 망한 영화",
                              "4. 인종보단 개연성이 문제",
                              "5. 왜 인종만 논란인가, 개연성은",
                              "6. 백인이든 흑인이든",
                              "7. 원작의 훼손",
                              "8. 완전히 다른 작품"))
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=10)

top_term_topic_name <- top_term_topic %>% left_join(name_topic, by="topic")
top_term_topic_name

ggplot(top_term_topic_name,
       aes(x=reorder_within(term, beta, name),
           y=beta,
           fill=factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~name, scales = "free", ncol = 2) +
  coord_flip() + 
  scale_x_reordered() +
  labs(title = "영화 인어공주 댓글 토픽",
       subtitle = "토픽별 주요 단어 top 10",
       x = NULL, y = NULL) +
  theme(title = element_text(size = 14))

install.packages("ldatuning")
library(ldatuning)

models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:10,
                           return_models = T,
                           control = list(seed = 1234))
models %>% select(topics, Griffiths2004)

FindTopicsNumber_plot(models)


