
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(readr)

# 1~2. 

# 데이터 읽어오기
raw_data = readLines("text-mining-data.txt", encoding = "UTF-8")

# 한글 문자가 아닌 글자와, 공백을 제거하여 tibble 구조로 생성
data = raw_data %>% str_replace_all("[^가-힣]", replacement = " ") %>% str_squish() %>% as_tibble()


# 데이터를 단어 별로 쪼갬
word_space = data %>% unnest_tokens(input = value, output = word, token = "words")

# 단어 별로 개수를 세어줌
word_space = word_space %>% count(word, sort = T)

# 한 글자 짜리 단어는 의미를 가질 가능성이 적으므로 제외함
word_space = word_space %>% filter(str_count(word)>1)

# 가장 많이 사용된 단어 20개 
top20 <- word_space %>% head(20)


# 한글 깨짐 
# install.packages("extrafont")
#library(extrafont)
#font_import()

theme_set(theme_grey(base_family='AppleGothic'))

ggplot(top20, aes(x = reorder(word, n), y = n)) + 
  geom_col() + 
  coord_flip()


# 3. 

# 국민청원 '정치개혁' 데이터 읽어오기
raw_data2 = readLines("text-mining-data2.txt", encoding = "UTF-8")

# 한글 문자가 아닌 글자와, 공백을 제거하여 tibble 구조로 생성
data2 = raw_data2 %>% str_replace_all("[^가-힣]", replacement = " ") %>% str_squish() %>% as_tibble()

# 데이터를 단어 별로 나누어 단어 별로 개수를 세고, 한 글자 짜리 단어는 제외함
word_space2 = data2 %>% unnest_tokens(input = value, output = word, token = "words") %>% count(word, sort = T) %>% filter(str_count(word)>1)

# 비교를 위해 카테고리 이름을 붙여줌 
word_space <- word_space %>% mutate(category = "행정")
word_space2 <- word_space2 %>% mutate(category = "정치개혁")

# 두 데이터를 하나로 합침
freq <- bind_rows(word_space, word_space2)

# TF-IDF 분석
freq <- freq %>% bind_tf_idf(term = word, document = category, n = n)

# 각 데이터에서 중요하게 사용된 단어를 확인한다 
freq %>% filter(category == "행정")
freq %>% filter(category == "정치개혁")


# 4. 

# 감정사전 읽어오기
dic <- read_csv("knu_sentiment_lexicon.csv")

# 단어가 어느 문장에서 추출되었는지 알 수 있도록 남겨둠
word_data = data %>% unnest_tokens(input = value, output = word, token = "words", drop = F)

# 데이터를 기준으로 left join 하고, 감정사전에 해당 단어 정보가 없으면  0을 부여한다. 
word_data <- word_data %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# 문장 별로 그룹화 하여 해당 문장이 감정점수 몇점인지 계산한다. 
# (그룹화를 풀어두어 나중에도 데이터를 사용할 수 있도록 함)
score_data <- word_data %>% group_by(value) %>% summarise(score = sum(polarity)) %>% ungroup()

# 감정점수가 1점 이상인 문장에는 positive, -1점 이하이면 negative, 0점 이면 neutral 이라 한다. 
score_data <- score_data %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))

# 감정 별로 빈도를 구하고 비율을 낸다. 
freq_score <- score_data %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)

# 긍/부정/중립 빈도 그래프
ggplot(freq_score, aes(x = sentiment, y = n, fill = sentiment)) + 
  geom_col() +
  geom_text(aes(label = n), vjust = - 0.3) + 
  scale_x_discrete(limits = c("pos", "neu", "neg"))
