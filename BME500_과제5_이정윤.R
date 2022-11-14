library(xml2)
library(tidyr)
url<- "https://www.nobelprize.org"
html<- read_html(url)
html
library(XML)
library(stringr)
html.parsed<- htmlParse(html)

areas <- xpathSApply(html.parsed, path="//ul[@class='sub-menu']//a[@href]",
                     fun=xmlGetAttr, "href") %>%
  .[2:7] %>%
  str_c(url, .)
areas


area<- str_sub(areas[1], start=str_locate(areas[1],
                                              "prizes/")[,2]+1, end=-2)
area


html<- read_html(areas[1])
html
html.parsed<- htmlParse(html)
url.winner<- xpathSApply(html.parsed, path="//div[@class='text']//a[@href]",
                         fun=xmlGetAttr, "href") %>%
  .[2]
url.winner

url.winner.full<- xpathSApply(html.parsed, path="//div[@class='text']//a[@href]",
                              fun=xmlGetAttr, "href")

url.winner.full
library(dplyr)

html <- read_html(url.winner)
html
html.parsed<- htmlParse(html)
title <- xpathSApply(html.parsed, "//h3/a", xmlValue) %>%
  word(end=-2) #뒤에서 두번째 단어까지 가져오라는 말(처음부터~뒤마지막2번쨰)
title

library(readr)
year<- xpathSApply(html.parsed, "//h3/a", xmlValue) %>%
  parse_number()
year

winner.year <- xpathSApply(html.parsed, str_c("//p//a[contains(@href, '", year[1],"')]/ancestor::p/a"),
                           fun=xmlValue) %>%
  str_c(collapse="; ")
winner.year

achievement.year <- xpathSApply(html.parsed, str_c("//p//a[contains(@href, '", year[1],"')]/ancestor::p"),
                          xmlValue) %>%
  str_sub(str_locate(., "“|”")[,1]) %>%
  str_replace_all("“|”", "") %>%
  str_c(collapse="; ")
achievement.year
# str_locate(string, pattern)인데 string자리를 비울 수 없어서 . 찍은거
# str_locate는 패턴 위치(인덱스)를 행렬로 반환해줌. 
# 따라서 [,1]을 쓴건데 [1,],[,2],[,1] 다 되는듯...?

winner<- vector("list", length(year)) 
achievement <- vector("list", length(year))
i <- 0
for (y in year) {
  i <- i + 1
  winner[[i]]<- xpathSApply(html.parsed, str_c("//p//a[contains(@href, '", y,"')]/ancestor::p/a"),
                                 fun=xmlValue) %>%
    str_c(.,collapse="; ")
  achievement[[i]]<- xpathSApply(html.parsed, str_c("//p//a[contains(@href, '", y,"')]/ancestor::p"),
                                 xmlValue) %>%
    str_sub(str_locate(., "“|”")[,1]) %>% 
    str_replace_all("“|”", "") %>%
    str_c(collapse="; ")
}
winner
achievement

library(purrr)
winner %>%
  map_chr(~str_c(., collapse="; "))
achievement %>%
  map_chr(~str_c(., collapse="; "))


# 1.

getNobelPrize <- function() {
  library(tidyverse)
  library(rvest)
  library(dplyr)
  library(XML)
  library(xml2)
  url <- "https://www.nobelprize.org"
  html <- read_html(url)
  html.parsed<- htmlParse(html)
  url.areas <- xpathSApply(html.parsed, path="//ul[@class='sub-menu']//a[@href]",
                       fun=xmlGetAttr, "href") %>%
    .[2:7] %>%
    str_c(url, .)
  nobel.prize <- tibble()
  for (url.area in url.areas) {
    area<- str_sub(url.area, start=str_locate(url.area,
                                              "prizes/")[,2]+1, end=-2)
    html <- read_html(url.area)
    html.parsed<- htmlParse(html)
    url.winner <- xpathSApply(html.parsed, path="//div[@class='text']//a[@href]",
                           fun=xmlGetAttr, "href") %>%
      .[2]
    html<- read_html(url.winner)
    html.parsed<- htmlParse(html)
    title <- xpathSApply(html.parsed, "//h3/a", xmlValue) %>%
      word(end=-2)
    year<- xpathSApply(html.parsed, "//h3/a", xmlValue) %>%
      parse_number()
    
    winner <- vector("list", length(year)) 
    achievement <- vector("list", length(year))
    i <- 0
    for (y in year) {
      i <- i + 1
      winner[[i]]<- xpathSApply(html.parsed, str_c("//p//a[contains(@href, '", y,"')]/ancestor::p/a"),
                                     fun=xmlValue) %>%
        str_c(.,collapse="; ")
      achievement[[i]]<- xpathSApply(html.parsed, str_c("//p//a[contains(@href, '", year[i],"')]/ancestor::p"),
                                     xmlValue) %>%
        str_sub(str_locate(., "“|”")[,1]) %>% 
        str_replace_all("“|”", "") %>%
        str_c(collapse="; ")
    }
    prize <- tibble(area=area, title=title, year=year,
                    winner=map_chr(winner, ~str_c(., collapse="; ")),
                    achievement=map_chr(achievement, ~str_c(., collapse="; ")))
    nobel.prize <- bind_rows(nobel.prize, prize)
    Sys.sleep(sample(10,1)*0.1)
  }
  nobel.prize<- bind_cols(id=1:nrow(nobel.prize), nobel.prize)%>%
    mutate(area=factor(area,
                       levels=c("physics", "chemistry", "medicine",
                                "literature", "peace", "economics")))
  return(nobel.prize)
}
nobel.prize<- getNobelPrize()
nobel.prize

View(nobel.prize)


# 2.

# 데이터 전처리
# 예측변수로 사용할 단어 추출- 공적 텍스트를 tm의 코퍼스로 변환
library(tidyverse)
library(tm)
docs <- nobel.prize %>% 
  select(doc_id=id, text=achievement, everything())
docs 
corp <- VCorpus(DataframeSource(docs))
corp

# 데이터 정제 작업
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, stemDocument)

# 코퍼스로부터 단어 추출 (문서-용어행렬 생성)
dtm <- DocumentTermMatrix(corp)
dtm
inspect(dtm[1:10, 1:10]) #문서-용어행렬 탐색

nobel <- as_tibble(as.matrix(dtm)) %>% 
  mutate(nobel_area=nobel.prize$area) %>% 
  select(nobel_area, everything())
nobel

# 예측변수의 값을 범주형 값으로 변환
toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels=c(0, 1), labels=c("n", "y"))
  return(x)
}

nobel <- nobel %>% 
  mutate(across(where(is.numeric), toFactor))
nobel

set.seed(123)
train <- sample(nrow(nobel), 0.7*nrow(nobel))
nobel.train <- nobel[train,]
nobel.test <- nobel[-train,]
table(nobel.train$nobel_area); sum(table(nobel.train$nobel_area))
table(nobel.test$nobel_area); sum(table(nobel.test$nobel_area))

# 의사결정나무
library(rpart)
nobel.dtree <- rpart(nobel_area ~ ., data=nobel.train, method="class",
                     parms=list(split="gini"), control=list(cp=0))
nobel.dtree

nobel.dtree.pred <- predict(nobel.dtree, newdata=nobel.test, type="class")
head(nobel.dtree.pred)

# (1) 혼동행렬
table(nobel.test$nobel_area, nobel.dtree.pred, dnn=c("Actual", "Predicted"))
sum(diag(table(nobel.test$nobel_area, nobel.dtree.pred)))
# (2) 예측정확도
mean(nobel.test$nobel_area==nobel.dtree.pred)

# 나이브베이즈 
library(e1071)
nobel.nb <- naiveBayes(x=nobel.train[-1], y=nobel.train$nobel_area, type="class")

nobel.nb.pred <- predict(nobel.nb, newdata=nobel.test[-1])
head(nobel.nb.pred)
# (1) 혼동행렬
table(nobel.test$nobel_area, nobel.nb.pred, dnn=c("Actual", "Predicted"))
sum(diag(table(nobel.test$nobel_area, nobel.nb.pred)))
# (2) 예측정확도
mean(nobel.nb.pred==nobel.test$nobel_area)
