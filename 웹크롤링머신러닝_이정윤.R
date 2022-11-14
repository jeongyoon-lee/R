# 1 #
library(xml2)
library(XML)
url <- "https://www.summet.com/dmsi/html/codesamples/addresses.html"
html<- paste(readLines(url), collapse=" ")
html
library(stringr)

name <- unlist(str_extract_all(html, "<li>.*?<br/>")) %>%
  gsub("<.*?>", "", .)
name

telephone <- unlist(str_extract_all(html, "\\(\\d{3}\\)\\s\\d{3}(-| )?\\d{4}")) %>%
  gsub("<.*?>", "", .)
telephone

library(tibble)
phonebook <- tibble(name=name, telephone=telephone)
head(phonebook)
dim(phonebook)


# 2 #
url <- "https://www.cnbc.com/2020/01/06/the-best-paying-jobs-of-2020-from-us-news-and-world-report.html"
html <- read_html(url)
html.parsed<- htmlParse(html)
job <- xpathSApply(html.parsed, "//h2[@class='ArticleBody-styles-makeit-subtitle--LnHeO']",
                   xmlValue, trim=T) %>%
  gsub("\\d{1,2}\\.", "", .)
job

rank<- xpathSApply(html.parsed, "//h2[@class='ArticleBody-styles-makeit-subtitle--LnHeO']",
                 xmlValue, trim=T) %>%
  str_extract_all(., "\\d{1,2}") %>%
  as.numeric()
rank

salary <- xpathSApply(html.parsed, "//h2[@class='ArticleBody-styles-makeit-subtitle--LnHeO']/following-sibling::div[@class='group']",
                      xmlValue, trim=T)[1:25] %>%
  str_extract_all(., "Mean salary.*?\\$\\d+\\,\\d+") %>%
  gsub("\\D", "", .) %>%
  as.numeric()
salary
library(tibble)
library(dplyr)
jobsalary <- tibble(rank=rank, job=job, salary=salary)
jobsalary <- jobsalary %>%
  arrange(desc(salary))
jobsalary


# 3 #
library(XML)
library(xml2)
library(stringr)

url <- "https://www.business.kaist.ac.kr/_prog/prof/index.php?site_dvs_cd=kr&menu_dvs_cd=040101&div_cd=&skey=&sval=&&GotoPage=1"
html <- read_html(url)
html.parsed<- htmlParse(html)
url2 <- "https://www.business.kaist.ac.kr/_prog/prof/index.php?site_dvs_cd=kr&menu_dvs_cd=040101&div_cd=&skey=&sval=&&GotoPage=2"
html2 <- read_html(url2)
html.parsed2<- htmlParse(html2)
url3<- "https://www.business.kaist.ac.kr/_prog/prof/index.php?site_dvs_cd=kr&menu_dvs_cd=040101&div_cd=&skey=&sval=&&GotoPage=3"
html3 <- read_html(url3)
html.parsed3<- htmlParse(html3)

url.basic<- "https://www.business.kaist.ac.kr"
profs <- xpathSApply(html.parsed, path="//div[@class='tit']/following-sibling::a",
                     fun=xmlGetAttr, "href") %>%
  str_c(url.basic, .)
profs

profs2 <- xpathSApply(html.parsed2, path="//div[@class='tit']/following-sibling::a",
                      fun=xmlGetAttr, "href") %>%
  str_c(url.basic, .)
profs2

profs3 <- xpathSApply(html.parsed3, path="//div[@class='tit']/following-sibling::a",
                      fun=xmlGetAttr, "href") %>%
  str_c(url.basic, .)
profs3

total.pages <- paste(c(profs, profs2, profs3))
total.pages

# ÀÌ¸§
url.prof <- total.pages[1]
html.url.prof <- read_html(url.prof)
html.url.prof.parsed <- htmlParse(html.url.prof)
name <- xpathSApply(html.url.prof.parsed, "//div[@class='name']", xmlValue,
                    trim=T) %>%
  str_replace(c("ºÎ±³¼ö|Á¶±³¼ö|±³¼ö|ÁÖÀÓ"), "")
name

# ÀÌ¸ÞÀÏ
url.prof <- total.pages[1]
html.url.prof <- read_html(url.prof)
html.url.prof.parsed <- htmlParse(html.url.prof)
email.url.prof <- xpathSApply(html.url.prof.parsed, "//div[@class='con']//li", xmlValue,
                              trim=T) %>% 
  paste(collapse="") %>%
  str_extract("E-mail.*@kaist.ac.kr") %>%
  str_replace("E-mail.", "")
email.url.prof

# research
url.prof <- total.pages[1]
html.url.prof <- read_html(url.prof)
html.url.prof.parsed <- htmlParse(html.url.prof)
research <- xpathSApply(html.url.prof.parsed, "//div[@class='txt1']//span", xmlValue,
                        trim=T) %>%
  str_replace(",.*", "")
research

# ¹øÈ£
url.prof <- total.pages[1]
html.url.prof <- read_html(url.prof)
html.url.prof.parsed <- htmlParse(html.url.prof)
phone <- xpathSApply(html.url.prof.parsed, "//div[@class='con']//li", xmlValue,
                     trim=T) %>%
  str_extract("Tel.*") %>% 
  paste(collapse="") %>%
  str_extract("[[:digit:]-]{2,}")
phone

# publication
url.prof <- total.pages[1]
html.url.prof <- read_html(url.prof)
html.url.prof.parsed <- htmlParse(html.url.prof)
publication <- xpathSApply(html.url.prof.parsed, 
                           "//div[@id='obj3']//ul[@class='list_1st']", xmlValue,
                           trim=T) %>%
  paste(collapse="")
publication

i <- 0
for (url.prof in total.pages) {
  i <- i + 1
  html.url.prof <- read_html(url.prof)
  html.url.prof.parsed <- htmlParse(html.url.prof)
  email.url.prof[[i]] <- xpathSApply(html.url.prof.parsed, "//div[@class='con']//li", xmlValue,
                                     trim=T) %>% 
    paste(collapse="") %>%
    str_extract("E-mail.*@kaist.ac.kr") %>%
    str_replace("E-mail.", "")
  research[[i]] <- xpathSApply(html.url.prof.parsed, "//div[@class='txt1']//span", xmlValue,
                               trim=T)  %>%
    str_replace(",.*", "")
  phone[[i]] <- xpathSApply(html.url.prof.parsed, "//div[@class='con']//li", xmlValue,
                            trim=T) %>%
    str_extract("Tel.*") %>% 
    paste(collapse="") %>%
    str_extract("[[:digit:]-]{2,}")
  name[[i]] <- xpathSApply(html.url.prof.parsed, "//div[@class='name']", xmlValue,
                           trim=T) %>%
    str_replace(c("ºÎ±³¼ö|Á¶±³¼ö|±³¼ö|ÁÖÀÓ"), "")
  publication[[i]] <- xpathSApply(html.url.prof.parsed, 
                                  "//div[@id='obj3']//ul[@class='list_1st']", xmlValue,
                                  trim=T)  %>%
    paste(collapse="")
}
library(tibble)
faculty.bizkaist.raw <- tibble(name=name, research=research, phone=phone,
                               email=email.url.prof, publication=publication)

faculty.bizkaist.raw

major <- xpathSApply(html.parsed, "//div[@class='txt_b']//li[1]", xmlValue,
                     trim=T) %>%
  as_tibble()
major

major2 <- xpathSApply(html.parsed2, "//div[@class='txt_b']//li[1]", xmlValue,
                      trim=T) %>%
  as_tibble()
major2

major3 <- xpathSApply(html.parsed3, "//div[@class='txt_b']//li[1]", xmlValue,
                      trim=T) %>%
  as_tibble()
major3

# Æ¼ºí ¿­ ÇÕÄ¡Å° 
library(dplyr)
total.major<- rbind(major, major2, major3)
total.major

faculty.bizkaist <- cbind(faculty.bizkaist.raw, total.major) %>%
  as_tibble() %>%
  rename(major=value) %>%
  select("name", "major", "research", "phone", "email", "publication")
faculty.bizkaist
View(faculty.bizkaist)


# 4 #
faculty.bizkaist.clean<- faculty.bizkaist %>%
  mutate(publication=str_replace_all(publication, "[°¡-ÆR]", ""))
faculty.bizkaist.clean

tib<- faculty.bizkaist.clean %>%
  rename(text=publication) %>%
  add_column(doc_id=1:nrow(.), .before=1)
tib

library(tidyverse)
library(tm)
docs <- tib %>% 
  select(doc_id=doc_id, text=text, everything())
docs
corp <- VCorpus(DataframeSource(docs))
corp

corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, stripWhitespace)

dtm <- DocumentTermMatrix(corp)
dtm
inspect(dtm[1:10, 1:10])

kaist <- as_tibble(as.matrix(dtm)) %>% 
  mutate(major_area=faculty.bizkaist.clean$major, name=faculty.bizkaist.clean$name) %>% 
  select(major_area, name, everything())
kaist

toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels=c(0, 1), labels=c("n", "y"))
  return(x)
}

kaist <- kaist %>% 
  mutate(across(where(is.numeric), toFactor))
kaist

set.seed(123)
train <- sample(nrow(kaist), 0.8*nrow(kaist))
kaist.train <- kaist[train,]
kaist.test <- kaist[-train,]
table(kaist.train$major_area); sum(table(kaist.train$major_area))
table(kaist.test$major_area); sum(table(kaist.test$major_area))

# ³ªÀÌºêº£ÀÌÁî 
library(e1071)
kaist.nb <- naiveBayes(x=kaist.train[-1], y=kaist.train$major_area, type="class")

kaist.nb.pred <- predict(kaist.nb, newdata=kaist.test[-1])
head(kaist.nb.pred)
# (1) È¥µ¿Çà·Ä
table(kaist.test$major_area, kaist.nb.pred, dnn=c("Actual", "Predicted"))
sum(diag(table(kaist.test$major_area, kaist.nb.pred)))
# (2) ¿¹ÃøÁ¤È®µµ
mean(kaist.nb.pred==kaist.test$major_area)

name <- kaist.test$name
major <- kaist.test$major_area
predicted <- kaist.nb.pred

df <- data.frame(major, predicted)
rownames(df)<- name
df
View(df)
