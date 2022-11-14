library(readr)
library(stringr)
library(RCurl)

url<- "https://www.cleveland.com/metro/2017/12/case_western_reserve_university_president_barbara_snyders_base_salary_and_bonus_pay_tops_among_private_colleges_in_ohio.html"
html<- paste(readLines(url), collapse=" ")
html
guess_encoding(url)
Encoding(html)<-"UTF-8"

answer<- unlist(str_extract_all(html, "<li><span>.*?\\$[\\d+,]+</span></li>"))
answer<- gsub("<.*?>", "", answer)
answer

# pay.pattern <-"\\$\\d+,\\d+" (이게 나은듯)
pay.pattern <- "\\$[\\d+,]+"
pay<- unlist(str_extract_all(answer,pay.pattern)) %>%
  gsub("\\D", "", .) %>%
  as.numeric()
pay
class(pay)
# $를 없애줘야 숫자로 변환이 가능, 원래 gsub는 마지막에 string을 지정해줘야 
# 하는데 %>%로 연결해서 .을 찍음

pay.pattern <- "\\$[\\d+,]+"
pay<- unlist(str_extract_all(answer,pay.pattern)) 
pay<- gsub("\\D", "", pay)
pay<- as.numeric(pay)
class(pay)
pay

president.pattern<- "[[:alpha:] .]{2,}\\," 
  # "[[:alpha:] .]{2,}" 이게 나은거 같네..
president<- unlist(str_extract(answer,president.pattern))
president<- gsub(",", "", president)
president

university.pattern<- "\\,[[:alpha:] ]{2,}"
university<- unlist(str_extract_all(answer,university.pattern))
university<- gsub(", ", "", university)
university

library(tibble)
library(tidyr)
library(dplyr)


# (a)
president.pay <- tibble(pay=pay, president=president, university=university) %>%
  select("president", "university", "pay")
president.pay


library(ggplot2)
library(scales)
library(forcats)

# (b)
ggplot(president.pay, aes(x=pay, y=fct_reorder(president,pay)))+
  geom_col(fill="salmon", color="gray50")+
  labs(x="Total Compensation Per Year (dollars)",
       y="President",
       title="Total Compensation of University President",
       subtitle="Private Universities in Ohio, US",
       caption="Source: cleveland.com")+
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))+
  geom_text(aes(label=paste0("$", format(pay, big.mark=","))),
            size=3, color="dimgray", fontface="bold", hjust=-0.1) +
  geom_text(aes(label=paste(university)), size=1.8, color="dark blue",
            x=10, hjust=0, fontface="bold")+
  scale_x_continuous(limit=c(0,1100000), expand=c(0,0), labels=comma)
  
  











