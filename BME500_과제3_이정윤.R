#1.
data(mtcars)
head(mtcars)
library(forcats)
library(ggplot2)
ggplot(mtcars, aes(x=mpg, y=fct_reorder(rownames(mtcars), mpg)))+
  geom_point(color="salmon")+
  labs(title="Car Model vs. Fuel Consumption",
       x="Fuel Consumption (miles per gallon)",
       y="Car Model")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        plot.title=element_text(size=9))


#2.
data(diamonds)
head(diamonds)
ggplot(diamonds, aes(x=carat, y=price, color=cut, fill=cut))+
  geom_point(color="gray", size=0.5)+
  geom_smooth()+
  facet_grid(~cut) +
  facet_wrap(~cut, nrow=2)+
  labs(title="Diamond Price Distribution by Carat and Cut",
       x="Carat", y="Price (dollars)") +
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text=element_text(size=6),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        strip.text = element_text(face="bold", size=6))
    #strip.text: fair, good 써있는 부분

#3.
url<-"http://www.statsci.org/data/general/cherry.txt"
cherry<- scan(url, skip=1, what=list(Diam=numeric(),
                                     Height=numeric(),
                                     Volume=numeric()))
cherry<- as.data.frame(cherry)
str(cherry)
cherry
library(ggplot2)
ggplot(cherry, aes(x=Height, y=Diam, size=Volume))+
  geom_point(shape=21, color="black", bg="red",alpha=0.5, stroke=0.7)+
  labs(title="Cherry Trees",
       subtitle="Relationship between height and diameter of cherry trees",
       caption="Source: www.statsci.org")+
  theme(plot.title=element_text(face="bold", size=10),
        axis.title=element_text(size=9),
        axis.text=element_text(size=7),
        plot.caption=element_text(size=6),
        plot.subtitle = element_text(size=8),
        legend.text = element_text(size=6),
        legend.title=element_text(size=7),
        legend.key = element_rect(fill="transparent"),
        panel.background=element_rect(fill="transparent", color="black"))
    #legend.key:범례에서 동그라미 크기 그려져있는 부분 배경색 