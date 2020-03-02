#libraries
library(tidyverse)
library(gapminder)
library(patchwork)

# Get the Data
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

#integrate gapminder data
gap2007<- gapminder %>% filter(year==2007)
gap_food <- merge(food_consumption, gap2007, by="country")
gap_food <- gap_food %>% mutate(tot_co2 = co2_emmission*pop, tot_consump = consumption*pop)

#bar and lollipop plots
p1 <- gap_food %>% group_by(food_category) %>% summarise(total_co2 = sum(tot_co2)) %>%
  ggplot(aes(y=total_co2, x=reorder(food_category,total_co2))) +
  geom_bar(fill="orange", stat="identity") + coord_flip() + theme_minimal() +
  labs(title="Total global CO2 emissions from food by food category", x="", y="CO2 emissions")

p2 <- gap_food %>% group_by(country) %>%
  summarise(co2 = sum(co2_emmission), pop=mean(pop)) %>% filter(co2 > 1000) %>%
  ggplot(aes(x=co2, y=reorder(country, co2))) +
  geom_segment(aes(x=0, xend=co2,y=reorder(country, co2),yend=reorder(country, co2))) +
  geom_point(size=3, color="orange") + 
  theme_minimal() + labs(x="CO2 emissions per capita", y="", title="CO2 emissions from food per capity by country, highest ranked countries") +
  xlim(c(0,2500))

#scatter plots
gap_food_sum <- gap_food %>% group_by(country) %>%
  summarise(lifeExp=mean(lifeExp), pop=mean(pop), co2 = sum(co2_emmission)) %>% 
  mutate(high=co2>1500)

 p3 <- ggplot(gap_food_sum, aes(x=co2, y=lifeExp, size=pop, col=high)) +
  geom_point(alpha=0.5) + 
    geom_text(data=gap_food_sum %>% filter(high==T), aes(label=country), size=3, col="black", nudge_y=0.75) +
    theme_minimal() + theme(legend.position = "none") + scale_color_brewer(palette="Dark2") +
   labs(title="CO2 emissions per capita by country", subtitle="Size of bubble relative to population", x="CO2 emissions per capita", y="Life expectancy (years)")

gap_food_beef <- gap_food %>% filter(food_category=="Beef") %>% group_by(country) %>%
    summarise(lifeExp=mean(lifeExp), pop=mean(pop), co2 = sum(co2_emmission)) %>% 
    mutate(high=co2>750)
  
 p4 <- ggplot(gap_food_beef, aes(x=co2, y=lifeExp, size=pop, col=high)) +
    geom_point(alpha=0.5) + scale_color_brewer(palette="Dark2") +
    geom_text(data=gap_food_beef %>% filter(high==T), aes(label=country), size=3, col="black", nudge_y=0.75) +
    theme_minimal() + theme(legend.position="none") + labs(title="Too much beef? CO2 emissions from beef per capita by country", subtitle="Size of bubble relative to population", x="CO2 emissions per capita from beef", y="Life expectancy (years)", caption="Source: Gapminder and nu3.de")
  
 p1+p2+p3+p4 + plot_annotation(title="Don't have a cow, man. Beef eaters and global warming", 
                               theme = theme(plot.title = element_text(size = 18)))
 ggsave("co2andfood.png", height=10, width=15)
 