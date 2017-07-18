library(readr)
library(ggplot2)
library(ggthemes)
library(png)

results2017tdsb <- read_csv("research/results_tdsb_2017.csv")
results2017 <- read_csv("research/results_2017.csv")
results2016 <- read_csv("research/results_2016.csv")

myvars <- c("rating", "rank_overall")
newtdsb <- results2017tdsb[myvars]

linearFit <- subset(results2017, rating < 7 & rating > 5)

reckonerTheme = theme_fivethirtyeight() + theme(
  axis.title = element_text(),
  axis.title.x = element_text(hjust=0.5),
  plot.title = element_text(size = 15, lineheight = 0.9, face = "bold", hjust=0.5),
  plot.subtitle = element_text(hjust=0.5)
)

lm_eqn <- function(){
  eq <- substitute(italic(y) == a %.% italic(x)* + b + " ,"~~italic(R)^2~"="~r2, 
                   list(a = -0.0077, 
                        b = 8.8175, 
                        r2 = 0.9956))
  as.character(as.expression(eq)); 
  
}
ggplot(results2017, aes(x = rank_overall, y = rating)) + 
  geom_point(shape = 16) +
  geom_smooth(data = linearFit, aes(x = rank_overall,y = rating), method = lm) +
  geom_point(aes(x = 259, y = 6.8), color = "#669D31", shape = 18, size = 3) + # St. Francis SS
  geom_point(aes(x = 358, y = 6.3), color = "#991B1E", shape = 18, size = 3) + # Chatham-Kent SS
  annotate("text", x = 580, y = 6.8, label = lm_eqn(), parse = TRUE) +
  annotate("text", x = 180, y = 6.3, label = "St. Francis SS", color = "#669D31") +
  annotate("text", x = 280, y = 5.5, label = "Chatham-Kent SS", color = "#991B1E") +
  ggtitle("Fraser Institute Rating vs Overall Rank") +
  xlab("Overall Rank (out of 740)") +
  ylab("Rating (out of 10)") +
  reckonerTheme +
  theme(legend.position = "none")

linearFit.lm = lm(rating ~ rank_overall, data=linearFit)

summary(linearFit.lm)

