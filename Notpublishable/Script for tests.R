library(tidyverse)
library(ggthemes)
library(scales)
setwd("~/Dropbox/R/Site-Gabriel/content/post")

Percentage <- as.numeric(Digit$X__2)
Digit <- as.numeric(Digit$X__1)
Base <- as.data.frame(cbind(Digit, Percentage))
write_xlsx(Base, "Base.xlsx")

ggplot(data = Base, mapping = aes(Digit, Percentage)) +
  geom_col() +
  theme_economist() +
  scale_y_continuous(name="Distribution", limits=c(0, 0.22), labels = percent) +
  scale_x_discrete(name="First Digit", breaks=c("1","2","3","4","5","6","7","8","9"),
                   labels=c("1","2","3","4","5","6","7","8","9")) +
  ggtitle("Distribution of the first Digit 1-99")

Digit <- c(1,2,3,4,5,6,7,8,9)
Distribution <- c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9)
Benford <- c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
Base <- as.data.frame(cbind(Digit, Distribution, Benford))

write_xlsx(Base, "Base.xlsx")


```{r Beta, tidy=FALSE, echo=FALSE, message=FALSE, include=TRUE, warning=FALSE}
Base <- read_excel("Base.xlsx", sheet=1, col_names = TRUE)
Benford <- as.data.frame(cbind(Base$Digit, Base$Benford))
ggplot(data = Base, mapping = aes(Digit, Benford)) +
  geom_col() +
  theme_economist() +
  ggtitle("Benford Distribution")
```

ggplot(data = Base, mapping = aes(Digit, Benford)) +
  geom_col() +
  theme_economist() +
  ggtitle("Benford Distribution")




  
