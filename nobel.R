#set work directory
getwd()
setwd("C:\\ÎÄµµ\\ÎÄÕÂ\\nobel prize")
getwd()
nobel <- read.csv("nobel price.csv")


#package
install.packages("ggplot2")
library(ggplot2)
suppressWarnings(suppressMessages(library("ggplot2")))
install.packages("dplyr")
library(dplyr)


#check the data
summary(nobel)
str(nobel)
factor(nobel$award.year)
nobel$award.year <- factor(nobel$award.year)

factor(nobel$Name)
nobel$Name <- factor(nobel$Name)

factor(nobel$Award)
nobel$Award <- factor(nobel$Award)

factor(nobel$Gender)
nobel$Gender <- factor(nobel$Gender)

factor(nobel$Birth.Country)
nobel$Birth.Country <- factor(nobel$Birth.Country)

factor(nobel$Birth.Continent)
nobel$Birth.Continent <- factor(nobel$Birth.Continent)

factor(nobel$award.country)
nobel$award.country <- factor(nobel$award.country)

factor(nobel$award.continent)
nobel$award.continent <- factor(nobel$award.continent)

#-----------------------------------Gender
#gender gap
a <- ggplot(data = nobel, aes(x=Gender))
a + stat_count(width = 0.5,fill = "white", color = "DarkBlue")

#nobel by gender and category
b<- ggplot(data = nobel, aes(x=Award, fill=Gender))
b + stat_count(width = 0.5) +
  facet_grid(Gender~.,scales = "free")


#Gender over Time
p1 <- as.data.frame(table(nobel$award.year, nobel$Gender))
colnames(p1) <- c("year", "gender", "Freq")
p2 <- mutate(group_by(p4, gender), cumsum = cumsum(Freq))
ggplot(subset(p2, gender != "Org")) + 
  geom_point(aes(year,log(cumsum), color = gender)) + 
  scale_x_discrete(breaks = seq(1901, 2020, 10)) +
  scale_color_manual(values = c("darkorange", "skyblue","Green")) +
  labs(x = "Year", y = "log(cumulative sum) of laureates", 
       title = "Cumulative Sum of Nobel Laureates by Gender over Time")


#Gender over time by category

p3 <- as.data.frame(table(nobel$award.year, nobel$Award, nobel$Gender))
colnames(p3) <- c("year", "category", "gender", "Freq")
p4<- mutate(group_by(p3, category, gender), cumsum = cumsum(Freq))
ggplot(subset(p4, gender != "Org")) + geom_point(aes(year, log(cumsum), color = gender)) + 
  facet_grid(category ~ .) + 
  theme_bw() + 
  scale_x_discrete(breaks = seq(1901, 2020, 10)) + 
  scale_color_manual(values = c("darkorange", "skyblue","Green")) + 
  labs(x = "Year", y = "log(cumulative sum) of laureates", 
       title = "Cumulative Sum of Nobel Laureates by Gender & Category over Time")


#-----------------------------------Age
#age by category
ggplot(subset(nobel, Gender != "Org")) + 
  geom_violin(aes(x = Award, y = award.age), fill = "LightBlue") +
  stat_summary(aes(x = Award, y = award.age), 
               fun.y = "median", geom = "point") + 
  labs(x = "Category", y = "Age (years)", 
       title = "Age Distribution of Nobel Laureates by Category")


#age distribution by year awarded
ggplot(subset(nobel, Gender != "Org"), 
       aes(x = award.year, y = award.age)) +
  facet_wrap(.~Award) + 
  geom_point() + 
  geom_smooth() +
  labs(x = "Year", y = "Age(years) at end of year", 
       title = "Age of Nobel Laureates Over Time by Category")


#-----------------------------------Continent
#continent by category
ggplot(data = nobel, aes(x = award.continent)) + 
  stat_count(width = 0.5,aes(fill = Award)) +
  labs(x = "Continent", y = "Count", title = "All Nobel Prizes by Continent and Category") + 
  scale_fill_manual(values = c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494"),
                    name = "Category")


#continent by Gender
ggplot(data = nobel, aes(x = award.continent)) + 
  stat_count(width = 0.5, aes(fill = Gender)) +
  labs(x = "Continent", y = "Count", 
       title = "All Nobel Prizes by Continent and Gender") +
  scale_fill_manual(values = c("#41b6c4", "#ffd800", "#253494"), 
                    name = "Gender")

#Continent over time
p5 <- as.data.frame(table(nobel$award.year, nobel$award.continent))
colnames(p5) <- c("year", "Continent", "Freq")
p6<- mutate(group_by(p5, Continent), cumsum2 = cumsum(Freq))
ggplot(data = p6) + geom_point(aes(year, log(cumsum2), 
                                   color = B.Continent)) + 
  scale_x_discrete(breaks = seq(1901, 2020, 10)) +
  labs(x = "Year", y = "log(cumulative sum) of Continent", 
       title = "Cumulative Sum of Nobel Laureates by Continent over Time")

  





