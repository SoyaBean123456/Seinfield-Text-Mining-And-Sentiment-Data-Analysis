#install.packages('readr')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('tm')
#install.packages('tmap')
#install.packages('wordcloud')
#install.packages('RColorBrewer')
#install.packages('gridExtra')
#install.packages('kableExtra')

library(readr)
library(dplyr)
library(ggplot2)
library(tm)
library(tmap)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)
library(kableExtra)

# set directory setwd("C:\\Users\\user\\Desktop\\WebA Assigment")

#import dataset
winere <- read_csv("winemag-data-130k-v2.csv")

# To get the dimension/size of the dataset
dim(winere)
#[1] 129971     14

# craete wordcloud  for designation and description
makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[50]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE,random.order=FALSE) 
}  
makeWordCloud(winere[["designation"]][1:50])
makeWordCloud(winere[["description"]][1:50])

# to analyse the distribution of graph then plot a distribution of graph.

# create 2 new variable that is totalpcnt and accum
wineCountry <- winere %>% 
  group_by(country) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  mutate(totpcnt = round(total/ sum(total), digits=7), accum = cumsum(totpcnt))
wineCountry

wineCountry %>% head(10) %>%
  ggplot( aes(x= factor(country, levels = wineCountry$country[order(desc(wineCountry$totpcnt))]), y = total)) +
  geom_col() + 
  geom_text(aes(label = sprintf("%.1f %%", 100*totpcnt), y = total + 1500)) +
  labs(x="Country", y="Total Reviews", title="Distribution of Wine Reviews by Top 10 Countries")

# to get the mean rating of wine
wineRating = winere %>% 
  group_by(country) %>%
  summarise_at(vars(points), funs(points = mean(., na.rm=T))) %>%
  arrange(desc(points)) %>%
  head(10)

# plot barchart of rating by country
ggplot(data=wineRating, aes(x=reorder(country,-points), y= points)) + 
  geom_bar(stat="identity", fill = "navy") + 
  coord_cartesian(ylim=c(85,92)) + 
  labs(x="Countries", y="Rating", title="Top 10 Countries by Average Rating")

# Create two graphs testing normality: histogram with normal dstribution and qq plot
q1=ggplot(winere, aes(x=points, col=I('gray'))) + 
  geom_histogram(binwidth = 1, aes(y=..density..)) +
  stat_function(fun=dnorm, args = list(mean = mean(winere$points), sd= sd(winere$points)), col = 'blue')

q2=ggplot(winere, aes(sample=c(scale(points)))) + stat_qq() + geom_abline(intercept = 0, slope = 1)

grid.arrange(q1, q2, nrow=1)


#clean the description and filter the country by spain
sw<-winere$description[winere$country=="Spain"]

sw<-VectorSource(sw)
sw<-VCorpus(sw)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stemDocument)
  #corpus <- tm_map(corpus, removeWords, stop_wine)
  corpus <-tm_map(corpus, stripWhitespace)
  return(corpus)
}
swc<-clean_corpus(sw)

swctdm<-TermDocumentMatrix(swc)

#creating matrix from TDM
swcm<-as.matrix(swctdm)
sf<-rowSums(swcm)
sf<-sort(sf, decreasing=T)

#creating a data frame
swf<-data.frame(term=names(sf), num=sf)

#creating a plot of 10 top stems
ggplot(data=head(swf, 10), aes(x=factor(term, levels = swf$term[order(-swf$num)]), y=num)) + 
  geom_col(fill="red") + 
  labs(x="Word Stems", y="Count", title="Top 10 Word Stems in Spanish Wine Reviews") +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,3200,400)) +
  
#creating a wordcloud
wordcloud(swf$term, swf$num, max.words=50, color="blue")

#plot chart for review and point of density
ggplot(winere, aes(x=points)) + geom_histogram(binwidth = 1, color= 'white') + 
  coord_cartesian(xlim = c(75, 100))+
  labs(title ="Points Histogram", x = "Points Given by Reviewer", y = "Number  of Reviews") +
  scale_x_continuous(breaks=seq(75,100, by = 1))

countries = winere %>%
  group_by(country) %>%
  count() 

top_countries = countries %>%
  filter(n>500)

Best_Wines = winere %>%
  filter(country %in% top_countries$country) %>%
  select(country,points)

Best_Wines %>%
  group_by(country) %>%
  summarise(Mean_Score = mean(points)) %>%
  arrange(desc(Mean_Score))%>%
  kable()

Best_Worst = Best_Wines %>%
  filter(country %in% c("Austria","Chile"))

ggplot(Best_Worst, aes(points, colour = country, fill = country)) + geom_density(alpha = .4) +
  labs(title ="Point Densities of Top and Bottom Countries with at Least 500 Reviews", x = "Points Given", y = "Density")

# plot Wine Score vs price
I_Wines = winere %>%
  select(price, points) %>%
  filter(!is.na(price)) %>%
  filter(!is.na(points))

ggplot(I_Wines, aes(x=price, y=points)) +    geom_jitter(shape=1)    + coord_cartesian(xlim = c(0, 4000), ylim = c(75, 100)) +
  labs(title ="Score vs Price", x = "Price", y = "Score")

# categories point into 3 grade
Wine_Eng = winere %>%
  mutate(grade = ifelse(points > 91,"Good",ifelse(points >86,"Average","Bad")))

Wine_Eng$grade = as.factor(Wine_Eng$grade)
Wine_Eng$grade = factor(Wine_Eng$grade,levels(Wine_Eng$grade)[c(3,1,2)])

#price of wine under 100 dollor
ggplot(Wine_Eng, aes(x=price, fill = grade)) + geom_histogram(binwidth = 5, color= 'white') + 
  coord_cartesian(xlim = c(0, 100))+
  labs(title ="Price Histogram for Wines Under $100", x = "Price in Dollars", y = "Number  of Reviews") +
  scale_x_continuous(breaks=seq(0,100, by = 10))

#price and score correlaction
ggplot(winere, aes(price, points, color = country)) + 
  geom_point(alpha = 0.01) +
  geom_count() +
  theme_minimal() + 
  labs(x = 'Price', y = 'Points', title = "Price Vs Ratings")+
  coord_flip()+
  scale_size_area(max_size = 10)

#Which wine have a better score
winere$wine_type <- ifelse(winere$variety == "Chardonnay" 
                             | winere$variety == "Riesling" 
                             | winere$variety == "Sauvignon Blanc" 
                             | winere$variety == "White Blend" 
                             | winere$variety == "Sparkling Blend" 
                             | winere$variety == "Pinot Gris" 
                             | winere$variety == "Champagne Blend" 
                             | winere$variety == "Grüner Veltliner" 
                             | winere$variety == "Pinot Grigio" 
                             | winere$variety == "Portuguese White" 
                             | winere$variety == "Viognier" 
                             | winere$variety == "Gewürztraminer" 
                             | winere$variety == "Gewürztraminer", "White Wine", "Red Wine")

winere %>%
  group_by(variety, wine_type) %>%
  summarise(n=n(),
            avg_score = mean(points),
            avg_price = mean(price)) %>%
  ggplot(aes(x=avg_price, y= avg_score, size = n, colour = wine_type))+
  geom_point()+
  scale_color_manual(values = c("#CC3300", "#FFCC00"))

#price over continent
NAmerica <- c('US', 'Canada', 'Mexico')
SAmerica <- c('Chile', 'Argentina', 'Brazil', 'Colombia', 'Uruguay', 'Paraguay', 'Bolivia', 'Peru', 'Venezuala')
Europe <- c('Germany', 'France', 'England', 'Spain', 'Portugal', 'Italy', 'Austria')
winere$continent <- ifelse(winere$country %in% NAmerica, 'NA', ifelse(winere$country %in% SAmerica, 'SA', ifelse(winere$country %in% Europe, 'EU', 'Other')))
winere$price <- ifelse(winere$price > 250, NA, winere$price)
ggplot(winere, aes(x=points, y=price, color=continent)) + geom_point() 

Price_points_reg <- lm(log(price) ~ points + wine_type, data = new_data)
summary(Price_points_reg)


#Regression Model
Price_points_reg <- lm(log(price) ~ points + wine_type, data = winere)
summary(Price_points_reg)
par(mfrow = c(2,2))
plot(Price_points_reg)

