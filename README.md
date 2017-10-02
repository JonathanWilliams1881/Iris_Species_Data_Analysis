# Iris_Species_Data_Analysis
Regression Analysis of Iris Dataset

Iris Species Data Analysis
Jonathan Williams
July 8, 2017
This is my first Markdown Document
```r
data("iris")
View(iris)

library(ggplot2)
## Warning: package 'ggplot2' was built under R version 3.4.1
library(GGally)
## Warning: package 'GGally' was built under R version 3.4.1
my.structure <- str(iris)
## 'data.frame':    150 obs. of  5 variables:
##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
my.summary <- summary(iris)
my.summary
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
##        Species  
##  setosa    :50  
##  versicolor:50  
##  virginica :50  
##                 
##                 
## 
```
We want to be able to differentiate the species based upon some defining characteristics. Petal Length has the greatest overall variability so let's begin by separating dataframes by species.
We need a factor that has unique characteristics for each species in order to distinguish the flower type. Let's check if the Petal Lengths seem distinguishable by species.
setosa.df <- subset(iris, iris$Species == "setosa")
setosa.summary.pl <- summary(setosa.df$Petal.Length)
setosa.summary.pl
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.400   1.500   1.462   1.575   1.900
versicolor.df <- subset(iris, iris$Species == "versicolor")
versicolor.summary.pl <- summary(versicolor.df$Petal.Length)
versicolor.summary.pl
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    3.00    4.00    4.35    4.26    4.60    5.10
virginica.df <- subset(iris, iris$Species == "virginica")
virginica.summary.pl <- summary(virginica.df$Petal.Length)
virginica.summary.pl
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   4.500   5.100   5.550   5.552   5.875   6.900
The means of Petal Length may be different for each species. Their ranges are clearly different, So we make a dataset of petal lengths.
iris.petal.length <- data.frame(iris[ ,c("Petal.Length","Species")])
Let's make some informative plots
par(mfrow = c(1,2))
Quick.HistPL <- hist(iris.petal.length$Petal.Length,
                     xlab = "Petal Length",
                     main = "Histogram of Petal Length",
                     col = "#32CD32",
                     right = )
Quick.BoxPL <- boxplot(setosa.df$Petal.Length,
                       versicolor.df$Petal.Length, 
                       virginica.df$Petal.Length,
                       names = c("setosa", "versicolor", "virginica"),
                       main = "Boxplot of Petal Length",
                       col = c("red", "yellow", "blue"))

From these graphs, there is an indication that the Petal Lengths of the setosa flower may be different from the petal lengths of versicolor and virginica.
Let's gather better descriptive statistics about Petal Length of each species
var.stats.list.pl <- list(Setosa = list(c(Min = min(setosa.df$Petal.Length),
                                          Max = max(setosa.df$Petal.Length),
                                          Med = median(setosa.df$Petal.Length),
                                          Mean = mean(setosa.df$Petal.Length),
                                          SD =sd(setosa.df$Petal.Length))),
                       Versicolor = list(c(Min = min(versicolor.df$Petal.Length),
                                           Max = max(versicolor.df$Petal.Length),
                                           Med = median(versicolor.df$Petal.Length),
                                           Mean = mean(versicolor.df$Petal.Length),
                                           SD = sd(versicolor.df$Petal.Length))),
                       Virginica = list(c(Min = min(virginica.df$Petal.Length),
                                          Max = max(virginica.df$Petal.Length),
                                          Med = median(virginica.df$Petal.Length),
                                          Mean = mean(virginica.df$Petal.Length),
                                          SD = sd(virginica.df$Petal.Length)))
                       )
Petal.Length.Summary <- data.frame(var.stats.list.pl) 
names(Petal.Length.Summary) <- c("Setosa Petal.Length", "Versicolor Petal.Length" ,"Virginica Petal.Length")
Petal.Length.Summary
##      Setosa Petal.Length Versicolor Petal.Length Virginica Petal.Length
## Min             1.000000                3.000000              4.5000000
## Max             1.900000                5.100000              6.9000000
## Med             1.500000                4.350000              5.5500000
## Mean            1.462000                4.260000              5.5520000
## SD              0.173664                0.469911              0.5518947
Let's continue by comparing the means of the Petal Lengths of the three flowers.
Null Hypothesis: Mean petal lenghts of all three species of flowers is the same.
Alternative Hypothesis: At least one of the means is different from the others
linear.model.pl <- aov(Petal.Length ~ Species, data = iris.petal.length)
anova(linear.model.pl)
## Analysis of Variance Table
## 
## Response: Petal.Length
##            Df Sum Sq Mean Sq F value    Pr(>F)    
## Species     2 437.10 218.551  1180.2 < 2.2e-16 ***
## Residuals 147  27.22   0.185                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TUKEY <- TukeyHSD(linear.model.pl, ordered = TRUE)
TUKEY
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
##     factor levels have been ordered
## 
## Fit: aov(formula = Petal.Length ~ Species, data = iris.petal.length)
## 
## $Species
##                       diff     lwr     upr p adj
## versicolor-setosa    2.798 2.59422 3.00178     0
## virginica-setosa     4.090 3.88622 4.29378     0
## virginica-versicolor 1.292 1.08822 1.49578     0
plot(TUKEY, las = 1, col = "red")

Updated Histogram of Petal Length
HistPL <- ggplot(iris.petal.length, aes(x = Petal.Length))+
  geom_histogram(binwidth = 0.2, color = "black", aes(fill = Species))+
  geom_vline(aes(xintercept = mean(Petal.Length), color = Species), linetype = "dashed", color = "grey", size = 1)+
  xlab("Petal Length")+
  ylab("Frequency")+
  ggtitle("Histogram of Petal Length")
HistPL

So far, we conclude that there is a difference among mean petal lengths for the different species of iris flowers. Thus, Petal Length is a good way to tell the flowers apart. The issue is that Petals are only visible after a flower has already blossomed. We want to predict the flower type beforehand.
A logical continuation is to examine whether the sepal factors are correlated to petal length. In this way, we can use sepal findings to drive our predictions of petal lengths. Finally we can classify our flower.
Is there a relationship between Sepal Length and
Petal Length?
corr.matrix <- ggpairs(data = iris[1:3],
                       title = "Correlation Matrix",
                       upper = list(continuous = wrap("cor", size = 5)),
                       lower = list(continuous = "smooth")
)
corr.matrix

The Correlationg Matrix shows a strong relationship b/w Petal Length and Sepal Length (rsquare = .872). We begin by examining Sepal Length.
Descriptive statistics about Sepal Length of each species
var.stats.list.sl <- list(Setosa = list(c(Min = min(setosa.df$Sepal.Length),
                                          Max = max(setosa.df$Sepal.Length),
                                          Med = median(setosa.df$Sepal.Length),
                                          Mean = mean(setosa.df$Sepal.Length),
                                          SD =sd(setosa.df$Sepal.Length))),
                          Versicolor = list(c(Min = min(versicolor.df$Sepal.Length),
                                              Max = max(versicolor.df$Sepal.Length),
                                              Med = median(versicolor.df$Sepal.Length),
                                              Mean = mean(versicolor.df$Sepal.Length),
                                              SD = sd(versicolor.df$Sepal.Length))),
                          Virginica = list(c(Min = min(virginica.df$Sepal.Length),
                                             Max = max(virginica.df$Sepal.Length),
                                             Med = median(virginica.df$Sepal.Length),
                                             Mean = mean(virginica.df$Sepal.Length),
                                             SD = sd(virginica.df$Sepal.Length)))
)
Sepal.Length.Summary <- data.frame(var.stats.list.sl)
names(Sepal.Length.Summary) <- c("Setosa Sepal.Length", "Versicolor Sepal.Length", "Virginica Sepal.Length")
Sepal.Length.Summary
##      Setosa Sepal.Length Versicolor Sepal.Length Virginica Sepal.Length
## Min            4.3000000               4.9000000              4.9000000
## Max            5.8000000               7.0000000              7.9000000
## Med            5.0000000               5.9000000              6.5000000
## Mean           5.0060000               5.9360000              6.5880000
## SD             0.3524897               0.5161711              0.6358796
Histogram Sepal Length
HistSL <- ggplot(data = iris, aes(x = Sepal.Length))+
  geom_histogram(binwidth = 0.2, col = "black", aes(fill = Species))+
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "gray", linetype = "dashed", size = 1)+
  xlab("Sepal Length")+
  ylab("Frequency")+
  ggtitle("Histogram of Sepal Length")
HistSL

Sepal Lengths for Setosa, Versicolor, and Virginica flowers appear to be normally distributed around 5cm, 5.9cm and 6.5cm, respectively.
One-Way ANOVA of Petal Length vs Sepal Length, including Bartlett's Test
linear.model.slpl <- aov(iris$Petal.Length ~ iris$Sepal.Length, data = iris)
anova(linear.model.slpl)
## Analysis of Variance Table
## 
## Response: iris$Petal.Length
##                    Df Sum Sq Mean Sq F value    Pr(>F)    
## iris$Sepal.Length   1 352.87  352.87  468.55 < 2.2e-16 ***
## Residuals         148 111.46    0.75                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
bartlett.test(list(iris$Sepal.Length, iris$Petal.Length), data = iris)
## 
##  Bartlett test of homogeneity of variances
## 
## data:  list(iris$Sepal.Length, iris$Petal.Length)
## Bartlett's K-squared = 78.027, df = 1, p-value < 2.2e-16
Bartlett's test detects inequality of variances, but this is OK because the sample sizes are equal. The ANOVA test is significant suggesting a relationship between Sepal Length and Petal Length.
Linear Regression Plot of Petal Length vs Sepal Length colored by Species
regression <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length))+
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(method = lm)+
  xlab("Sepal Length")+
  ylab("Petal Length")+
  ggtitle("Petal Length vs Sepal Length")
regression

Only the head and tail of the prediciton values of Petal Length based on Sepal Length are shown below, for convenience...
df <- data.frame(iris.petal.length, predict(linear.model.slpl))
df$Sepal.Length <- iris$Sepal.Length
df <- df[ ,c(2,4,1,3)]
names(df) <- c("Species", "Sepal Length", "Petal Length", "Prediction.pl")
assign("Prediction.df", df)
rm(df)
head(Prediction.df)
##   Species Sepal Length Petal Length Prediction.pl
## 1  setosa          5.1          1.4      2.376565
## 2  setosa          4.9          1.4      2.004878
## 3  setosa          4.7          1.3      1.633192
## 4  setosa          4.6          1.5      1.447348
## 5  setosa          5.0          1.4      2.190722
## 6  setosa          5.4          1.7      2.934095
tail(Prediction.df)
##       Species Sepal Length Petal Length Prediction.pl
## 145 virginica          6.7          5.7      5.350058
## 146 virginica          6.7          5.2      5.350058
## 147 virginica          6.3          5.0      4.606684
## 148 virginica          6.5          5.2      4.978371
## 149 virginica          6.2          5.4      4.420841
## 150 virginica          5.9          5.1      3.863311
