
data("iris")

install.packages("ggplot2")
library(ggplot2)
install.packages("GGally")
library(GGally)

my.structure <- str(iris)

my.summary <- summary(iris)
my.summary

setosa.df <- subset(iris, iris$Species == "setosa")
setosa.summary.pl <- summary(setosa.df$Petal.Length)
setosa.summary.pl

versicolor.df <- subset(iris, iris$Species == "versicolor")
versicolor.summary.pl <- summary(versicolor.df$Petal.Length)
versicolor.summary.pl

virginica.df <- subset(iris, iris$Species == "virginica")
virginica.summary.pl <- summary(virginica.df$Petal.Length)
virginica.summary.pl

iris.petal.length <- data.frame(iris[ ,c("Petal.Length","Species")])

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

linear.model.pl <- aov(Petal.Length ~ Species, data = iris.petal.length)
anova(linear.model.pl)

TUKEY <- TukeyHSD(linear.model.pl, ordered = TRUE)
TUKEY

plot(TUKEY, las = 1, col = "red")

HistPL <- ggplot(iris.petal.length, aes(x = Petal.Length))+
  geom_histogram(binwidth = 0.2, color = "black", aes(fill = Species))+
  geom_vline(aes(xintercept = mean(Petal.Length), color = Species), linetype = "dashed", color = "grey", size = 1)+
  xlab("Petal Length")+
  ylab("Frequency")+
  ggtitle("Histogram of Petal Length")
HistPL

corr.matrix <- ggpairs(data = iris[1:3],
                       title = "Correlation Matrix",
                       upper = list(continuous = wrap("cor", size = 5)),
                       lower = list(continuous = "smooth")
)
corr.matrix

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

HistSL <- ggplot(data = iris, aes(x = Sepal.Length))+
  geom_histogram(binwidth = 0.2, col = "black", aes(fill = Species))+
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "gray", linetype = "dashed", size = 1)+
  xlab("Sepal Length")+
  ylab("Frequency")+
  ggtitle("Histogram of Sepal Length")
HistSL

linear.model.slpl <- aov(iris$Petal.Length ~ iris$Sepal.Length, data = iris)
anova(linear.model.slpl)

bartlett.test(list(iris$Sepal.Length, iris$Petal.Length), data = iris)

regression <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length))+
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(method = lm)+
  xlab("Sepal Length")+
  ylab("Petal Length")+
  ggtitle("Petal Length vs Sepal Length")
regression

df <- data.frame(iris.petal.length, predict(linear.model.slpl))
df$Sepal.Length <- iris$Sepal.Length
df <- df[ ,c(2,4,1,3)]
names(df) <- c("Species", "Sepal Length", "Petal Length", "Prediction.pl")
assign("Prediction.df", df)
rm(df)
head(Prediction.df)
tail(Prediction.df)
