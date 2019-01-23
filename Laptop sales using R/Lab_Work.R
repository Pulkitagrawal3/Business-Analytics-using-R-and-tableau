toyota.df <- read.csv('ToyotaCorolla.csv')
library(ggplot2)

tbl <- table(toyota.df$Fuel_Type,toyota.df$Quarterly_Tax)
prop.tbl <- prop.table(tbl, margin =2)
barplot(prop.tbl, xlab="Tax", ylab="Fuel Type", yaxt="n",main="")
axis(2, at=(seq(0,1, 0.2)), paste(seq(0,100,20), "%"))

split(names(iris),sapply(iris,class))
split(names(iris),sapply(iris, function(x) paste(class(x), collapse=" ")))
split(names(iris),sapply(iris, function(x) paste(class(x), collapse=" ")))

$numeric
[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"

(subset(toyota.df, select = 'Fuel_Type'))
w = table(toyota.df$Fuel_Type)
factor(toyota.df$Automatic)


cor(toyota.df$Price,toyota.df$)


pcs.cor <- prcomp(na.omit(toyota.df[,-c(1,2,8,11)]))
summary(pcs.cor)

round(cor(toyota.df[,-c(1,2,8,11)]),2)

install.packages("corrplot")

library(corrplot)
corrplot(toyota.df, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

heatmap(cor(toyota.df[,-c(1,2,8,11)]), Rowv = NA, Colv = NA)
## heatmap

install.packages("gplots")
library(ggplot2)
library(reshape) # to generate input for the plot
cor.mat <- round(cor(toyota.df[,-c(1,2,8,11,15)]),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  geom_text(aes(x = X1, y = X2, label = value))


library(gplots)
heatmap.2(cor(toyota.df[,-c(1,2,8,11,15)]), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(toyota.df[,-c(1,2,8,11,15)]),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(1,1))