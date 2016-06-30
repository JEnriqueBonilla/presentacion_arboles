library(tidyr)
library(dplyr)
library(ggplot2)
library(tree)
library(randomForest)
library(rpart)
library(mlbench)

options(digits=2)
control.completo <- rpart.control(cp=0, minsplit=10,
                                minbucket=1, xval=10, maxdepth=30)

tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
tree2 <- rpart(Species ~ Sepal.Width + Petal.Width, method = "class", data = iris, control = control.completo)


#dendograma

# plot(tree2) 
# text(tree2, use.n=TRUE) 

ddata <- dendro_data(tree2, uniform =T)

ggplot() + 
  geom_segment(data = ddata$segments, 
               aes(x = x, y = y, xend = xend, yend = yend), colour = "brown", size = 1) + 
  geom_text(data = ddata$labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 5, vjust = -1) +
  geom_text(data = ddata$leaf_labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 5, vjust = 1) +
  theme_dendro() 


#partición

plot(iris$Petal.Width, iris$Sepal.Width, pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species", add = T)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

#extra

data <- tree2$cptable %>% data.frame

names(data) <- c("cp","nsplit", "rel_error", "xerror", "xstd")

data %>% 
  gather(variable, valor, rel_error:xstd ) %>% 
  filter(variable != "xstd") %>% 
  ggplot(aes(x = nsplit, y = valor, group = variable, colour = variable, label = as.numeric(cp))) +
  geom_point() + geom_line() + geom_text()

