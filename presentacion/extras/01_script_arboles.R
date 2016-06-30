library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggdendro)
library(tree)
library(rpart)
library(mlbench) #data BostonHousing

control.completo <- rpart.control(cp=0, minsplit=10,
                                minbucket=1, xval=10, maxdepth=30)

arbol1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
arbol2 <- rpart(Species ~ Sepal.Width + Petal.Width, method = "class", 
               data = iris, control = control.completo)


#dendograma

#plot(arbol2) 
#text(arbol2, use.n=TRUE) 

ddata <- dendro_data(arbol2, uniform =T)

ggplot() + 
  geom_segment(data = ddata$segments, 
               aes(x = x, y = y, xend = xend, yend = yend), colour = "brown", size = 1) + 
  geom_text(data = ddata$labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 5, vjust = -1) +
  geom_text(data = ddata$leaf_labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 5, vjust = 1) +
  theme_dendro() #+ scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta="x")


#partición

plot(iris$Petal.Width, iris$Sepal.Width, pch=19,col=as.numeric(iris$Species))
partition.tree(arbol1, label="Species", add = T)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

#ejemplo
data(BostonHousing)
set.seed(15)
train <- sample(1:nrow(BostonHousing),400)
BostonHousing$train <- F
BostonHousing$train[train] <- T
entrena.arb <- BostonHousing[BostonHousing$train,]
prueba.arb <- BostonHousing[!BostonHousing$train,]
entrena.arb$train <- NULL

modelo.arb.completo <- rpart(medv~., data = entrena.arb, method = "anova", 
                       control=rpart.control(cp=0 , xval=10, minbucket=1))

ddata <- dendro_data(modelo.arb.completo , uniform =T)

ggplot() + 
  geom_segment(data = ddata$segments, 
               aes(x = x, y = y, xend = xend, yend = yend), colour = "brown", size = 1) + 
  geom_text(data = ddata$labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 1.5, vjust = -1) +
  geom_text(data = ddata$leaf_labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 1.5, vjust = 1) +
  theme_dendro() + scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta="x")

# cp = 25
plotcp(modelo.arb.completo)

(error.completo <- modelo.arb.completo$cptable %>%
  data.frame() %>% 
  dplyr::select(nsplit, CP, xerror) )

alpha <- error.completo$CP

errores.vmc <- ldply(alpha, 
                     function(i) {
                       
                      modelo <- rpart(medv~., data = entrena.arb, method = "anova", 
                                                             control=rpart.control(cp=i , xval=10, minbucket=1))
                      
                      error.entrena <- mean((predict(modelo) - entrena.arb$medv)^2)
                       
                      error.prueba <- mean((predict(modelo, prueba.arb)- prueba.arb$medv)^2)
                       
                       data.frame(CP = as.numeric(i), prueba = error.prueba, 
                                  entrenamiento = error.entrena)
                     })

errores.vmc


errores.vmc %>% 
  left_join(error.completo %>% 
              dplyr::select(-xerror)) %>% 
  gather(variable, valor, -CP, -nsplit) %>% 
  ggplot(aes(x = nsplit, y = valor, 
             colour = variable,
             group = variable)) + 
  geom_line() +
  geom_vline(xintercept = 25) +
  xlab("Altura del árbol") + 
  ylab("Error")

modelo.arb.final <- rpart(medv~., data = entrena.arb, method = "anova", 
                             control=rpart.control(cp=0.0018 , xval=10, minbucket=1))

ddata <- dendro_data(modelo.arb.final , uniform =T)

ggplot() + 
  geom_segment(data = ddata$segments, 
               aes(x = x, y = y, xend = xend, yend = yend), colour = "brown", size = 1) + 
  geom_text(data = ddata$labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 3, vjust = -1) +
  geom_text(data = ddata$leaf_labels, 
            aes(x = x, y = y, label = label), colour = "#006633", size = 3, vjust = 1) +
  theme_dendro() 

#error de prueba
mean((predict(modelo.arb.final, prueba.arb)- prueba.arb$medv)^2)
