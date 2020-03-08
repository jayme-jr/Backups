library(labestData)

library(ggplot2)



dados<-iris

names(iris)



ggplot(data = dados, 
       
       aes(x = Sepal.Length)) +
  
  geom_histogram(fill = "white", 
                 
                 colour = "red") +
  
  theme_dark()



ggplot(data = dados, 
       
       aes(x = Petal.Length,
           
           y = Sepal.Length,
           
           size =iris$Petal.Width )) +
  
  geom_point() +
  
  geom_smooth(method = 'loess') +
  
  facet_grid(~Species)



ggplot(data = dados, 
       
       aes(x = Petal.Length)) +
  
  geom_bar()+
  
  coord_flip()



ggplot(data = dados, 
       
       aes(x = Petal.Length,
           
           color = Species, 
           
           fill = Species)) +
  
  geom_density(alpha=0.3)



ggplot(data = dados, 
       
       aes(x = Petal.Length,
           
           y = Sepal.Length)) +
  
  geom_point(color = 'red') +
  
  geom_smooth(method = 'lm')+
  
  geom_rug()