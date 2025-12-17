## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)


## ----eval=FALSE---------------------------------------------------------------------------------------------
# #install.packages('tinytex')
# #tinytex::install_tinytex()
# 
# #if you need to uninstall TinyTeX, run tinytex::uninstall_tinytex()


## -----------------------------------------------------------------------------------------------------------
library(tidyverse)


## -----------------------------------------------------------------------------------------------------------
compute_SS <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sum_squared_diff <- sum((x - mean_x)^2)
  return(sum_squared_diff)
}

# These vectors are a sample from a population.
vector_a <- c(5, 5, 5, 5, 5)
vector_b <- c(1, 3, 5, 7, 9)


## -----------------------------------------------------------------------------------------------------------
compute_SS(vector_a)
compute_SS(vector_b)


## -----------------------------------------------------------------------------------------------------------
library(datasauRus)
lab2_datasaurus<- datasaurus_dozen

# a, b
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~ dataset)

# c
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~ dataset) +
  scale_colour_grey()

# d
ggplot(lab2_datasaurus, aes(x = x, y = y, shape = dataset)) +
  geom_point() +
  facet_wrap(~ dataset) +
  scale_shape_manual(values=c(1:13))

# e
ggplot(lab2_datasaurus, aes(x = x, y = y, group = dataset)) +
  geom_boxplot()



## -----------------------------------------------------------------------------------------------------------
mean_datasaurus <- lab2_datasaurus %>% 
    group_by(dataset) %>% 
    summarize(
      mean_x    = mean(x)
    )

print (mean_datasaurus)


## -----------------------------------------------------------------------------------------------------------
vars_datasaurus <- lab2_datasaurus %>% 
    group_by(dataset) %>% 
    summarize(
      mean_x = mean(x),
      mean_y = mean(y),
      median_y = median(y),
      var_x = var(x),
      var_y = var(y),
      cor_xy = cor(x,y)
    )

print (vars_datasaurus)


## -----------------------------------------------------------------------------------------------------------
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~ dataset) +
  scale_shape_manual(values=c(1:13)) +
  geom_point(data = vars_datasaurus, aes(x = mean_x, y = mean_y), color = "blue", size = 3)


## -----------------------------------------------------------------------------------------------------------
knitr::purl("lab2_YeajuDianaKim_r.Rmd")

