library(tidyverse)
library(modelr)
library(scatterplot3d)
library(readxl)
library(ggplot2)
library(tidyr)
library(modelr)
library(caret)
library(rsample)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(dplyr)
library(ipred)

PriceHousingMiami<- read_csv("D:/FACULTATE/FSEGA/FSEGA AN 3/SEMESTRUL 2/FUNDAMENTE DE BIG DATA/Proiect2/miami-housing.csv")

View(PriceHousingMiami)
ncol(PriceHousingMiami)

PriceHousingMiami <- select(PriceHousingMiami, -LATITUDE)
PriceHousingMiami <- select(PriceHousingMiami, -LONGITUDE)
PriceHousingMiami <- select(PriceHousingMiami, -WATER_DIST)
PriceHousingMiami <- select(PriceHousingMiami, -SUBCNTR_DI)
PriceHousingMiami <- select(PriceHousingMiami, -avno60plus)
PriceHousingMiami <- select(PriceHousingMiami, -month_sold)

names(PriceHousingMiami)[1] <- 'ID'
names(PriceHousingMiami)[2] <- 'Pret_vanzare'
names(PriceHousingMiami)[3] <- 'Suprafata_teren'
names(PriceHousingMiami)[4] <- 'Suprafata_casa'
names(PriceHousingMiami)[5] <- 'Valoare_bunuri_lux'
names(PriceHousingMiami)[6] <- 'Distanta_cale_ferata'
names(PriceHousingMiami)[7] <- 'Distanta_ocean'
names(PriceHousingMiami)[8] <- 'Distanta_centru'
names(PriceHousingMiami)[9] <- 'Distanta_autostrada'
names(PriceHousingMiami)[10] <- 'Vechime_constructie'
names(PriceHousingMiami)[11] <- 'Calitatea_structurii'

class(PriceHousingMiami$ID)
class(PriceHousingMiami$Pret_vanzare)
class(PriceHousingMiami$Suprafata_teren)
class(PriceHousingMiami$Suprafata_casa)
class(PriceHousingMiami$Valoare_bunuri_lux)
class(PriceHousingMiami$Distanta_cale_ferata)
class(PriceHousingMiami$Distanta_ocean)
class(PriceHousingMiami$Distanta_centru)
class(PriceHousingMiami$Distanta_autostrada)
class(PriceHousingMiami$Vechime_constructie)
class(PriceHousingMiami$Calitatea_structurii)

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Distanta_ocean, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Distanta_centru, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Vechime_constructie, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Suprafata_casa, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Distanta_autostrada, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Calitatea_structurii, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Valoare_bunuri_lux, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Distanta_cale_ferata, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

PriceHousingMiami %>%
  ggplot(mapping = aes(x = Suprafata_teren, y = Pret_vanzare)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))


model_pret_distanta<-lm(data=PriceHousingMiami, Pret_vanzare~Distanta_autostrada)
summary(model_pret_distanta)

sd(model_pret_distanta$residuals)

R_squared = (var(PriceHousingMiami$Pret_vanzare)-var(model_pret_distanta$residuals))/var(PriceHousingMiami$Pret_vanzare)
R_squared

confint(model_pret_distanta)

grid_distanta <- PriceHousingMiami %>%
data_grid(Distanta_autostrada = seq_range(Distanta_autostrada, 100)) %>%  
add_predictions(model_pret_distanta, "Pret_vanzare")
ggplot(PriceHousingMiami, aes(Distanta_autostrada, Pret_vanzare)) + geom_point(color = "chartreuse4") + geom_line(data = grid_distanta, color = "black", size = 1) 

model_pret_teren<-lm(data=PriceHousingMiami, Pret_vanzare~Suprafata_teren)
summary(model_pret_teren)

sd(model_pret_teren$residuals)

R_squared = (var(PriceHousingMiami$Pret_vanzare)-var(model_pret_teren$residuals))/var(PriceHousingMiami$Pret_vanzare)
R_squared

confint(model_pret_teren)

grid_suprafata <- PriceHousingMiami %>%
data_grid(Suprafata_teren = seq_range(Suprafata_teren, 100)) %>%  
add_predictions(model_pret_teren, "Pret_vanzare")
ggplot(PriceHousingMiami, aes(Suprafata_teren, Pret_vanzare)) + geom_point(color = "chartreuse4") + geom_line(data = grid_suprafata, color = "black", size = 1) 

model_pret_bunuri<-lm(data=PriceHousingMiami, Pret_vanzare~Valoare_bunuri_lux)
summary(model_pret_bunuri)

sd(model_pret_bunuri$residuals)

R_squared = (var(PriceHousingMiami$Pret_vanzare)-var(model_pret_bunuri$residuals))/var(PriceHousingMiami$Pret_vanzare)
R_squared

confint(model_pret_bunuri)

grid_bunuri <- PriceHousingMiami %>%
  data_grid(Valoare_bunuri_lux = seq_range(Valoare_bunuri_lux, 100)) %>%  
  add_predictions(model_pret_bunuri, "Pret_vanzare")
ggplot(PriceHousingMiami, aes(Valoare_bunuri_lux, Pret_vanzare)) + geom_point(color = "chartreuse4") + geom_line(data = grid_bunuri, color = "black", size = 1) 

model_pret_suprafata_bunuri <- lm(data = PriceHousingMiami, Pret_vanzare ~ Valoare_bunuri_lux +Suprafata_teren)
summary(model_pret_suprafata_bunuri)

s3d <- scatterplot3d(PriceHousingMiami$Valoare_bunuri_lux, PriceHousingMiami$Suprafata_teren, PriceHousingMiami$Pret_vanzare, type="p") 
s3d$plane3d(model_pret_suprafata_bunuri, lty.box = "solid")

set.seed(123)
price_split <- initial_split(PriceHousingMiami, prop = 0.7)
price_train <- training(price_split)
price_test  <- testing(price_split)

m1 <- rpart(
  formula = Pret_vanzare ~ .,
  data = price_train,
  method = "anova"
)

m1
rpart.plot(m1)
plotcp(m1)
m1$cptable

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)
head(hyper_grid,32)

models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = Pret_vanzare ~. ,
    data = price_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )  
mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_tree <- rpart(
  formula = Pret_vanzare ~ .,
  data = price_train,
  method = "anova",
  control = list(minsplit = 7, maxdepth = 14, cp = 0.01000000)
)

pred <- predict(m1, newdata = price_test)
RMSE(pred = pred, obs = price_test$Pret_vanzare)
optimal_tree

rpart.plot(optimal_tree)

pret_split <- initial_split(PriceHousingMiami, prop = 0.7, strata = "Pret_vanzare")
pret_train <- training(pret_split)

set.seed(123)
pret_rf <- randomForest(
  formula = Pret_vanzare ~ .,
  data = pret_train
)

pret_rf
plot(pret_rf)
