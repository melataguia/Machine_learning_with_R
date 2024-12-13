# Différentes librairies de notre travail
library('factoextra')
library('ggplot2')
library('GGally')
library('tidyverse')
library('arules')
library('arulesViz')
library('plotly')
library('MASS')
library('mlbench')
library('gam')
library('splines') 
library('e1071')
library('mlbench')
library('class')
library('sonar')


# Chargement des données Decathlon2 
data("decathlon2")
help()
data <- decathlon2
View(data)

ggpairs(data)


data2 = data[,-(11:13)]
resultat = prcomp(data2, scale. = TRUE)
str(resultat)
get_eigenvalue(resultat)
fviz_eig(resultat)
get_pca_var(resultat)
fviz_cos2(resultat, choice = "var", axes = 1)
fviz_cos2(resultat, choice = "var", axes = 2)
fviz_cos2(resultat, choice = "var", axes = 4)
fviz_pca_ind(resultat)


#Chargement du dataset Mushroom

data("Mushroom")
data3 <- Mushroom
help(Mushroom)
str(Mushroom)
itemInfo(Mushroom)


Mushroom@itemInfo
regles <- apriori(Mushroom, parameter = list(maxlen = 5, minlen = 2, support = 0.25))

str(regles)
inspect(regles)
head(regles)
regles_sort <- sort(regles, by = "lift")
head(regles_sort)


regles_sort2 <- sort(regles,
                      appearance = list(rhs = c("Class=edible","Class=poisonous")),confidence == 1,by = "lift")

plot(regles_sort2)
plot(regles_sort2,ebgine = "ploty")





# Machine learning supervisé

data("cars")
data_cars <- cars
view(data_cars)
help("cars")
summary(data_cars)

# Visualiser les données 'cars'
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  labs(
    title = "Relation entre la vitesse et la distance d'arrêt",
    x = "Vitesse (mph)",
    y = "Distance d'arrêt (ft)"
  ) +
  theme_minimal()

# Entrainement du model avec la fonction lm 

modele1<-lm(formula = dist ~ 1 + speed, data_cars)
str(modele1)
summary(modele1)

# Visualiser les données 'cars'
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_abline(intercept = modele1$coefficients[1], slope = modele1$coefficients[2], color = "blue") +
  labs(
    title = "Relation entre la vitesse et la distance d'arrêt",
    x = "Vitesse (mph)",
    y = "Distance d'arrêt (ft)"
  ) +
  theme_minimal()




# Prédictions basées sur le modèle
prediction1 <- predict(object = modele1, newdata = cars)

# Afficher les prédictions
head(prediction1)


#En utilisant modele deux pour la prédiction des donnees d'entrainement 




modele2 <- lm(formula = dist ~ 0 + speed, data_cars)
str(modele2)
summary(modele2)

# Visualiser les données 'cars' du modèle 2

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_abline(slope = modele2$coefficients[1], color = "blue") +
  labs(
    title = "Relation entre la vitesse et la distance d'arrêt MD2",
    x = "Vitesse (mph)",
    y = "Distance d'arrêt (ft)"
  ) +
  theme_minimal()

# Prédictions basées sur le modèle
prediction2 <- predict(object = modele2, newdata = cars)

# Afficher les prédictions

head(prediction2)
head(prediction1)


# Calcul de la Mean Squared Error 1 et 2
mse1 <- mean((cars$dist - prediction1)^2)

mse2 <- mean((cars$dist - prediction2)^2)

print(mse1)
print(mse2)




# En divisant le dataset en données train et test 

indice <- sample(x =1:50, size = 40 )

data_training <- cars [indice,]

data_test<- cars [-indice,]



# Entrainement du model avec la fonction lm  pour data_training

modele1<-lm(formula = dist ~ 1 + speed, data_training)
str(modele1)
summary(modele1)

# Visualiser les données 'cars'
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_abline(intercept = modele1$coefficients[1], slope = modele1$coefficients[2], color = "blue") +
  labs(
    title = "Relation entre la vitesse et la distance d'arrêt",
    x = "Vitesse (mph)",
    y = "Distance d'arrêt (ft)"
  ) +
  theme_minimal()




# Prédictions basées sur le modèle
prediction1 <- predict(object = modele1, newdata = data_test)

# Afficher les prédictions
head(prediction1)


#En utilisant modele deux pour la prédiction des donnees d'entrainement 




modele2 <- lm(formula = dist ~ 0 + speed, data_training)
str(modele2)
summary(modele2)

# Visualiser les données 'cars' du modèle 2

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_abline(slope = modele2$coefficients[1], color = "blue") +
  labs(
    title = "Relation entre la vitesse et la distance d'arrêt MD2",
    x = "Vitesse (mph)",
    y = "Distance d'arrêt (ft)"
  ) +
  theme_minimal()

# Prédictions basées sur le modèle
prediction2 <- predict(object = modele2, newdata = data_test)

# Afficher les prédictions

head(prediction2)
head(prediction1)


# Calcul de la Mean Squared Error 1 et 2
mse1 <- mean((data_test$dist - prediction1)^2)

mse2 <- mean((data_test$dist - prediction2)^2)


print(mse1)
print(mse2)

# Travail avec data hills

data("hills")
View(hills)
help(hills)
summary(hills)

# Entrainement du model 
model = lm(time ~., data = hills)
summary(model)

#modele 2

model2 = lm(time ~ . + dist*climb, data = hills)
summary(model2)









# Travail avec le dataset Pima

data("PimaIndiansDiabetes2")
help(PimaIndiansDiabetes2)
View(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)


# Division du dataset PimaIndiansDiabetes2 en données train et test 

indice1 <- sample(x =1:50, size = 70 )

data_training1 <- PimaIndiansDiabetes2 [indice1,]

data_test1<- PimaIndiansDiabetes2 [-indice1,]


# Division du dataset PimaIndiansDiabetes2 en données train et test

total_lignes <- nrow(PimaIndiansDiabetes2)


taille_train <- floor(0.7 * total_lignes)

set.seed(123) 
indice_train <- sample(1:total_lignes, size = taille_train)


data_training1 <- PimaIndiansDiabetes2[indice_train, ]
data_test1 <- PimaIndiansDiabetes2[-indice_train, ]


# entrai... du modele 

modele1<-glm(formula = diabetes ~ glucose,family = "binomial", data_training1)
str(modele1)
summary(modele1)


ggplot(data = data_training1, 
       mapping = aes(x = diabetes,
                       y = glucose) +
         geom_line()  ) 
  


# Visuali.. avec ggplot2

ggplot(data = data_training1, 
       mapping = aes(x = diabetes, y = glucose)) +
  geom_jitter() + 
  labs(
    title = "Relation entre le diabète et le glucose",
    x = "État du diabète",
    y = "Niveau de glucose"
  ) +
  theme_minimal()



# Prediction modele 1

prediction3 <- predict(object = modele1, newdata = data_test1, type = "response")

transf <- ifelse(prediction3 < 0.5,"neg","pos")

table(data_test1$diabetes,transf)


# modele 2 
modele2 <-glm(formula = diabetes ~ .,family = "binomial", data_training1)
str(modele2)
summary(modele2)

# prediction modele 2

prediction4 <- predict(object = modele2, newdata = data_test1, type = "response")

transf <- ifelse(prediction4 < 0.5,"neg","pos")

table(data_test1$diabetes,transf)







# Travail avec les données Boston 

data("Boston")
View(Boston)
help(Boston)
summary(Boston)



#division des donnnées train et test


total_lignes <- nrow(Boston)
set.seed(123)
indice_train <- sample(1:total_lignes, size = 400)


data_training2 <- Boston[indice_train, ]       
data_test2 <- Boston[-indice_train, ] 



# entrainement model 1
modele3 <- gam(medv ~ bs(lstat, degree = 2), data = data_training2)

str(modele3)
summary(modele3)



# Visualisation du spline ...
plot(modele3, se = TRUE, col = "blue", shade = TRUE, main = "Spline de degré 2 sur lstat")


# Prédictions ..de test

predictions <- predict(modele3, newdata = data_test2)


# Calculer la MSE
mse <- mean((data_test2$medv - predictions)^2)

print(mse)


# entrainement spline degre 2


modele4 <- gam(medv ~ bs(lstat, degree = 2) + bs(tax, degree = 2), data = data_training2)


summary(modele4)


# Visualisation du spline degre 2 ...
# Visualiser les splines ajustées pour lstat et tax
par(mfrow = c(1, 2))  # Mettre deux graphiques sur une seule ligne
plot(modele4, se = TRUE, col = "blue", shade = TRUE, main = "Spline de lstat")
plot(modele4, se = TRUE, col = "red", shade = TRUE, main = "Spline de tax")


# Prédictions ..de test

predictions2 <- predict(modele4, newdata = data_test2)


# Calculer la MSE
mse2 <- mean((data_test2$medv - predictions2)^2)

print(mse2)





# entrainement d'un modele de spline degré 5


modele5 <- gam(medv ~ bs(lstat, degree = 5) + bs(tax, degree = 5), data = data_training2)
summary(modele5)


# visualisation des plots degré 5
# Visualiser les splines ajustées pour lstat et tax
par(mfrow = c(1, 2))  # Mettre deux graphiques sur une seule ligne
plot(modele5, se = TRUE, col = "blue", shade = TRUE, main = "Spline de lstat (degree = 5)")
plot(modele5, se = TRUE, col = "red", shade = TRUE, main = "Spline de tax (degree = 5)")

# Prédictions ..de test

predictions3 <- predict(modele5, newdata = data_test2)


# Calculer la MSE
mse3 <- mean((data_test2$medv - predictions3)^2)

print(mse3)



# entrainement d'un modele de spline degré 30


modele6 <- gam(medv ~ bs(lstat, degree = 30) + bs(tax, degree = 30), data = data_training2)
summary(modele6)


# Prédictions ..de test

predictions4 <- predict(modele6, newdata = data_test2)


# Calculer la MSE
mse3 <- mean((data_test2$medv - predictions4)^2)

print(mse3)

degres = 2:25
DE = length(degres)
MSEs = array(NA,DE)
for (i in 1:DE){
  modele <- gam(medv ~ bs(lstat, degree = degres[i]) + bs(tax, degree = degres[i]), data = data_training2)
  predictions <- predict(modele, newdata = data_test2)
  MSEs[i] = mean((data_test2$medv - predictions)^2)
}

plot(MSEs, type = 'l')




# chargement du dataset cats

data("cats")
View(cats)
help(cats)
summary(cats)

#transformaa en datafr
cats_df <- as.data.frame(cats)
summary(cats_df)


# Visualisation
ggplot(cats_df, aes(x = Hwt, y = Bwt, color = Sex)) +
  geom_point() +
  labs(title = "Poids vs Taille des Chats", x = "Poids (Hwt)", y = "Taille (Bwt)") +
  scale_color_manual(values = c("blue", "pink"))





# division du dataset en train et test
total_lignes <- nrow(cats_df)

set.seed(123) 
indice_train <- sample(1:total_lignes, size = 100)
data_training3 <- cats_df[indice_train, ]
data_test3 <- cats_df[-indice_train, ]






# Entrainement du  modèle SVM ...
modele7 <- svm(Sex ~ Hwt + Bwt, data = data_training3, kernel = "linear", scale = FALSE)
summary(modele7)




# Visualisation du modèle
plot(modele7, data = data_training3, main = "SVM avec noyau linéaire")


# prediction ...
predictions1 <- predict(modele7, newdata = data_test3)

# matrice de confusion...
table(Predicted = predictions1, Actual = data_test3$Sex)




# Methodes locales 


data("Glass")
view(Glass)
help(Glass)
summary(Glass)



# Découper les données en training et test sets
total_lignes <- nrow(Glass)
set.seed(123)

indice_train <- sample(1:total_lignes, size = 150)


data_training5 <- Glass[indice_train, ]      
data_test5 <- Glass[-indice_train, ]      



training5_t <- data_training5[, -which(names(data_training5) == "Type")]
test5_t <- data_test5[, -which(names(data_test5) == "Type")]


#prediction 


prediction <- knn(training5_t, test5_t,cl = Glass$Type[indice_train], k = 3)

actual <- data_test5$Type
cf <- table(actual,prediction)
cf
accuracy <- sum(diag(cf))/length(actual)
sprintf("Accuracy: %.2f%%", accuracy*100)


#travail avec le data "Sonar"

data("Sonar")
view(Sonar)
help(Sonar)
summary(Sonar)

# Découpe des données en training et test sets
total_lignes <- nrow(Sonar)
set.seed(123)

indice_train <- sample(1:total_lignes, size = 150)


data_training6 <- Sonar[indice_train, ]      
data_test6 <- Sonar[-indice_train, ]  


# Entrainement du modele
