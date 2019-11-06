# génération aléatoire des données à classifier
# avec une première moitié dans une classe et la seconde dans une autre classe
library(class)
# nn = 50
# x <- rbind(matrix(rnorm(2*nn, mean = 0.5, sd = 0.3), ncol = 2),
#            matrix(rnorm(2*nn, mean = 1, sd = 0.3), ncol = 2))
# colnames(x) <- c("x", "y")
# z <- rbind(matrix(1,nn, 1), matrix(2,nn, 1))
# colnames(z) <- c("z")
# 
# donnees <- cbind(x,z)
# donnees
# 
# plot(donnees[,1:2], col = z)
library(ggplot2)
library(stats)
library(graphics)
library(cluster)
library(fpc)

data_ini <- iris

scatter <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species)) +
  theme_bw()+
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

donnees <- iris 

# on prépare un sosu-ensemble d'apprentissae et un autre de test
n <- nrow(donnees)

I <- sample(1:n,(2*n)/3)
J <- setdiff(1:n,I)

# on prépare les données : on construit le classifieur K-NN
# pour les valeurs numérique et on extrait explicitement la classe 
# à predire

cl <- donnees[I,5]

dlrn <- donnees[I,1:4]
dtest <- donnees[J,1:4]

library (class)

mknn1 <- knn(dlrn, dtest,cl, k=1)
mknn1
t<- table(mknn1, donnees[J,5])


mknn3 <- knn(dlrn, dtest,cl, k=3)
mknn3
table(mknn3, donnees[J,5])

mknn7 <- knn(dlrn, dtest,cl, k=7)
mknn7
table(mknn7, donnees[J,5])

mknn11 <- knn(dlrn, dtest,cl, k=11)
mknn11
table(mknn11, donnees[J,5])
errmin <-100
k_final <- 0
for (k in 1:5){
  kmin<- (k-1)*2+1
  mknn <- knn(dlrn, dtest,cl, k=kmin)
  tablec<- table(mknn, donnees[J,5])
  err<- sum(tablec[1,2],tablec[1,3],tablec[2,1], tablec[2,3],tablec[3,1], tablec[3,2])
  if (err< errmin){
    errmin<- err
    k_final <- kmin
  }
  
}


# validation croisée

train <- donnees[,1:4]
cl <- donnees[,5]
model <- knn.cv(train,cl,k=k_final)
model
table(cl,model)


library(caret)
confusionMatrix(cl,model)



########## 

library(cluster.datasets)
library(stringr)
library(dict)
library(hash)

dataSet <- mammal.dentition
n_row <- nrow(dataSet)



dentition_classes <- c()
hash()

dentition_classes_dict <- hash()


for (i in 0:n_row) {
  values <- tolower(unlist(strsplit(dataSet[i,1], " ")))
  if (length(values) == 1) {
    if (is.element(values, dentition_classes) == FALSE){
      dentition_classes <- c(dentition_classes, values)
      .set(dentition_classes_dict, keys=values, values=1)
    } else {
      .set(dentition_classes_dict, keys=values, values= (dentition_classes_dict[[values]] + 1))
      }
  } else {
    if(length(values) >= 2) {
      classe <- values[length(values)]
      if (is.element(classe, dentition_classes) == FALSE){
        dentition_classes <- c(dentition_classes, classe)
        #dentition_classes_dict[classe] <- as.numeric(0)
        .set(dentition_classes_dict, keys=classe, values=1)
      } else {
        .set(dentition_classes_dict, keys=classe, values= (dentition_classes_dict[[classe]] + 1))
        }
      }
    } 
  }


View(dentition_classes_dict)

## méthode LVQ (Learning Vector Quantitation ) avec une classification par prototypes
cl <- as.factor(cl)
cd <- lvqinit(train, cl, 10)

lv1 <- lvqtest(cd, train)
confusionMatrix(cl,lv1)

cd0 <- olvq1(train, cl, cd)
lv2 <- lvqtest(cd0, train)
confusionMatrix(cl,lv2)

cd1 <- lvq1(train, cl, cd0)
lv3 <- lvqtest(cd1, train)
confusionMatrix(cl,lv3)

