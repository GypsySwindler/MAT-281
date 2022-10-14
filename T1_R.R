#install.packages("naivebayes")
library ("naivebayes")



### 1.C ###

datos = read.table ( file.choose() , header = FALSE , sep =",")
colnames(datos)<-c("LongitudSepalo","AnchoSepalo","LongitudPetalo","AnchoPetalo","Etiqueta")
datos
datos$Etiqueta<-as.factor(datos$Etiqueta)
datos
attach(datos)


modelo<-naive_bayes(Etiqueta ~
                    LongitudSepalo +
                    AnchoSepalo +
                    LongitudPetalo + 
                    AnchoPetalo,
              data=datos,usekernel=T)

par ( mfrow =c(1 ,4) )
plot ( modelo ) ;
cor(datos[,c(1,2,3,4)])



### 1.D ###

predict ( modelo , 
          newdata = data.frame (   LongitudSepalo  = 3 , 
                                   AnchoSepalo     = 2 , 
                                   LongitudPetalo  = 3 , 
                                   AnchoPetalo     = 2)
          , type = "prob")




### 1.E ###
library (class)

datos = read.table ( file.choose() , header = FALSE , sep =",")
colnames(datos)<-c("LongitudSepalo","AnchoSepalo","LongitudPetalo","AnchoPetalo","Etiqueta")
datos$Etiqueta<-as.factor(datos$Etiqueta)
attach(datos)

 knn ( train = cbind (LongitudSepalo, AnchoSepalo, LongitudPetalo, AnchoPetalo) , 
               test = cbind (3,2,3,2) ,
               cl = Etiqueta ,
               k =14 ,
               prob =T)

 
 
 ### 2.B ###
 
 library ("rpart")
 library ("rpart.plot")
 
 
 datos = read.table ( file.choose() , header = FALSE , sep =",")
 colnames(datos)<-c("Inglés","Instructor","Curso","Semestre", "Estudiantes", "Etiqueta")
 datos$Etiqueta[datos$Etiqueta==2]<-0
 datos$Etiqueta[datos$Etiqueta==3]<-0
 datos
 attach(datos)
 
 
 
 ### 2.c ###
 
 modelo <- rpart ( Etiqueta ~ 
                     Inglés +
                     Instructor +
                     Curso +
                     Semestre +
                     Estudiantes , 
                   data = datos )
 rpart.plot ( modelo )
 
 