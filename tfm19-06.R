####################################################
# CLUSTERIZACIÓN Y PREDICCIÓN DE SERIES TEMPORALES #
#     APLICACIÓN AL MERCADO ELÉCTRICO EUROPEO      #
#     TFM INGENIERÍA DE SISTEMAS DE DECISIÓN       #        
#           Alicia Burgos Carrascón                #
#                  Junio 2022                      #
####################################################

#En esta hoja de R se explicará el procedimiento para la obtención de los resulados de la memoria

#En primer lugar, se toma como ruta aquella que contiene el archivo con los datos
#En mi caso, es: C:\Users\Usuario\Desktop\master\09 TFM\datostfm . Para poder ejecutar el código debe seguirse la misma ruta.
ruta=file.choose()

#Creamos un directorio con la carpeta del TFM tal y como la tenemos en el ordenador. Lo fijamos como directorio de trabajo
directorio=substr(ruta,1,nchar(ruta)-13)
setwd(directorio)

#Ahora insertamos las librerías requeridas
library(readxl) #para leer los datos
#library(imputeTS) #para imputar series temporales
library(xts) #para extender series temporales
library(dygraphs) #para representar gráficamente las series
library(TSclust) #para clusterizar series temporales
library(cluster) #necesaria para tsclust
library(pdc) # Requerida por TSclust
library(MASS) #para escalado multidimensional
library(factoextra) #para establecer el número idóneo de clusers
library(NbClust) #para establecer el número idóneo de clusers
library(e1071) #para estadísticos como svm y knn
library(forecast) #predicción de modelos estadísticos: sarima y tbats
library(tsfknn) #predicción de knn
library(rnn) #predicción de redes neuronales regresivas
library(ForecastComb) #combinación de modelos predictivos

#Incluimos en el directorio el modelo ARNN
source(paste(directorio,'ARNN.R',sep=''))

#Fijamos la semilla para poder reproducir las simulaciones
set.seed(1234)

################################
#         Funciones            #
################################

#Calcula los RSME y MAE a partir de las observaciones y predicciones
medidas_error=function(observaciones,predicciones){
  RSME=sqrt(mean((observaciones-predicciones)^2))
  MAE=mean(abs(observaciones-predicciones))
  list(RSME=RSME, MAE=MAE)
}

#Dibuja los conjuntos de train, test y la prediccion
plot_train_test_pred=function(pais,prediccion,titulo){
  train=Conjunto_train_2[,pais]
  test=Conjunto_test_2[,pais]
  prediccion=xts(prediccion,order.by=fechas_2,frequency=12)
  combinacion_series=cbind(train,test,prediccion)
  colnames(combinacion_series)=c('train','test','prediccion')
  dygraph(combinacion_series,titulo)
}

#Dibuja el ajuste del modelo dentro del conjunto de entrenamiento
plot_train_pred=function(pais,prediccion,titulo){
  train=Conjunto_train_2[,pais]
  prediccion=xts(prediccion,order.by=fechas_1,frequency=12)
  combinacion_series=cbind(train,prediccion)
  colnames(combinacion_series)=c('train','prediccion')
  dygraph(combinacion_series, titulo)
}

###########################################################
#   Carga y representación de las series temporales       #
###########################################################

#Lee los datos, que esán en la hoja y el rango de filas y columnas indicado
datos=read_excel(ruta,'Hoja2','B8:Z92')

#Define la serie temporal de los datos, que es mensual y empieza en enero 2015 
series_datos=ts(datos[,2:25],start=c(2015,1),frequency=12)

#Cantidad de datos en la serie
tam_datos<- nrow(series_datos)

#Crea la fecha de los datos originales para poder representarla
fechas=seq(as.Date("2015-01-01"),length=tam_datos,by="1 month")
series_datos_obj=xts(series_datos,order.by=fechas,frequency=12)

#Ejemplo del plot de la memoria
plot(xts(series_datos_obj[,'Chequia'],order.by=fechas,frequency=12),
     main='Precio medio del mW/h en la UE',col=1,
     xlab="Year",ylab="Euros",
     grid.col=NA,yaxis.left=TRUE,yaxis.right=FALSE)
lines(xts(series_datos_obj[,'Dinamarca']),col=2,lwd=2.0)
lines(xts(series_datos_obj[,'Alemania']),col=3,lwd=2.0)
lines(xts(series_datos_obj[,'Estonia']),col=4,lwd=2.0)
lines(xts(series_datos_obj[,'Irlanda']),col=5,lwd=2.0)
lines(xts(series_datos_obj[,'Grecia']),col=6,lwd=2.0)
lines(xts(series_datos_obj[,'España']),col=7,lwd=2.0)
lines(xts(series_datos_obj[,'Francia']),col=8,lwd=2.0)
lines(xts(series_datos_obj[,'Italia']),col="aquamarine3",lwd=2.0)
lines(xts(series_datos_obj[,'Suiza']),col="aquamarine4",lwd=2.0)
lines(xts(series_datos_obj[,'Letonia']),col="blueviolet",lwd=2.0)
lines(xts(series_datos_obj[,'Lituania']),col="darkmagenta",lwd=2.0)
lines(xts(series_datos_obj[,'Hungría']),col="hotpink",lwd=2.0)
lines(xts(series_datos_obj[,'Paises Bajos']),col="lightpink",lwd=2.0)
lines(xts(series_datos_obj[,'Austria']),col="lightsalmon",lwd=2.0)
lines(xts(series_datos_obj[,'Polonia']),col="indianred",lwd=2.0)
lines(xts(series_datos_obj[,'Portugal']),col="red4",lwd=2.0)
lines(xts(series_datos_obj[,'Rumanía']),col="tomato4",lwd=2.0)
lines(xts(series_datos_obj[,'Eslovenia']),col="violet",lwd=2.0)
lines(xts(series_datos_obj[,'Eslovaquia']),col="yellow3",lwd=2.0)
lines(xts(series_datos_obj[,'Finlandia']),col="yellow4",lwd=2.0)
lines(xts(series_datos_obj[,'Suecia']),col="yellowgreen",lwd=2.0)
lines(xts(series_datos_obj[,'Noruega']),col="skyblue3",lwd=2.0)

addLegend("topleft",on=1,
          legend.names=c("Chequia","Dinamarca","Alemania","Estonia","Irlanda","Grecia","España","Francia","Italia","Suiza","Letonia","Lituania","Hungría","Países Bajos",
                         "Austria","Polonia","Portugal","Rumanía","Eslovenia","Eslovaquia","Finlandia","Suecia","Noruega"),
          lty=c(1,1),lwd=c(2,1),
          col=c(1:8,"aquamarine3","aquamarine4","blueviolet","darkmagenta","hotpink","lightpink","lightsalmon",
                "indianred","red4","tomato4","violet","yellow3","yellow4","yellowgreen","skyblue3"),cex=0.7,pch=16)


#Copia de las series para poder estandarizarlas
series_imputadas=series_datos


###########################################################
#       Clusterización de las series temporales           #
###########################################################

#Escalamos lo primero de todo las series para evitar variaciones en las medidas de distancia
series_escaladas=scale(series_datos)

#Creamos el data frame que almacena los datos de las 4 distancias a estudiar entre elementos
mis_datos=data.frame(tipos=c('EUCL','DTW','COR','CORT')) #está compuesto por vectores de 4 componentes
tam_mis_datos=nrow(mis_datos) #tomamos como tamaño el número de filas
mis_datos$dist[[1]]=diss(series_escaladas,METHOD='EUCL') #la primera entrada es la distancia euclídea
mis_datos$dist[[2]]=diss(series_escaladas,METHOD='DTWARP')#la segunda entrada es la distancia dtw
mis_datos$dist[[3]]=diss(series_escaladas,METHOD='COR') #la tercera, la distancia de correlación
mis_datos$dist[[4]]=diss(series_escaladas,METHOD='CORT') #la cuarta, la distancia de correlación temporal

#Mostramos las distancias entre los elementos que conforman nuestros datos
for(i in 1:tam_mis_datos){
  print(paste('Distancia ',mis_datos$tipos[[i]]))
  print(as.matrix(mis_datos$dist[[i]]))
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(i)


###########################################################
#                Escalado multidimensional                #
###########################################################

#Aplicamos la técnica de escalado multidimensional al total de distancias calculadas
for (i in 1:tam_mis_datos){
  mis_datos$escalado[[i]]=resultado_Euc=isoMDS(mis_datos$dist[[i]])
}

#Creamos un pdf en el directorio de trabajo que contenga su representación gráfica
pdf(paste(directorio,"Escalado_Multidimensional.pdf", sep=''),width=8,height=8)
for (i in 1:tam_mis_datos){
  plot(mis_datos$escalado[[i]]$points[,1],
       mis_datos$escalado[[i]]$points[,2],
       main=mis_datos$tipos[i],cex=2,pch=16,bty='l',t='n')
  text(mis_datos$escalado[[i]]$points[,1],
       mis_datos$escalado[[i]]$points[,2],
       labels=rownames(mis_datos$escalado[[i]]$points),cex=.7)
}


#Eliminamos los objetos del entorno de trabajo para ganar memoria
dev.off()
rm(i)

#Creamos un png en el directorio de trabajo que contenga su representación gráfica
for (i in 1:tam_mis_datos){
  png(paste(directorio,"Escalado_Multidimensional_",mis_datos$tipos[[i]],".png",sep=''))
  plot(mis_datos$escalado[[i]]$points[,1],
       mis_datos$escalado[[i]]$points[,2],
       main=mis_datos$tipos[i],cex=2,pch=16,bty='l',t='n')
  text(mis_datos$escalado[[i]]$points[,1],
       mis_datos$escalado[[i]]$points[,2],
       labels=rownames(mis_datos$escalado[[i]]$points),cex=.7)
  dev.off()
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(i)


################################
#         Dendogramas          #
################################

#Creamos los dendogramas para cada distancia con distintos tipos de linkage
for (i in 1:tam_mis_datos){
  mis_datos$dend_sing[[i]]=hclust(mis_datos$dist[[i]],method='single') #linkage simple
  mis_datos$dend_ward[[i]]=hclust(mis_datos$dist[[i]],method='ward.D') #linkage Ward
  mis_datos$dend_aver[[i]]=hclust(mis_datos$dist[[i]],method='average') #linkage average
  mis_datos$dend_cent[[i]]=hclust(mis_datos$dist[[i]],method='centroid') #linkage de centroides
  mis_datos$dend_comp[[i]]=hclust(mis_datos$dist[[i]],method='complete') #linkage completo
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(i)

#Definimos el número de linkages
n_linkages=5

#Creamos un archivo pdf con los dendogramas y los distintos linkages. En cada página se grafica un linkage.
pdf(paste(directorio,"Dendogramas_columnas.pdf",sep=''),width=14,height=6)
par(mfrow=c(1,4)) #para dividir la imagen en 4 (distancias)
for(i in 4:(n_linkages+3)){
  for(j in 1:tam_mis_datos){
    plot(mis_datos[[j,i]],main=paste('Distancia ',mis_datos$tipos[[j]]))
  }
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
dev.off()
rm(i)

#Creamos un archivo pdf con los dendogramas y los distintos linkages. En cada página se grafica una distancia.
pdf(paste(directorio,"Dendogramas_filas.pdf",sep=''),width=14,height=6)
par(mfrow=c(1,5)) #para dividir la imagen en 5 (linkages)
for (j in 1:tam_mis_datos){
  for (i in 4:(n_linkages+3)){
    plot(mis_datos[[j,i]],main=paste('Distancia ',mis_datos$tipos[[j]]))
  }
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
dev.off()
rm(i)

#Ejemplo del plot de la memoria
pdf(paste(directorio,"Dendogramas_cort.pdf",sep=''),width=14,height=6)
par(mfrow=c(1,5))
for (i in 4:(n_linkages+3)){
  plot(mis_datos[[4,i]],main=paste('Distancia ',mis_datos$tipos[[4]]))
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
dev.off()
rm(i)

#############################################
#         Número óptimo de clusters         #
#############################################

# La cantidad óptima de centroides k a utilizar no necesariamente se conoce de antemano,
# por lo que es necesario aplicar una técnica conocida como el Método del Codo o Elbow Method a fin de determinar dicho valor. 
# Este método busca seleccionar la cantidad ideal de grupos a partir de la optimización de la WCSS (Within Clusters Summed Squares).
# Se escalan los datos primero
s=scale(datos)
#Se usa el algoritmo de k-means a fin de escoger los centroides iniciales que garantizaran la convergencia adecuada del modelo
fviz_nbclust(s,kmeans,method="wss") +
  geom_vline(xintercept=4,linetype=2)+
  labs(subtitle="Elbow method")

#Ahora generamos dendogramas con los distintos linkages, en función del número de cortes o grupos.
#En este caso, iteraremos hasta 4, que es el número óptimo obtenido con el Método del Codo.

for (i in 1:tam_mis_datos){ #Recorre las distancias
  pdf(paste(directorio,'Distancia_',mis_datos$tipos[[i]],'.pdf', sep=''),width=14,height=6)
  par(mfrow=c(1,5))
  for (corte in 2:4){ #Itera el número de grupos
    for (j in 4:(n_linkages+3)){ #Itera el número de linkages
      plot(mis_datos[[i,j]],cex=0.8)
      rect.hclust(mis_datos[[i,j]],k=corte,border=1:4)
    }
  }
  dev.off()
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(i)

#Ejemplo del plot de la memoria
pdf(paste(directorio,'Distancia_CORT_4.pdf',sep=''),width=14,height=6)
par(mfrow=c(1,5))

for (j in 4:(n_linkages+3)){#Itera el número de linkages
  plot(mis_datos[[4,j]],cex=0.8)
  rect.hclust(mis_datos[[4,j]],k=4,border=1:4)
}
dev.off()

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(j)

#Ahora, escogemos 4 como el número idóneo de clusters, basándonos en el criterio del Codo.
#En cuanto al agrupamiento, se decide elegir el completo.
#Finalmente, después de valorar todas las posibilidades, se decide tomar la distancia CORT
#pues tienen en cuenta la proximidad de las observaciones en términos de distancia y también de comportamiento.
num_clusters=4
linkage='complete'
tipo_distancia='CORT'

#Con esta elección, se crean dos objetos que almacenen la distancia y los dendogramas
distancia=diss(series_escaladas,tipo_distancia)
dend_CORT_comp=hclust(distancia,linkage)

#Dibuja el árbol de decisión incluido en la memoria
distribucion=cutree(dend_CORT_comp,num_clusters)
plot(dend_CORT_comp,labels=row.names(dend_CORT_comp),hang=-1,main="Dendograma de clusters",ylab="Altura")
rect.hclust(dend_CORT_comp,k=4,which=1:4,border=1:4,cluster=distribucion)


###########################################################
#       Elección de representantes de los clusters        #
###########################################################

#Se muestra a qué cluster pertenece cada variable
distribucion
#Se crea un vector de ceros
representantes=c(NULL)
#Se itera por cada grupo
for (i in unique(distribucion)){
  #Se toman las series a las que se les ha asignado el cluster i
  distribucion_aux=series_imputadas[,distribucion==i]
  #Si el cluster está formado por una sola serie, se elige como representante 
  if (dim(as.matrix(distribucion_aux))[2]==1){
    representantes=c(representantes,names(distribucion)[distribucion==i])
  }
  else{ 
    #Se toman los representantes basándose en su distancia a la serie media
    serie_media_aux=ts(apply(distribucion_aux,1,mean),start=c(2015,1),frequency=12)
    #Se añade este último dato y se recalcula la distancia a la serie media
    serie_aux=ts.union(distribucion_aux,serie_media_aux)
    distancia_aux=as.matrix(diss(serie_aux,METHOD='CORT'))
    #Se coge la última columna, que es la entrada que nos interesa
    num_variables=ncol(distancia_aux)
    vector_aux=distancia_aux[1:(num_variables-1),num_variables]
    #La serie representante es aquella que minimiza la distancia anterior
    representantes=c(representantes,substr(names(which.min(vector_aux)),18,28))
  }
}

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(i,distribucion_aux,serie_media_aux,distancia_aux,num_variables,vector_aux)

#Listamos los cuatro representantes
representantes


###########################################################
#      Modelos de predicción de series temporales         #
###########################################################

#Predicciones a hacer
tam_test=2

#Creamos conjunto de entrenamiento
Conjunto_train=ts(series_imputadas[1:(tam_datos-tam_test),],start=c(2015,1),frequency=12)                 

#Creamos conjunto de test
Conjunto_test=ts(series_imputadas[(tam_datos-(tam_test-1)):tam_datos,],start=c(2021,11),frequency=12)

#Extendemos la serie del entrenamiento para poder representarla
fechas_1=seq(as.Date("2015-01-01"),length=tam_datos-tam_test,by="1 month")
Conjunto_train_2=xts(series_imputadas[1:(tam_datos-tam_test),],order.by=fechas_1,frequency=12)

#Extendemos la serie del test para poder representarla
fechas_2=seq(as.Date("2021-11-01"),length=tam_test,by="1 month")
Conjunto_test_2=xts(series_imputadas[(tam_datos- (tam_test-1)):tam_datos,],order.by=fechas_2,frequency=12)

#Como solo vamos a predecir sobre las series representantes, las cogemos como conjunto de test
Conjunto_test=Conjunto_test[,representantes]

#Aplicamos los distintos modelos

#··············SARIMA......................# Paquete forecast

#Creamos vectores vacíos para almacenar el modelo, la predicción y el error
modelo_sarima=c(NULL) 
prediccion_sarima=c(NULL) 
medidas_error_sarima=c(NULL)

for (r in representantes){
  #Entrenamos el modelo
  modelo_sarima_aux=auto.arima(Conjunto_train[,r],d=NA,D=NA,max.p=5,max.q=5,max.P=2,max.Q=2,max.order=5,max.d=2,max.D=1,
                               start.p=2,start.q=2,start.P=1,start.Q=1,stepwise=TRUE,nmodels=94,trace=FALSE,method=NULL,
                               truncate=NULL,xreg=NULL,allowdrift=TRUE,allowmean=TRUE,lambda=NULL,biasadj=FALSE,parallel=FALSE,
                               num.cores=2,stationary=FALSE,seasonal=TRUE,ic=c("aicc","aic","bic"),test=c("kpss","adf","pp"),
                               test.args=list(),seasonal.test=c("seas","ocsb","hegy","ch"),seasonal.test.args=list())
  #Predecimos con el modelo
  prediccion_sarima_aux=forecast(modelo_sarima_aux,h=tam_test,level=95)
  
  #En caso de haber predicciones negativas, se cambian por cero. El precio no puede ser negativo.
  prediccion_sarima_aux$fitted[prediccion_sarima_aux$fitted <0]=0
  
  #Calculamos las medidas de error
  medidas_error_sarima_aux=medidas_error(Conjunto_test[,r],prediccion_sarima_aux$mean)
  
  #En las variables definidas al principio se encuentra la información pertinente
  modelo_sarima=c(modelo_sarima,list(modelo_sarima_aux))
  prediccion_sarima=c(prediccion_sarima,list(prediccion_sarima_aux))
  medidas_error_sarima=c(medidas_error_sarima,list(medidas_error_sarima_aux))
}


#Etiquetamos los resultados con los nombres de los representantes
names(modelo_sarima)=representantes
names(prediccion_sarima)=representantes
names(medidas_error_sarima)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,modelo_sarima_aux,prediccion_sarima_aux,medidas_error_sarima_aux)

#Representación gráfica del ajuste del modelo dentro del conjunto de entrenamiento
plot_train_pred('Eslovenia',modelo_sarima[['Eslovenia']]$fitted,"SARIMA (Eslovenia)")
plot_train_pred('Estonia',modelo_sarima[['Estonia']]$fitted,"SARIMA (Estonia)")
plot_train_pred('España',modelo_sarima[['España']]$fitted,"SARIMA (España)")
plot_train_pred('Suecia',modelo_sarima[['Suecia']]$fitted,"SARIMA (Suecia)")

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_sarima[['Eslovenia']]$mean,"SARIMA (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_sarima[['Estonia']]$mean,"SARIMA (Estonia)")
plot_train_test_pred('España',prediccion_sarima[['España']]$mean,"SARIMA (España)")
plot_train_test_pred('Suecia',prediccion_sarima[['Suecia']]$mean,"SARIMA (Suecia)")


#··············TBATS......................# Paquete forecast

#Creamos vectores vacíos para almacenar el modelo, la predicción y el error
modelo_tbats=c(NULL) 
prediccion_tbats=c(NULL) 
medidas_error_tbats=c(NULL) 

for (r in representantes){
  #Entrenamos el modelo
  modelo_tbats_aux=tbats(Conjunto_train[,r])
  #Predecimos con el modelo
  prediccion_tbats_aux=forecast(modelo_tbats_aux,h=tam_test,level=95)
  
  #En caso de haber predicciones negativas, se cambian por cero. El precio no puede ser negativo.
  prediccion_tbats_aux$fitted[prediccion_tbats_aux$fitted <0]=0
  
  #Calculamos las medidas de error
  medidas_error_tbats_aux=medidas_error(Conjunto_test[,r],prediccion_tbats_aux$mean)
  
  #En las variables definidas al principio se encuentra la información pertinente
  modelo_tbats=c(modelo_tbats,list(modelo_tbats_aux))
  prediccion_tbats=c(prediccion_tbats,list(prediccion_tbats_aux))
  medidas_error_tbats=c(medidas_error_tbats,list(medidas_error_tbats_aux))
}

#Etiquetamos los resultados con los nombres de los representantes
names(modelo_tbats)=representantes
names(prediccion_tbats)=representantes
names(medidas_error_tbats)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,modelo_tbats_aux,prediccion_tbats_aux,medidas_error_tbats_aux)

#Representación gráfica del ajuste del modelo dentro del conjunto de entrenamiento
plot_train_pred('Eslovenia',modelo_tbats[['Eslovenia']]$fitted.values,"TBATS (Eslovenia)")
plot_train_pred('Estonia',modelo_tbats[['Estonia']]$fitted.values,"TBATS (Estonia)")
plot_train_pred('España',modelo_tbats[['España']]$fitted.values,"TBATS (España)")
plot_train_pred('Suecia',modelo_tbats[['Suecia']]$fitted.values,"TBATS (Suecia)")

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_tbats[['Eslovenia']]$mean,"TBATS (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_tbats[['Estonia']]$mean,"TBATS (Estonia)")
plot_train_test_pred('España',prediccion_tbats[['España']]$mean,"TBATS (España)")
plot_train_test_pred('Suecia',prediccion_tbats[['Suecia']]$mean,"TBATS (Suecia)")


#··············ARNN......................# Paquete rnn

#Creamos vectores vacíos para almacenar el modelo, la predicción y el error
modelo_arnn=c(NULL)
prediccion_arnn=c(NULL)
medidas_error_arnn=c(NULL)

for (r in representantes){
  #Entrenamos el modelo
  modelo_arnn_aux=arnn(x=Conjunto_train[,r],H=2,lags=1:tam_test)
  
  #Predecimos con el modelo
  prediccion_arnn_aux=forecast(modelo_arnn_aux,h=tam_test,level=90)
  
  #En caso de haber predicciones negativas, se cambian por cero. El precio no puede ser negativo.
  prediccion_arnn_aux$mean[prediccion_arnn_aux$mean < 0]=0
  
  #Calculamos las medidas de error
  medidas_error_arnn_aux=medidas_error(Conjunto_test[,r],prediccion_arnn_aux$mean)
  
  #En las variables definidas al principio se encuentra la información pertinente
  modelo_arnn=c(modelo_arnn,list(modelo_arnn_aux))
  prediccion_arnn=c(prediccion_arnn,list(prediccion_arnn_aux))
  medidas_error_arnn=c(medidas_error_arnn,list(medidas_error_arnn_aux))
}

#Etiquetamos los resultados con los nombres de los representantes
names(modelo_arnn)=representantes
names(prediccion_arnn)=representantes
names(medidas_error_arnn)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,modelo_arnn_aux,prediccion_arnn_aux,medidas_error_arnn_aux)

#Se añaden los 2 retardos correspondientes para la predicción de las variables explicativas
for (r in representantes){
  modelo_arnn[[r]]$fitted=ts(c(NaN,NaN,modelo_arnn[[r]]$fitted),start=c(2015,1),frequency=12)
}

#Representación gráfica del ajuste del modelo dentro del conjunto de entrenamiento
plot_train_pred('Eslovenia',modelo_arnn[['Eslovenia']]$fitted,"ARNN (Eslovenia)")
plot_train_pred('Estonia',modelo_arnn[['Estonia']]$fitted,"ARNN (Estonia)")
plot_train_pred('España',modelo_arnn[['España']]$fitted,"ARNN (España)")
plot_train_pred('Suecia',modelo_arnn[['Suecia']]$fitted,"ARNN (Suecia)")

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_arnn[['Eslovenia']]$mean,"ARNN (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_arnn[['Estonia']]$mean,"ARNN (Estonia)")
plot_train_test_pred('España',prediccion_arnn[['España']]$mean,"ARNN (España)")
plot_train_test_pred('Suecia',prediccion_arnn[['Suecia']]$mean,"ARNN (Suecia)")


#··············KNN......................# Paquete tsfknn

#ESTRATEGIA RECURSIVA

#Creamos vectores vacíos para almacenar el modelo, la predicción y el error
modelo_knn_rec=c(NULL) 
prediccion_knn_rec=c(NULL) 
medidas_error_knn_rec=c(NULL) 

for (r in representantes){
  #Entrenamos el modelo
  modelo_knn_rec_aux=knn_forecasting(Conjunto_train[,r],h=tam_test,lags=1:tam_test,k=9,msas="recursive")
  
  #Predecimos con el modelo
  prediccion_knn_rec_aux=modelo_knn_rec_aux$prediction
  
  #En caso de haber predicciones negativas, se cambian por cero. El precio no puede ser negativo.
  prediccion_knn_rec_aux[prediccion_knn_rec_aux < 0]=0
  
  #Calculamos las medidas de error
  medidas_error_knn_rec_aux=medidas_error(Conjunto_test[,r],prediccion_knn_rec_aux)
  
  #En las variables definidas al principio se encuentra la información pertinente
  modelo_knn_rec=c(modelo_knn_rec,list(modelo_knn_rec_aux))
  prediccion_knn_rec=c(prediccion_knn_rec,list(prediccion_knn_rec_aux))
  medidas_error_knn_rec=c(medidas_error_knn_rec,list(medidas_error_knn_rec_aux))
}

#Etiquetamos los resultados con los nombres de los representantes
names(modelo_knn_rec)=representantes
names(prediccion_knn_rec)=representantes
names(medidas_error_knn_rec)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,modelo_knn_rec_aux,prediccion_knn_rec_aux,medidas_error_knn_rec_aux)

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_knn_rec[['Eslovenia']],"KNN Recursivo (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_knn_rec[['Estonia']],"KNN Recursivo (Estonia)")
plot_train_test_pred('España',prediccion_knn_rec[['España']],"KNN Recursivo (España)")
plot_train_test_pred('Suecia',prediccion_knn_rec[['Suecia']],"KNN Recursivo (Suecia)")


#ESTRATEGIA MIMO

#Creamos vectores vacíos para almacenar el modelo, la predicción y el error
modelo_knn_mimo=c(NULL) 
prediccion_knn_mimo=c(NULL) 
medidas_error_knn_mimo=c(NULL)  

for (r in representantes){
  #Entrenamos el modelo
  modelo_knn_mimo_aux=knn_forecasting(Conjunto_train[,r],h=tam_test,lags=1:tam_test,k=9,msas="MIMO")
  
  #Predecimos con el modelo
  prediccion_knn_mimo_aux=modelo_knn_mimo_aux$prediction
  
  #En caso de haber predicciones negativas, se cambian por cero. El precio no puede ser negativo.
  prediccion_knn_mimo_aux[prediccion_knn_mimo_aux < 0]=0
  
  #Calculamos las medidas de error
  medidas_error_knn_mimo_aux=medidas_error(Conjunto_test[,r],prediccion_knn_mimo_aux)
  
  #En las variables definidas al principio se encuentra la información pertinente
  modelo_knn_mimo=c(modelo_knn_mimo,list(modelo_knn_mimo_aux))
  prediccion_knn_mimo=c(prediccion_knn_mimo,list(prediccion_knn_mimo_aux))
  medidas_error_knn_mimo=c(medidas_error_knn_mimo,list(medidas_error_knn_mimo_aux))
}

#Etiquetamos los resultados con los nombres de los representantes
names(modelo_knn_mimo)=representantes
names(prediccion_knn_mimo)=representantes
names(medidas_error_knn_mimo)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,modelo_knn_mimo_aux,prediccion_knn_mimo_aux,medidas_error_knn_mimo_aux)

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_knn_mimo[['Eslovenia']],"k-NN MIMO (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_knn_mimo[['Estonia']],"k-NN MIMO (Estonia)")
plot_train_test_pred('España',prediccion_knn_mimo[['España']],"k-NN MIMO (España)")
plot_train_test_pred('Suecia',prediccion_knn_mimo[['Suecia']],"k-NN MIMO (Suecia)")


#··············SVM......................# Paquete e1071

#En primer lugar, definimos las variables dummy, que actuarán como predictores. Son vectores de tamaño 84.
#Hemos creado doce variables mensuales, con un 1 en la entrada del mes que les corresponde y ceros en el resto.
#Las definidos como un vector de longitud 12 que se repite 7 veces (años en el estudio), dando en total 84 observaciones, que es el número de train.
#Por otro lado, hemos incluido una dummy para el cambio de tendencia, que se observa en los 5 últimos meses de 2021,
#y otras dos dummy para los cuatrimestres que conforman el verano y el invierno, por si su estacionalidad pudiese influir.

cambio_tendencia=c(rep(c(0),times=79),rep(c(1),times=5))
verano=c(rep(c(0,0,0,0,0,1,1,1,1,0,0,0),times=84/12))
invierno=c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1),times=84/12))
enero=c(rep(c(1,0,0,0,0,0,0,0,0,0,0,0),times=84/12))
febrero=c(rep(c(0,1,0,0,0,0,0,0,0,0,0,0),times=84/12))
marzo=c(rep(c(0,0,1,0,0,0,0,0,0,0,0,0),times=84/12))
abril=c(rep(c(0,0,0,1,0,0,0,0,0,0,0,0),times=84/12))
mayo=c(rep(c(0,0,0,0,1,0,0,0,0,0,0,0),times=84/12))
junio=c(rep(c(0,0,0,0,0,1,0,0,0,0,0,0),times=84/12))
julio=c(rep(c(0,0,0,0,0,0,1,0,0,0,0,0),times=84/12))
agosto=c(rep(c(0,0,0,0,0,0,0,1,0,0,0,0),times=84/12))
septiembre=c(rep(c(0,0,0,0,0,0,0,0,1,0,0,0),times=84/12))
octubre=c(rep(c(0,0,0,0,0,0,0,0,0,1,0,0),times=84/12))
noviembre=c(rep(c(0,0,0,0,0,0,0,0,0,0,1,0),times=84/12))
diciembre=c(rep(c(0,0,0,0,0,0,0,0,0,0,0,1),times=84/12))

#Agrupamos las dummy en un dataframe
dummies=data.frame(cambio_tendencia,verano,invierno,enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre)

#Creamos vectores vacíos para almacenar el modelo, la predicción y el error
modelo_svm=c(NULL) 
prediccion_svm=c(NULL) 
medidas_error_svm=c(NULL)

for (r in representantes){
  Prod=series_imputadas[,r]
  Tiempo=1:length(Prod)
  DF=dummies
  DF$Prod=Prod
  DF$Tiempo=Tiempo
  DF$Lag1Prod=c(NA,Prod[1:(tam_datos-1)])
  DF$Med= c(NA,rollmean(Prod,k=2,align='center'))
  DF_train=DF[1:(tam_datos-tam_test),]
  DF_test=DF[(tam_datos- (tam_test-1)):tam_datos,]
  
  #Entrenamos el modelo. Se toma el kernel lineal porque es el que mejor resultados poroporciona
  modelo_svm_aux=svm(Prod ~.,data=DF_train,type='eps-regression',kernel='linear',cost=100)
  
  #Añadimos las predicciones a los datos para poder representarlo
  modelo_svm_aux[[r]]$fitted=ts(c(NaN,modelo_svm_aux[[r]]$fitted),start=c(2015,1),frequency=12)
  
  #Predecimos con el modelo
  prediccion_svm_aux=predict(modelo_svm_aux,newdata=DF_test)
  
  #En caso de haber predicciones negativas, se cambian por cero. El precio no puede ser negativo.
  prediccion_svm_aux[prediccion_svm_aux < 0]=0
  
  #Calculamos las medidas de error
  medidas_error_svm_aux=medidas_error(Conjunto_test[,r],prediccion_svm_aux)
  
  #En las variables definidas al principio se encuentra la información pertinente
  modelo_svm=c(modelo_svm,list(modelo_svm_aux))
  prediccion_svm=c(prediccion_svm,list(prediccion_svm_aux))
  medidas_error_svm=c(medidas_error_svm,list(medidas_error_svm_aux))
}

#Etiquetamos los resultados con los nombres de los representantes
names(modelo_svm)=representantes
names(prediccion_svm)=representantes
names(medidas_error_svm)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,Prod,Tiempo,DF,DF_train,DF_test,modelo_svm_aux,prediccion_svm_aux,medidas_error_svm_aux)

#Se añaden 1 lag para evitar errores en el plot
for (r in representantes){
  modelo_svm[[r]]$fitted=ts(c(NaN,modelo_svm[[r]]$fitted),start=c(2015,1),frequency=12)
}

#Representación gráfica del ajuste del modelo dentro del conjunto de entrenamiento
plot_train_pred('Eslovenia',modelo_svm[['Eslovenia']]$fitted,"SVM (Eslovenia)")
plot_train_pred('Estonia',modelo_svm[['Estonia']]$fitted,"SVM (Estonia)")
plot_train_pred('España',modelo_svm[['España']]$fitted,"SVM (España)")
plot_train_pred('Suecia',modelo_svm[['Suecia']]$fitted,"SVM (Suecia)")

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_svm[['Eslovenia']],"SVM (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_svm[['Estonia']],"SVM (Estonia)")
plot_train_test_pred('España',prediccion_svm[['España']],"SVM (España)")
plot_train_test_pred('Suecia',prediccion_svm[['Suecia']],"SVM (Suecia)")



#··············MODELO COMBINADO......................# Paquete ForecastComb

#Definen las estructuras para su almacenamiento en memoria
prediccion=c(NULL)
prediccion_estruct=c(NULL) #Detalla cuáles de las predicciones va quitando de la matriz final

for (r in representantes){ #Agrupamos en un dataframe todos los modelos
  prediccion_aux=data.frame(prediccion_sarima[[r]]$mean,prediccion_tbats[[r]]$mean,prediccion_arnn[[r]]$mean,
                            modelo_knn_mimo[[r]]$prediction,modelo_knn_rec[[r]]$prediction,prediccion_svm[[r]])
  
  prediccion_aux=ts(prediccion_aux,start=c(2021,11),frequency=12) #Mi preducción empieza en Noviembre de 2021 
  
  #Definimos la estructura de forma correcta para aplicar la combinación
  prediccion_estruct_aux=list(foreccomb(Conjunto_test[,r],prediccion_aux))
  
  #En las variables definidas al principio se encuentra la información pertinente
  prediccion=c(prediccion,list(prediccion_aux))
  prediccion_estruct=c(prediccion_estruct,prediccion_estruct_aux)
}

#Etiquetamos los resultados con los nombres de los representantes
names(prediccion)=representantes
names(prediccion_estruct)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,prediccion_aux,prediccion_estruct_aux)


#··············MEDIA SA......................#

#Definen las estructuras para su almacenamiento en memoria
prediccion_SA=c(NULL) 
medidas_error_SA=c(NULL) 

for (r in representantes){
  prediccion_SA_aux=comb_SA(prediccion_estruct[[r]]) #Combinamos las predicciones
  medidas_error_SA_aux=medidas_error(Conjunto_test[,r],prediccion_SA_aux$Fitted) #Calculamos sus errores
  
  #En las variables definidas al principio se encuentra la información pertinente
  prediccion_SA=c(prediccion_SA,list(prediccion_SA_aux))
  medidas_error_SA=c(medidas_error_SA,list(medidas_error_SA_aux))
}

#Etiquetamos los resultados con los nombres de los representantes
names(prediccion_SA)=representantes
names(medidas_error_SA)=representantes


#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,prediccion_SA_aux,medidas_error_SA_aux)

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_SA[['Eslovenia']]$Fitted,"Media SA (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_SA[['Estonia']]$Fitted,"Media SA (Estonia)")
plot_train_test_pred('España',prediccion_SA[['España']]$Fitted,"Media SA (España)")
plot_train_test_pred('Suecia',prediccion_SA[['Suecia']]$Fitted,"Media SA (Suecia)")


#··············MEDIA BG......................#

#Definen las estructuras para su almacenamiento en memoria
prediccion_med_var=c(NULL) 
medidas_error_med_var=c(NULL) 


for (r in representantes){
  prediccion_BG=comb_BG(prediccion_estruct[[r]]) #Combinamos las predicciones
  medidas_error_BG=medidas_error(Conjunto_test[,r],prediccion_BG$Fitted) #Calculamos sus errores
  
  #En las variables definidas al principio se encuentra la información pertinente
  prediccion_med_var=c(prediccion_med_var,list(prediccion_BG))
  medidas_error_med_var=c(medidas_error_med_var,list(medidas_error_BG))
}

#Etiquetamos los resultados con los nombres de los representantes
names(prediccion_med_var)=representantes
names(medidas_error_med_var)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,prediccion_BG,medidas_error_BG)

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_med_var[['Eslovenia']]$Fitted,"Media BG (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_med_var[['Estonia']]$Fitted,"Media BG (Estonia)")
plot_train_test_pred('España',prediccion_med_var[['España']]$Fitted,"Media BG (España)")
plot_train_test_pred('Suecia',prediccion_med_var[['Suecia']]$Fitted,"Media BG (Suecia)")


#··············MEDIA CLS......................#

#Definen las estructuras para su almacenamiento en memoria
prediccion_CLS=c(NULL) 
medidas_error_CLS=c(NULL) 

for (r in representantes){
  prediccion_CLS_aux=comb_CLS(prediccion_estruct[[r]]) #Combinamos las predicciones
  medidas_error_CLS_aux=medidas_error(Conjunto_test[,r],prediccion_CLS_aux$Fitted) #Calculamos sus errores
  
  #En las variables definidas al principio se encuentra la información pertinente
  prediccion_CLS=c(prediccion_CLS,list(prediccion_CLS_aux))
  medidas_error_CLS=c(medidas_error_CLS,list(medidas_error_CLS_aux))
}

#Etiquetamos los resultados con los nombres de los representantes
names(prediccion_CLS)=representantes
names(medidas_error_CLS)=representantes

#Eliminamos los objetos del entorno de trabajo para ganar memoria
rm(r,prediccion_CLS_aux,medidas_error_CLS_aux)

#Representación gráfica de la predicción dentro del conjunto de test
plot_train_test_pred('Eslovenia',prediccion_CLS[['Eslovenia']]$Fitted,"Media CLS (Eslovenia)")
plot_train_test_pred('Estonia',prediccion_CLS[['Estonia']]$Fitted,"Media CLS (Estonia)")
plot_train_test_pred('España',prediccion_CLS[['España']]$Fitted,"Media CLS (España)")
plot_train_test_pred('Suecia',prediccion_CLS[['Suecia']]$Fitted,"Media CLS (Suecia)")

#Se ejecuta esta línea para ver la estructura de foreccomb que queda al final
prediccion_estruct

