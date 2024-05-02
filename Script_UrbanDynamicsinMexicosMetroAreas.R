##Urban Dynamics in Mexico’s Metropolitan Areas: 
#Assessing the Patterns, Trends and Drivers of Socio-Spatial Segregation 
  #Marco Cruz-Sandoval1,*, Gonzalo Peraza-Mues1,2, Fernando Gómez-Zaldívar2, Roberto Ponce-López1,2*
  #1 Center for the Future of Cities, Tecnologico de Monterrey, Monterrey, Mexico. 
  #2 School of Government and Public Transformation, Tecnologico de Monterrey, Monterrey, Mexico. 
  #*Correspondence:  rpl@tec.mx

ls()
rm(list=ls())
library(dplyr)
library(tidyr) 
library(ggplot2)
library(NbClust)
library(factoextra)
library(jtools)
library(sandwich)
library(patchwork)


#Kmeans clustering-----

Ckm1<- read.csv("CondensadoTP.csv",TRUE,",")

Ckm2 <- scale(Ckm1[,4:12])
Ckm2

nuevoCKM <- data.frame(Ckm1[,1:3],Ckm2)
nuevoCKM 

m.distancia <- get_dist(nuevoCKM,method = "euclidean")
fviz_dist(m.distancia,gradient=list(low="blue",mid="white",high="red"))

fviz_nbclust(nuevoCKM[,4:12],kmeans,method="wss")
fviz_nbclust(nuevoCKM[,4:12],kmeans,method="silhouette")
fviz_nbclust(nuevoCKM[,4:12],kmeans,method="gap_stat")
resnumclust <- NbClust(nuevoCKM[,4:12],distance="euclidean",min.nc = 2,max.nc = 10,method = "kmeans",index = "alllong")
resnumclust
print(resnumclust$Best.nc)

#Number of clusters
k2 <- kmeans(Ckm2,centers = 5,nstart=100)
k2

str(k2)
k2$cluster

nuevoCKM <- data.frame(Ckm1[,1:3],Ckm2)
nuevoCKM 

#plotear los clusters
par(mfrow=c(1,1))

fviz_cluster(k2,Ckm2 )
fviz_cluster(k2,Ckm2,ellipse.type = "euclid",repel = TRUE,star.plot=TRUE)

res2 <-  hcut(Ckm2,k=5,stand = TRUE)

res2
fviz_dend(res2,rect=TRUE,cex=0.5,k_colors=c("red", "blue", "green","orange", "purple","gold"))


ClusterKmean2 <- data.frame(Ckm1,k2$cluster)
ClusterKmean2
#write.csv(ClusterKmean2,file = 'ClusterKmeanTP2.csv')


#Cluster Characterization-----
Centros_CT3 <- read.csv("ClusterKmeanTP2.csv",TRUE,",")


Centros_CT_means3 <- Centros_CT3 %>%
  group_by(clusters3) %>%
  summarise(across(5:13, mean, na.rm = TRUE))

Centros_CT_normalized3 <- Centros_CT_means3 %>%
  mutate(across(2:10, ~(. / (max(.)))))
Centros_CT_long3 <- pivot_longer(Centros_CT_normalized3, 
                                 cols = 2:10, 
                                 names_to = "variable", 
                                 values_to = "value")

# Asignar colores específicos a cada cluster

Centros_CT_long3$variable <- factor(Centros_CT_long3$variable, levels = c("TransitModeShare", "ESA_BuiltUp_Ratio","UrbanizedSurfaceChange", "PopulationChange","PopDensity", "GDP_Change", "GDP_PerCapita" ,"EconomicComplexity", "SegregationIndex"))
colors3 <- c("#fb8072","lightgreen", "#8dd3c7", "#fee090", "#bebada", "#9467bd","orange","#fbb4ea","darkred")

ggplot(Centros_CT_long3, aes(y = factor(variable), x = value, fill = factor(variable))) + 
  geom_col(position = position_dodge(),width=0.5) + 
  scale_fill_manual(values = colors3) + 
  facet_wrap(~ "Cluster"+ clusters3, scales = "free_y") + 
  labs(x = "", y = "", fill = "Variable") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=7.5 ),legend.position = "bottom",strip.text = element_text(size = 8))

#OLS Models-------

#Model 1-----
#Apply log to control scales to variables

Testing<- read.csv("VF_R4_NewLabels.csv",TRUE,",")
Testing

# Ajustar un modelo de regresión lineal
R.ilr.t <- lm(SegregationIndex ~ GDP_PerCapita + GDP_Change + PopDensity + PopulationChange + 
                UrbanizedSurfaceChange + TransitModeShare + ESA_BuiltUp_Ratio + Roads + 
                NumberMunicipalitiesinMetroArea, data = Testing)

# Generar un resumen con coeficientes robustos
summary(R.ilr.t)

# Generar un resumen con coeficientes robustos
summ(R.ilr.t, robust = "HC1",center=TRUE)

# Generar un gráfico de los coeficientes con intervalos de confianza robustos

plot1 <- plot_summs(R.ilr.t,robust = "HC1",scale=TRUE,center=TRUE,n.sd=3)
plot1

#Model 2-----

Testing<- read.csv("VF_R4_NewLabels.csv",TRUE,",")
Testing

cor(Testing$E_Complexity,Testing$Roads)

R.ilr.t2 <- lm(SegregationIndex ~ EconomicComplexity + GDP_PerCapita 
               + GDP_Change  + PopDensity + PopulationChange + UrbanizedSurfaceChange + TransitModeShare  +
                 ESA_BuiltUp_Ratio + NumberMunicipalitiesinMetroArea , data = Testing)

summary(R.ilr.t2)


summ(R.ilr.t2, robust = "HC1",center=TRUE)

plot2 <- plot_summs(R.ilr.t2,robust = "HC1",scale=TRUE,center=TRUE,n.sd=3)
plot2


plot1 + plot2 + plot_annotation(tag_levels = "1", tag_prefix = "Model ")
