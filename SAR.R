library(sp) 
library(rgdal) 
library(rgeos)
library(spdep) # Old
library(spatialreg) # New


setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Capítulo_2/Análise")

load("SARerr_Model.Rdata")


#### Load the main Shapefile of counties and reproject ####

Shape_Muni <- readOGR("/Users/lorena.mendes.simon/Documents/Doutorado/Dados/IBGE", layer = "Shape_sem_dados_errados") #From rgdal package

Shape_Muni <- spTransform(Shape_Muni, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

ID <- row.names(Shape_Muni@data) 

Shape_Muni@data$ID <- ID # Assigning an ID 


#### Upload the main csv data table previously generated ####

Tabela_Principal <- read.csv("Tabela_Principal.csv")


#### Join the csv data with the counties shapefile ####

Shape_Mun_data <- merge(Shape_Muni,Tabela_Principal[,c(1,3:33)], by = "ID")

writeOGR(obj=Shape_Mun_data, dsn="Shape_Mun_data", layer="Shape_Mun_data", driver="ESRI Shapefile") # Tive que exportar os dados para reabri-los em um workspace vazio, caso contrario a memoria do computador fica muito sobrecarregada e não roda

#Obs: Shapefile atribute table names are abreviated when saved out of the console


#### Sum both sewage treatment and pluviometric management in a single "Sanitation" named column ####

Sanitation <- Shape_Mun_data@data$Rede_Esgoto + Shape_Mun_data@data$Manejo_Pluvial

Shape_Mun_data@data$Saneamento <- Sanitation


#### Calculate the neighbours matrix from polygons (cities) ####

nb_poly <- poly2nb(Shape_Mun_data)
nb_poly
plot(Shape_Mun_data, border = 'lightgrey')
plot(nb_poly, coordinates(Shape_Mun_data), add = TRUE, col = 'red')


#### Spatial weights with Weights style "W" (row standardized) ####

listw_br <- nb2listw(nb_poly, zero.policy = TRUE) 
summary(listw_br, zero.policy = TRUE)


####----------------------------------  OLS model  -------------------------------------#####


#OLS model 
ols_past_cases <- lm(log(Shape_Mun_data@data$Previous_years_mean+1) ~ Shape_Mun_data@data$Urban_ratio + 
                          Shape_Mun_data@data$Densidade + Shape_Mun_data@data$Estab_Saude + Shape_Mun_data@data$PIB +
                       Shape_Mun_data@data$Saneamento + Shape_Mun_data@data$Escolaridade + Shape_Mun_data@data$Pop_Resid +
                       Shape_Mun_data@data$Ae_Den_Per_mean + Shape_Mun_data@data$Al_Den_Per_mean + 
                        (Shape_Mun_data@data$Both_Den_Persist_Mean*Shape_Mun_data@data$Densidade) +
                        (Shape_Mun_data@data$Urban_ratio*Shape_Mun_data@data$Both_Den_Persist_Mean)) 

summary(ols_past_cases)
res.ols <- residuals(ols_past_cases)

moran.test(res.ols, listw = listw_br, zero.policy = TRUE) 


####---------------------------------- Make a correlation matrix (Exploratory) ------------------------####

ols_past_cases_cor <- cor(Shape_Mun_data@data[,c(9,11,13, 15:17, 19:22)])
ols_past_cases_cor <- round(ols_past_cases_cor,2)
View(ols_past_cases_cor)


#####------------------------------------- Evaluate Collinearity -----------------------------------#####

library(car)

vif(ols_past_cases) # variance inflation factors
sqrt(vif(ols_past_cases)) > 10


####-----------------------------------   Step-Wise Regressions -------------------------------------####

## General to Specific ##


# Brady Persistence #

first_Persist_model <- lm(log(Shape_Mun_data@data$Previous_years_mean+1) ~ Shape_Mun_data@data$Both_Den_Persist_Mean + 
                             (Shape_Mun_data@data$Both_Den_Persist_Mean*Shape_Mun_data@data$Densidade))
vif(first_Persist_model)


# Socioeconomic variables #

first_Socio_data <- lm(log(Shape_Mun_data@data$Previous_years_mean+1) ~ Shape_Mun_data@data$Urban_ratio + 
                         Shape_Mun_data@data$Densidade + Shape_Mun_data@data$Estab_Saude + Shape_Mun_data@data$PIB +
                         Shape_Mun_data@data$Saneamento + Shape_Mun_data@data$Escolaridade + Shape_Mun_data@data$Pop_Resid + 
                          (Shape_Mun_data@data$Both_Den_Persist_Mean*Shape_Mun_data@data$Densidade) + 
                          (Shape_Mun_data@data$Urban_ratio*Shape_Mun_data@data$Both_Den_Persist_Mean))

vif(first_Socio_data)


# Out Pop_Resid

second_Socio_data <- lm(log(Shape_Mun_data@data$Previous_years_mean+1) ~ Shape_Mun_data@data$Urban_ratio + 
                         Shape_Mun_data@data$Densidade + Shape_Mun_data@data$Estab_Saude + Shape_Mun_data@data$PIB +
                         Shape_Mun_data@data$Saneamento + Shape_Mun_data@data$Escolaridade +
                           (Shape_Mun_data@data$Both_Den_Persist_Mean*Shape_Mun_data@data$Densidade) + 
                           (Shape_Mun_data@data$Urban_ratio*Shape_Mun_data@data$Both_Den_Persist_Mean))

vif(second_Socio_data)


# Out Escolaridade

third_Socio_data <- lm(log(Shape_Mun_data@data$Previous_years_mean+1) ~ Shape_Mun_data@data$Urban_ratio + 
                         Shape_Mun_data@data$Densidade + Shape_Mun_data@data$Estab_Saude + Shape_Mun_data@data$PIB +
                         Shape_Mun_data@data$Saneamento + (Shape_Mun_data@data$Both_Den_Persist_Mean*Shape_Mun_data@data$Densidade) + 
                          (Shape_Mun_data@data$Urban_ratio*Shape_Mun_data@data$Both_Den_Persist_Mean))

vif(third_Socio_data)



####-------------------------------------   SARerr model ----------------------------------------####


# SARerr model for previous years (2007-2014)

sar_past <- spatialreg::errorsarlm(lm(log(Shape_Mun_data@data$Previous_years_mean+1) ~ Shape_Mun_data@data$Urban_ratio + 
                                         Shape_Mun_data@data$Densidade + Shape_Mun_data@data$Estab_Saude + log(Shape_Mun_data@data$PIB) +
                                         Shape_Mun_data@data$Saneamento + Shape_Mun_data@data$Both_Den_Persist_Mean + (Shape_Mun_data@data$Both_Den_Persist_Mean*Shape_Mun_data@data$Densidade) + 
                                         (Shape_Mun_data@data$Urban_ratio*Shape_Mun_data@data$Both_Den_Persist_Mean)), listw = listw_br, zero.policy = TRUE)

summary(sar_past)
Shape_Mun_data@data$Residuals_past <- residuals(sar_past)

moran.test(Shape_Mun_data@data$Residuals_past, listw = listw_br, zero.policy = TRUE) 


# SARerr model for epidemic years (2015-2016)

sar_recent <- spatialreg::errorsarlm(lm(log(Shape_Mun_data@data$Epid_Recent_mean+1) ~ Shape_Mun_data@data$Urban_ratio + 
                                           Shape_Mun_data@data$Densidade + Shape_Mun_data@data$Estab_Saude + log(Shape_Mun_data@data$PIB) +
                                           Shape_Mun_data@data$Saneamento + Shape_Mun_data@data$Both_Den_Persist_Mean + (Shape_Mun_data@data$Both_Den_Persist_Mean*Shape_Mun_data@data$Densidade) + 
                                           (Shape_Mun_data@data$Urban_ratio*Shape_Mun_data@data$Both_Den_Persist_Mean)), listw = listw_br, zero.policy = TRUE)

summary(sar_recent)
Shape_Mun_data@data$Residuals_recent <- residuals(sar_recent)

moran.test(Shape_Mun_data@data$Residuals_recent, listw = listw_br, zero.policy = TRUE) 


####-------------------------------------- Models' R-squared ---------------------------------------------------#### 

## Reference: Cooper & Purvis (2010)

sar_past$logLik_lm.model # Likelihood value of the null model

R_SAR_past <- 1- exp((-2/5570)*(-7861.188-(sar_past$logLik_lm.model)))


sar_recent$logLik_lm.model # ValorLikelihood value of the null model

R_SAR_recent <- 1- exp((-2/5570)*(-8726.668-(sar_recent$logLik_lm.model)))



####------------------------------------   Maps SARerr (Exploratory) ------------------------------------------####

 library(tmap)

 Map_resid_past <- tm_shape(Shape_Mun_data) + tm_fill("Residuals_past", palette = "RdYlBu", style = "quantile", n = 5, title = "Resids") + tm_borders(alpha=.2) + 
  tm_compass(size = 1.8, fontsize = 0.5) + 
  tm_layout(title = "a)", title.size = 1.0, legend.title.size = 0.7, frame = FALSE, legend.text.size = 0.5, legend.position = c("right", "BOTTOM")) 

 
 Map_resid_recent <- tm_shape(Shape_Mun_data) + tm_fill("Residuals_recent", palette = "RdYlBu", style = "quantile", n = 5, title = "Resids") + tm_borders(alpha=.2) + 
   tm_compass(size = 1.8, fontsize = 0.5) + 
   tm_layout(title = "b)", title.size = 1.0, legend.title.size = 0.7, frame = FALSE, legend.text.size = 0.5, legend.position = c("right", "BOTTOM")) 

 
 
####------------------------------------ Correlograms (Exploratory) ------------------------------------------#### 

 library(ggplot2)
 
## Past
 
correlogram_sar_past <- sp.correlogram(nb_poly, var = Shape_Mun_data@data$Residuals_past, 
                                       zero.policy = TRUE, style = "W", order=20, method = "I", 
                                       randomisation = FALSE) # Time-consuming

print(correlogram_sar_past)


# ggplot2 Graph
 
Y_cor <- as.data.frame(correlogram_sar_past$res)
x_cor <- c(1:20)
data <- cBind(Y_cor,x_cor)
names(data)<- c("Morans", "x", "t", "Distance")


qplot(data$Distance, data$Morans,data = data, ylab="Moran's I", xlab = "Distance", geom = c("point", "line"), 
      color = "Red") + geom_line(aes(y = 0), color = "Black") + theme_classic() + 
   theme(text = element_text(size = 18))


#ggplot(subset(data), aes(x = Distance, y = Morans, ylab="Moran's I")) + geom_point(color = "Red") + geom_line(color="Red") + geom_line(aes(y = 0), color = "Black")

 
## Recent

correlogram_sar_recent <- sp.correlogram(nb_poly, var = Shape_Mun_data@data$Residuals_recent, 
                                         zero.policy = TRUE, style = "W", order=20, method = "I", 
                                         randomisation = FALSE) # Time-Consuming
 
print(correlogram_sar_recent)


# ggplot2 Graph

Y_cor_2 <- as.data.frame(correlogram_sar_recent$res)
x_cor_2 <- c(1:40)
data_2 <- cBind(Y_cor_2,x_cor_2)
names(data_2)<- c("Morans", "x", "t", "Distance")


qplot(data_2$Distance, data_2$"Morans", data = data_2, ylab="Moran's I", xlab = "Distance", 
      geom = c("point", "line"), color = "Red") + geom_line(aes(y = 0), color = "Black") + 
   theme_classic() + theme(text = element_text(size = 18))



### Save Rdata ### (Carefull! Be sure you are saving the right thing.)

save.image("SARerr_Model.Rdata")



