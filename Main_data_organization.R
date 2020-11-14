library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(tmap)



setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Capítulo_2/Análise")

load("Organização_tabela_principal.Rdata")



##### IBGE Map (Shapefile) #####

setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Dados/IBGE")


## Load file

Shape_Muni <- readOGR(".", layer = "Shape_sem_dados_errados") #From rgdal package

proj4string (Shape_Muni) # See current projection 


## first reproject the shapefile

Shape_Muni <- spTransform(Shape_Muni, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

proj4string (Shape_Muni) # See reprojection 

# writeOGR(Shape_Muni, "Mapa_reprojetado", "Mapa_reprojetado", driver = "ESRI Shapefile") # Save reprojected map


## Calculate the counties area

Shape_Muni@data$AreaKm2 <- area(Shape_Muni)/1000000 # This calculate the area in km2
head(Shape_Muni@data)



##### Dengue Persistence Data ##### 

setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Dados/Brady_et.al.")


## Raster Aegypti (endemic) 

Ae_persist_DEN<-raster("Annual_AUC_Ae_PERSIST_DEN.tif") #(Global raster)

x11(title = "Ae_persist_DEN")
plot(Ae_persist_DEN)


## Raster Albopictus 

Al_persist_DEN<-raster("Annual_AUC_Al_PERSIST_DEN.tif") #(Global raster)

x11(title = "Al_persist_DEN")
plot(Al_persist_DEN)



##### Test to extract raster Information######

# Create some sample raster data
r <- raster(ncol=36, nrow=18)
r[] <- 1:ncell(r)
plot(r)


#Create some sample polygons
cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                              Polygons(list(Polygon(cds2)), 2)))
  
plot(polys)
str(polys)


# Extract the raster values underlying the polygons
v <- extract(r, polys)
v





##### Extract dengue persistence information by county #####


## Ae. Aegypti 

Ae_Den_Muni <- extract(Ae_persist_DEN, Shape_Muni, small = TRUE)

Ae_Den_Per_mean <- unlist(lapply(Ae_Den_Muni, mean)) # Mean by countie

Ae_Den_Per_SD <-  unlist(lapply(Ae_Den_Muni,sd)) # Standard deviation


## Ae. Albopictus 

Al_Den_Muni <- extract(Al_persist_DEN, Shape_Muni, small = TRUE)

Al_Den_Per_mean <- unlist(lapply(Al_Den_Muni, mean))

Al_Den_Per_SD <- unlist(lapply(Al_Den_Muni,sd))



### Data Matrix ###

Matriz_final <- matrix()
ID <- as.vector(0:5569) 

Matriz_final <- cbind(ID,Ae_Den_Per_mean,Ae_Den_Per_SD,Al_Den_Per_mean,Al_Den_Per_SD)

length(which(is.na(Matriz_final[,3]))) # Amount of counties that have only one raster value (NO standard deviation then)



### Bind MS data with Dengue Persistence by mosquito specie ###

setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Dados/MS") # Change directory temporarily

Dengue_2014_MN <- read.csv("Dengue_por_municipio_2014.csv")
Dengue_2015_MN <- read.csv("Dengue_por_municipio_2015.csv")
Dengue_2016_MN <- read.csv("Dengue_por_municipio_2016.csv")


# OBS: Get back to previous directory

setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Dados")


## Merge cases data with transmission probability


Merged_2014 <- merge(Dengue_2014_MN, Matriz_final, by = "ID")


Merged_2015 <- merge(Dengue_2015_MN, Matriz_final, by = "ID")


Merged_2016 <- merge(Dengue_2016_MN, Matriz_final, by = "ID")


## Take weekly cases out and leave only the total number of cases

# 2014

Tab_Total_2014 <- Merged_2014[,c(1:4, 58:62)]
names(Tab_Total_2014)[5] <- "Total_cases_MS"

# 2015

Tab_Total_2015 <- Merged_2015[,c(1:4, 57:61)]
names(Tab_Total_2015)[5] <- "Total_cases_MS"

# 2016

Tab_Total_2016 <- Merged_2016[,c(1:4, 54:58)]
names(Tab_Total_2016)[5] <- "Total_cases_MS"



##### Exploratory Map #####

# Join data to the Brazilian counties shape

Den_Map_2014 <- merge(Shape_Muni, Tab_Total_2014, by.x = "CD_GEOCMU", by.y = "CD_IBGE") 
Den_Map_2015 <- merge(Shape_Muni, Tab_Total_2015, by.x = "CD_GEOCMU", by.y = "CD_IBGE") 
Den_Map_2016 <- merge(Shape_Muni, Tab_Total_2016, by.x = "CD_GEOCMU", by.y = "CD_IBGE") 

## Aegypty Mean 

# Map 2014

display.brewer.all() # Color options

tm_shape(Den_Map_2014) + tm_fill("Ae_Den_Per_mean", palette = "BuPu") + tm_borders(alpha=.1) +
  tm_shape(Den_Map_2014) + tm_bubbles(size = "Total_cases_MS", col = "Total_cases_MS", palette = "-RdGy", style = "quantile", legend.size.show = FALSE,
                                      title.col = "MS cases 2014", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
  

# Map 2015

tm_shape(Den_Map_2015) + tm_fill("Ae_Den_Per_mean", palette = "BuPu") + tm_borders(alpha=.1) +
  tm_shape(Den_Map_2015) + tm_bubbles(size = "Total_cases_MS", col = "Total_cases_MS", palette = "-RdGy", style = "quantile", legend.size.show = FALSE,
                                      title.col = "MS cases 2015", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)


# Map 2016

tm_shape(Den_Map_2016) + tm_fill("Ae_Den_Per_mean", palette = "BuPu") + tm_borders(alpha=.1) +
  tm_shape(Den_Map_2016) + tm_bubbles(size = "Total_cases_MS", col = "Total_cases_MS", palette = "-RdGy", style = "quantile", legend.size.show = FALSE,
                                      title.col = "MS cases 2016", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)



## Albopictus mean


# Map 2014

tm_shape(Den_Map_2014) + tm_fill("Al_Den_Per_mean", palette = "BuPu") + tm_borders(alpha=.1) +
  tm_shape(Den_Map_2014) + tm_bubbles(size = "Total_cases_MS", col = "Total_cases_MS", palette = "-RdGy", style = "quantile", legend.size.show = FALSE,
                                      title.col = "MS cases 2014", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)


# Map 2015

tm_shape(Den_Map_2015) + tm_fill("Al_Den_Per_mean", palette = "BuPu") + tm_borders(alpha=.1) +
  tm_shape(Den_Map_2015) + tm_bubbles(size = "Total_cases_MS", col = "Total_cases_MS", palette = "-RdGy", style = "quantile", legend.size.show = FALSE,
                                      title.col = "MS cases 2015", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)


# Map 2016

tm_shape(Den_Map_2016) + tm_fill("Al_Den_Per_mean", palette = "BuPu") + tm_borders(alpha=.1) +
  tm_shape(Den_Map_2016) + tm_bubbles(size = "Total_cases_MS", col = "Total_cases_MS", palette = "-RdGy", style = "quantile", legend.size.show = FALSE,
                                      title.col = "MS cases 2016", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)




#### Insert federation codes ####


CD_UF <- read.csv("Codigo_UF.csv")


# 2014

target <- which(names(Tab_Total_2014) == 'NM_MUNICIP')[1] #Jeito TOP de inserir colunas
Tab_Total_2014 <- cbind(Tab_Total_2014[,1:target,drop=F], data.frame(CD_UF), Tab_Total_2014[,(target+1):length(Tab_Total_2014),drop=F])


# 2015

target <- which(names(Tab_Total_2015) == 'NM_MUNICIP')[1] 
Tab_Total_2015 <- cbind(Tab_Total_2015[,1:target,drop=F], data.frame(CD_UF), Tab_Total_2015[,(target+1):length(Tab_Total_2015),drop=F])


# 2016

target <- which(names(Tab_Total_2016) == 'NM_MUNICIP')[1] 
Tab_Total_2016 <- cbind(Tab_Total_2016[,1:target,drop=F], data.frame(CD_UF), Tab_Total_2016[,(target+1):length(Tab_Total_2016),drop=F])



#### Adding SIDRA data ####

setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Dados/IBGE/Tabelas_SIDRA")

Pop_residente <- read.csv2("Pop_Residente.csv") # Tem todos os municipios
Escolaridade <- read.csv2("Escolaridade.csv") # Faltam 5 municipios
Saneamento <- read.csv2("Saneamento.csv") # Faltam 6, só um não é repetido
PIB <- read.csv2("PIB.csv") # Tem todos 
Estab_Saude <- read.csv2("Quantidade_estabelecimentos_de_saude.csv") # Faltam 3



##### Function to find missing values #####


queNOestaEn <- function(a, b){
  ab <- c(a, b)
  ba <- c(b, a)
  la <- length(a)
  lb <- (length(b))
  notINa <- b[!duplicated(ab)[(la + 1):(la + lb)]]
  notINb <- a[!duplicated(ba)[(lb + 1):(la + lb)]]
  return(list("not in first list" = notINa, "not in second list" = notINb))
}

queNOestaEn(a = Tab_Total_2014$CD_IBGE, b = Dengue_2013_MN$CD_IBGE)



#### Merge with previous tables and organize ####

# 2014

Tab_Total_2014_t1 <- merge(Tab_Total_2014, Pop_residente, by = "CD_IBGE")
Tab_Total_2014_t1 <- merge(Tab_Total_2014_t1, Escolaridade, by = "CD_IBGE")
Tab_Total_2014_t1 <- merge(Tab_Total_2014_t1, Saneamento, by = "CD_IBGE")
Tab_Total_2014_t1 <- merge(Tab_Total_2014_t1, PIB, by = "CD_IBGE")
Tab_Total_2014_t1 <- merge(Tab_Total_2014_t1, Estab_Saude, by = "CD_MS")


Tab_Total_2014 <- Tab_Total_2014_t1[,c(1:10, 12, 14, 16:17, 19, 21)]
names(Tab_Total_2014)[c(4)] <- c("NM_MUNICIP")


# 2015

Tab_Total_2015_t1 <- merge(Tab_Total_2015, Pop_residente, by = "CD_IBGE")
Tab_Total_2015_t1 <- merge(Tab_Total_2015_t1, Escolaridade, by = "CD_IBGE")
Tab_Total_2015_t1 <- merge(Tab_Total_2015_t1, Saneamento, by = "CD_IBGE")
Tab_Total_2015_t1 <- merge(Tab_Total_2015_t1, PIB, by = "CD_IBGE")
Tab_Total_2015_t1 <- merge(Tab_Total_2015_t1, Estab_Saude, by = "CD_MS")


Tab_Total_2015 <- Tab_Total_2015_t1[,c(1:10, 12, 14, 16:17, 19, 21)]
names(Tab_Total_2015)[c(4)] <- c("NM_MUNICIP")


# 2016

Tab_Total_2016_t1 <- merge(Tab_Total_2016, Pop_residente, by = "CD_IBGE")
Tab_Total_2016_t1 <- merge(Tab_Total_2016_t1, Escolaridade, by = "CD_IBGE")
Tab_Total_2016_t1 <- merge(Tab_Total_2016_t1, Saneamento, by = "CD_IBGE")
Tab_Total_2016_t1 <- merge(Tab_Total_2016_t1, PIB, by = "CD_IBGE")
Tab_Total_2016_t1 <- merge(Tab_Total_2016_t1, Estab_Saude, by = "CD_MS")


Tab_Total_2016 <- Tab_Total_2016_t1[,c(1:10, 12, 14, 16:17, 19, 21)]
names(Tab_Total_2016)[c(4)] <- c("NM_MUNICIP")


## Insert 0 where is NA 

Tab_Total_2014[is.na(Tab_Total_2014)] <- 0

Tab_Total_2015[is.na(Tab_Total_2015)] <- 0

Tab_Total_2016[is.na(Tab_Total_2016)] <- 0


## Organize table by ID

Tab_Total_2014 <- Tab_Total_2014[order(Tab_Total_2014$ID),]
Tab_Total_2015 <- Tab_Total_2015[order(Tab_Total_2015$ID),]
Tab_Total_2016 <- Tab_Total_2016[order(Tab_Total_2016$ID),]


## Calculate and add demographic density (hab/Km2)

Tab_Total_2014$Densidade <- round(Tab_Total_2014$Pop_Resid / Shape_Muni@data$AreaKm2, 2)
Tab_Total_2015$Densidade <- round(Tab_Total_2015$Pop_Resid / Shape_Muni@data$AreaKm2, 2)
Tab_Total_2016$Densidade <- round(Tab_Total_2016$Pop_Resid / Shape_Muni@data$AreaKm2, 2)


## Add previous years dengue cases records

setwd("/Users/lorena.mendes.simon/Documents/Doutorado/Dados/MS") # Change directory temporarily

Dengue_2007_MN <- read.csv("Casos_Dengue_2007.csv", sep = ";") 
Dengue_2008_MN <- read.csv("Casos_Dengue_2008.csv", sep = ";") 
Dengue_2009_MN <- read.csv("Casos_Dengue_2009.csv", sep = ";") 
Dengue_2010_MN <- read.csv("Casos_Dengue_2010.csv", sep = ";") 
Dengue_2011_MN <- read.csv("Casos_Dengue_2011.csv", sep = ";") 
Dengue_2012_MN <- read.csv("Casos_Dengue_2012.csv", sep = ";") 
Dengue_2013_MN <- read.csv("Casos_Dengue_2013.csv", sep = ";")

# obs. Found missing values with "queNOestaEn" function and added in dengue cases table as 0 notifications
 

## Put all cases in a single table 

Arq_names <- list.files(getwd(), pattern = "Casos")

Casos_Passados <- as.data.frame(as.matrix(Tab_Total_2014$CD_MS))

names(Casos_Passados) <- "CD_MS"

for (i in 1:length(Arq_names)) {
  
  Arq <- read.csv(Arq_names[i], sep = ";", header = TRUE)  
  
  Casos_Passados <- merge(Casos_Passados, Arq, by = "CD_MS")  
  
}


### Final table with all data ###

# Add previous dengue cases

Tab_1 <- merge(Tab_Total_2014, Casos_Passados, by = "CD_MS")
View(Tab_1)
names(Tab_1)[6] <- "Total_cases_MS_2014"
Tab_1 <- Tab_1[order(Tab_1$ID),]


# Add cases from 2015 and 2016

Tab_2 <- cbind(Tab_1, Tab_Total_2015$Total_cases_MS)
names(Tab_2)[25] <- "Total_cases_MS_2015"

Tab_2$Total_cases_MS_2016 <- Tab_Total_2016$Total_cases_MS


# Add counties area

Tab_2$AreaKm2 <- round(Shape_Muni$AreaKm2, 3)


# Organize final table

Tab_Tudo_Final <- Tab_2[,c(3:4, 27, 1:2, 5, 7:10, 11:17, 18:24, 6, 25:26 )]


# Mean and Standard deviation of combined  dengue transmission persistence by Aegypti and Albopictus  

Both_Den_Persist <- mapply(c, Ae_Den_Muni, Al_Den_Muni)

Both_Den_Persist_Mean <- unlist(lapply(Both_Den_Persist, mean))

Both_Den_Persist_SD <- unlist(lapply(Both_Den_Persist, sd))

target <- which(names(Tab_Tudo_Final) == 'Al_Den_Per_SD')[1] # Insert a new column in a specific place
Tab_Tudo_Final <- cbind(Tab_Tudo_Final[,1:target,drop=F], data.frame(Both_Den_Persist_Mean), Tab_Tudo_Final[,(target+1):length(Tab_Tudo_Final),drop=F])

target <- which(names(Tab_Tudo_Final) == 'Both_Den_Persist_Mean')[1] 
Tab_Tudo_Final <- cbind(Tab_Tudo_Final[,1:target,drop=F], data.frame(Both_Den_Persist_SD), Tab_Tudo_Final[,(target+1):length(Tab_Tudo_Final),drop=F])

Tab_Tudo_Final[is.na(Tab_Tudo_Final)] <- 0 # Replace Nas by 0


# Recent epidemic years (2015-2016) mean

Tab_Tudo_Final$Epid_Recent_mean <- rowMeans(Tab_Tudo_Final[30:31])


# Past years (2007-2014) mean

Tab_Tudo_Final$Previous_years_mean <- rowMeans(Tab_Tudo_Final[22:29])



#### Insert urbanization data (the ratio between the urbanized area and the total area of the county) ####

ID_Referencia <- as.matrix(0:5569) # Create a reference ID

Tab_area_intersect <- read.csv("/Users/lorena.mendes.simon/Documents/Doutorado/Dados/IBGE/Urbanização/Area_inter_Qgis.csv", header = FALSE,sep = ";") # Import intersection area created in QGIS

names(Tab_area_intersect)[1:3] <- c("FID_shape", "NM_MUNICIP", "Area_Intersect")


# Sum the many intersection areas inside each county

Area_Inter <- matrix() # Empty Matrix

for (i in 1:5570) {
  
  print(i)
  
  ID_Ref <- ID_Referencia [i]
  
  Value <- vector(mode="numeric", length=0) # Create an empty vector
  
  
  for (j in 1:14094) {
    
    
    if (ID_Ref == Tab_area_intersect$FID_shape [j]){
      
      Value <- append(Value, Tab_area_intersect$Area_Intersect [j]) # Add values to the vector
    }
    
    else {
      
      Area_Inter [i] <- sum(Value) # Sum urbanized areas inside each county
    }
    
  }
}


# Ratio between the county total area and the referred urbanized amount

Prop_urb <- as.data.frame(Tab_Tudo_Final$ID)

Prop_urb$NM_MUNICIP <- Tab_Tudo_Final$NM_MUNICIP 
Prop_urb$Area_TotalKm2 <- Tab_Tudo_Final$AreaKm2
Prop_urb$Area_inter <- Area_Inter

Prop_urb$Urb_ratio <- round(Prop_urb$Area_inter/Prop_urb$Area_Total,4) # Proportion of urbanized area


target <- which(names(Tab_Tudo_Final) == 'Densidade')[1] # Add Urbanized ratio to the main data matrix
Tab_Tudo_Final <- cbind(Tab_Tudo_Final[,1:target,drop=F], data.frame(Prop_urb$Urb_ratio), Tab_Tudo_Final[,(target+1):length(Tab_Tudo_Final),drop=F])

names(Tab_Tudo_Final)[20] <- "Urban_ratio"


target <- which(names(Tab_Tudo_Final)== 'AreaKm2')[1]
Tab_Tudo_Final <- cbind(Tab_Tudo_Final[,1:target, drop = F], data.frame(Prop_urb$Area_inter), Tab_Tudo_Final[,(target+1):length(Tab_Tudo_Final), drop = F])

names(Tab_Tudo_Final)[4] <- "Urban_AreaKm2"


#### Export main data table ####

write.csv(Tab_Tudo_Final, "Tabela_Principal.csv", row.names = FALSE)


#### Save Rdata #### 

save.image("Organização_tabela_principal.Rdata") #Carefull! Be sure of what you are saving!



