################# 
#### Analyse, Dataframes, Figures and Maps 
#### Author: Pedro Rajão
#### Data: 15/08/2023
#################
################# 
#### packages
#################
library(ggplot2)
library(sf)
library(dplyr)
library(terra)
library(stringr)
library(raster)
library(rgdal)
library(cowplot)
library(lattice)
library(dplyr)
library(tidyr)
library(extrafont)

################# 
#### dataframes and Analyses
#################
diretorio <- "C:/Users/pedro.rajao/Desktop/AGEVAP/10. Resutados - C-CO2-PAF/2. SENTINEL 2/1. Tables Export of GEE"
arquivos <- list.files(path = diretorio, pattern = "^tabela_areas_classes_uso_solo_\\d{4}\\.csv$", full.names = TRUE)

lista_df <- lapply(arquivos, function(arquivo) {
  df <- read.csv(arquivo)
  ano <- sub("^tabela_areas_classes_uso_solo_(\\d{4})\\.csv$", "\\1", basename(arquivo))
  df$ano <- as.factor(ano)
  return(df)
})

data_frame_final <- bind_rows(lista_df) 
data_frame_final$sum_forest..m2. <- data_frame_final$forest..m2. + data_frame_final$secondary_forest..m2.
levels(as.factor(data_frame_final$NOME_PROP))  # TODAS AS PROPRIEDADES INTEGRADAS AO PAF

data_frame_final_PAF2023 <- data_frame_final %>% 
  filter(NOME_PROP %in% c('Sítio Inhamoras', 
                          'Sítio Suinã',
                          'Sítio São José',
                          'Sítio Nossa Senhora da Piedade',
                          'Sítio Lago Azul',
                          'Sítio Inhamoras',
                          'Sítio do Casaco',
                          'Sítio do Cambucá',
                          'Sítio das Tocas',
                          'Sítio Candeias',
                          'Loteamento Fazenda Grama',
                          'Fazenda Talismã',
                          'Fazenda Sertão do Procópio',
                          'Fazenda São José',
                          'Fazenda Olaria'))   # TODAS AS PROPRIEDADES INTEGRADAS AO PAF 2023
    

data_frame_final_PAF_b <- cbind(data_frame_final_PAF2023[2:10], data_frame_final_PAF2023[19], stack(data_frame_final_PAF2023[11:17]))
data_frame_final_PAF_b$ind <- as.factor(data_frame_final_PAF_b$ind)

#########
### data: ALL PAF (all versions OR 2023)
### Two LUclasses: sussecional stages forests
soma_por_classe_ano <- aggregate(data_frame_final_PAF_b$values, 
                                 by = list(data_frame_final_PAF_b$ind, data_frame_final_PAF_b$ano), 
                                 FUN = sum)

colnames(soma_por_classe_ano) <- c("classe", "ano", "m2")
soma_por_classe_ano$ano <- as.character(soma_por_classe_ano$ano)
soma_por_classe_ano$classe <- as.factor(soma_por_classe_ano$classe)
soma_por_classe_ano$m2 <- (soma_por_classe_ano$m2)/10000
colnames(soma_por_classe_ano) <- c("classe", "ano", "hectares")
str(soma_por_classe_ano)

#general chart: Forest AND Initial Forests
class_names <- c("forest..m2." = "floresta avançada", 'agriculture..m2.' = 'agricultura/silvicultura', "pasture..m2." = "pastagem", 
                 "urban..m2." =  'área urbana/solo exposto', "water..m2." = 'rios, lagos', "rock..m2." = "afloramento rochoso", "secondary_forest..m2." = 'floresta inicial/média')


p<-ggplot(data=soma_por_classe_ano, aes(x=ano, y=hectares, group= classe, 
                                     color=classe)) +
  geom_line(size=2) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(hjust = 1, size = 12), text = element_text(family = 'serif')) +
  scale_color_manual(values=c('#45c2a5', '#006400', '#ffd966', "#D5D5E5", '#00ff00',
                              "darkred", '#0000ff' ), 
                     labels=class_names) +
  xlab("ano") + ylab("hectares") +
    theme(legend.position = "none")
print(p)
ggsave('twoForest_LULC_ALL_PAF_2023_v1', plot = p, device = "png", dpi = 300, path = 'C:/Users/pedro.rajao/Desktop/AGEVAP/10. Resutados - C-CO2-PAF/2. SENTINEL 2')



### data: ALL PAF (all versions OR 2023)
### one forest LU classe: SUM_Forest
colunas_selecionadas <- c(2:10, 19, 11, 13, 14, 16, 17, 20)
data_frame_final_PAF_c <- data_frame_final_PAF2023[, colunas_selecionadas]
data_frame_final_PAF_d <- cbind(data_frame_final_PAF_c[1:10], stack(data_frame_final_PAF_c[11:16]))
data_frame_final_PAF_d$ind <- as.factor(data_frame_final_PAF_d$ind)

soma_por_classe_ano <- aggregate(data_frame_final_PAF_d$values, 
                                 by = list(data_frame_final_PAF_d$ind, data_frame_final_PAF_d$ano), 
                                 FUN = sum)

colnames(soma_por_classe_ano) <- c("classe", "ano", "m2")
soma_por_classe_ano$ano <- as.character(soma_por_classe_ano$ano)
soma_por_classe_ano$classe <- as.factor(soma_por_classe_ano$classe)
soma_por_classe_ano$m2 <- (soma_por_classe_ano$m2)/10000
colnames(soma_por_classe_ano) <- c("classe", "ano", "hectares")
str(soma_por_classe_ano)

#general chart: SUM_Forest
class_names <- c("sum_forest..m2." = "floresta", 'agriculture..m2.' = 'agricultura/silvicultura', "pasture..m2." = "pastagem", 
                 "urban..m2." =  'área urbana/solo exposto', "water..m2." = 'rios, lagos', "rock..m2." = "afloramento rochoso")


p2 <- ggplot(data=soma_por_classe_ano, aes(x=ano, y=hectares, group= classe, 
                                     color=classe)) +
  geom_line(size=2) +
  theme_cowplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(hjust = 1, size = 12), text = element_text(family = 'serif')) +
  scale_color_manual(values=c('#45c2a5', '#ffd966', "#D5D5E5",  
                              "darkred", '#0000ff', '#006400'), 
                     labels=class_names) +
  xlab("ano") + ylab("hectares") +
  theme(legend.position = "none")
print(p2)
ggsave('oneForest_LULC_ALL_PAF_2023_v1', plot = p2, device = "png", dpi = 300, path = 'C:/Users/pedro.rajao/Desktop/AGEVAP/10. Resutados - C-CO2-PAF/2. SENTINEL 2')


##############
#########
### data: FOR properties PAF
### 
########
##############
soma_por_prop_classe_ano <- aggregate(data_frame_final_PAF_b$values, 
                                      by = list(data_frame_final_PAF_b$ind, data_frame_final_PAF_b$ano,
                                                data_frame_final_PAF_b$NOME_PROP), 
                                      FUN = sum)

colnames(soma_por_prop_classe_ano) <- c("classe", "ano", "prop_POL", "hectares")
soma_por_prop_classe_ano$ano <- as.factor(soma_por_prop_classe_ano$ano)
soma_por_prop_classe_ano$classe <- as.factor(soma_por_prop_classe_ano$classe)
soma_por_prop_classe_ano$prop_POL <- as.character(soma_por_prop_classe_ano$prop_POL)
soma_por_prop_classe_ano$hectares <- (soma_por_prop_classe_ano$hectares)/10000

write.csv2(soma_por_prop_classe_ano, 
           file = "C:/Users/pedro.rajao/Desktop/AGEVAP/10. Resutados - C-CO2-PAF/2. SENTINEL 2/5. Tables Export of R/soma_por_prop_classe_ano_PAF_2023.csv", row.names = FALSE)

### function for export CHARTS
lineplot_facet <- function(data, x, y, facet_var, group_var, color_var, colors, 
                           x_label, y_label, y_lim, y_breaks, n_cols = 3, output_dir = "") {
  facet_levels <- unique(data[[facet_var]])
  n_levels <- length(facet_levels)
  n_rows <- ceiling(n_levels/n_cols)
  
  #class_colors <- c("forest..m2." = "#006400", 'agriculture..m2.' = '#45c2a5', "pasture..m2." = "#ffd966", "urban..m2." =  'darkred', "water..m2." = '#0000ff', "rock..m2." = "#D5D5E5", "secondary_forest..m2." = '#00ff00')
  class_colors <- c("sum_forest..m2." = "#006400", 'agriculture..m2.' = '#45c2a5', "pasture..m2." = "#ffd966", "urban..m2." =  'darkred', "water..m2." = '#0000ff', "rock..m2." = "#D5D5E5")
  
  for (i in 1:n_levels) {
    level <- facet_levels[i]
    subset_data <- data[data[[facet_var]] == level, ]
    
    p <- ggplot(data = subset_data, aes({{x}}, {{y}}, group = {{group_var}}, color = {{color_var}})) +
      geom_line(size = 2) +
      theme_cowplot() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(hjust = 1, size = 12), text = element_text(family = 'serif')) +
      scale_color_manual(values = class_colors, guide = FALSE) +
      xlab(x_label) + ylab(y_label)  +
      ggtitle(paste(level))
    
    filename <- paste(output_dir, level, sep = "/") 
    ggsave(filename, plot = p, device = "png", dpi = 300)
    print(p)
  }
}
output_directory <- 'C:/Users/pedro.rajao/Desktop/AGEVAP/10. Resutados - C-CO2-PAF/2. SENTINEL 2/4. Figures Export R/allPAF/oneForest'
lineplot_facet(data = soma_por_prop_classe_ano, x = ano, y = hectares, 
               facet_var = 'prop_POL', group_var = classe, color_var = classe, 
               colors = class_colors,
               x_label = "ano", y_label = "hectares",
               output_dir = output_directory)


###############################################################################
################# 
#### MAPS
##

## Loading shapefiles
shapefile_limites2 <- readOGR(file.choose())
shapefile_restauracao <- readOGR(file.choose())
shapefile_conservacao <- readOGR(file.choose())
shapefile_restauracao_2023a <- readOGR(file.choose())

## Loading TIFFs
s2_2015 <- rast(file.choose())
s2_2016 <- rast(file.choose())
s2_2017 <- rast(file.choose())
s2_2018 <- rast(file.choose())
s2_2019 <- rast(file.choose())
s2_2020 <- rast(file.choose())
s2_2021 <- rast(file.choose())
s2_2022 <- rast(file.choose())
s2_2023 <- rast(file.choose())

str(s2_2023)
crs_raster <- crs(s2_2023)


# Reprojetar os shapefiles para a projeção do raster (UTM)
shapefile_limites3 <- spTransform(shapefile_limites2, crs_raster)
shapefile_restauracao <- spTransform(shapefile_restauracao, crs_raster)
shapefile_conservacao <- spTransform(shapefile_conservacao, crs_raster)
shapefile_restauracao_2023 <- spTransform(shapefile_restauracao_2023a, crs_raster)



# shapefile to vector
shapefile_limites <- vect(shapefile_limites3)
shapefile_linha_vermelha <- vect(shapefile_restauracao)
shapefile_linha_vermelha2023 <- vect(shapefile_restauracao_2023)
shapefile_linha_azul <- vect(shapefile_conservacao)

#############################################################################
###### map generator with manual selection properties
shapefile_PROP <- shapefile_limites[shapefile_limites$NOME_PROP == "Sítio Inhamoras",]
shapefile_PROPver <- shapefile_linha_vermelha[shapefile_linha_vermelha$NOME_PROP == "Associação Quilombola",]
shapefile_PROPver2023 <- shapefile_linha_vermelha2023[shapefile_linha_vermelha2023$NOME_PROP == "Sítio Inhamoras",]
shapefile_PROPaz <- shapefile_linha_azul[shapefile_linha_azul$NOME_PROP == "Associação Quilombola",]

raster_PROP <- mask(crop(s2_2015, shapefile_PROPver2023), shapefile_PROPver2023)
shapefile_fator_sf <- st_as_sf(shapefile_PROP)
shapefile_fator_sfV <- st_as_sf(shapefile_PROPver)
shapefile_fator_sfV2023 <- st_as_sf(shapefile_PROPver2023)
shapefile_fator_sfA <- st_as_sf(shapefile_PROPaz)

df_raster_clipped <- as.data.frame(raster_PROP, xy=TRUE)

# Define colors for each value of classification_2021
class_colors <- c("forest..m2." = "#006400", 'agriculture..m2.' = '#45c2a5', "pasture..m2." = "#ffd966", "urban..m2." =  'darkred', "water..m2." = '#0000ff', "rock..m2." = "#D5D5E5", "secondary_forest..m2." = '#00ff00')

print(
  ggplot() +
    geom_raster(aes(x = x, y = y, fill = factor(classification)), data = df_raster_clipped) +
    scale_fill_manual(values = cores) +
    geom_sf(data = shapefile_fator_sfV2023, color = "red", fill = "transparent", lwd = 0.5)  +
    cowplot::theme_cowplot() +
    scale_y_continuous(breaks = seq(min(df_raster_clipped$y), max(df_raster_clipped$y), by = 0.1))
)




#################################################
###### map generator for automatic CLIP: PAF restauração 2023 
## shapefile vetorizado e raster do mapbiomas.

gerar_mapas_separados_1 <- function(shapefile, raster, output_dir) {
  
  # Get unique values of NOME_PROP
  fatores <- unique(shapefile$NOME_PROP)
  
  # Define colors for each value of classification_2021
  cores <- c("0"="darkred", "1"="#006400", "2"="#0000ff", "3"='#ffd966', "4"="#45c2a5", "5"="#00ff00", "6"= '#D5D5E5')
  
  # Define labels for each class
  labels_classes <- c("1" = "floresta", "5" = "floresta regeneração", "4" = "agricultura","3" = "Pastagem", "6" = "rocha", "2" = "rios", "0" = "urbano")
  
  # Iterate over each factor and generate separate maps
  for (fator in fatores) {
    # Filter shapefile by the current factor
    shapefile_fator <- shapefile[shapefile$NOME_PROP == fator, ]
    
    # Crop and mask the raster using shapefile_fator
    raster_fator <- raster::crop(raster, shapefile_fator)
    raster_fator2 <- raster::mask(raster_fator, shapefile_fator)
    df_raster_fator <- as.data.frame(raster_fator2, xy = TRUE)
    
    # Convert shapefile_fator to sf object
    shapefile_fator_sf <- st_as_sf(shapefile_fator)
    
    # Generate the map
    p <- ggplot() +
      geom_raster(aes(x = x, y = y, fill = factor(df_raster_fator[[3]])), data = df_raster_fator) +
      geom_sf(data = shapefile_fator_sf, aes(geometry = geometry), color = "red", fill = "transparent", lwd = 0.0007) +
      scale_fill_manual(values = cores, label=labels_classes, name = "classes de Uso do solo") +
      theme_cowplot() +
      ggtitle(paste(fator)) +
      labs(x = "latitude (º)", y = "longitude (º)") +
      theme(legend.text = element_text(size = 8), 
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 45, hjust = 1),
            axis.ticks.length = unit(0.15, "cm"),
            legend.position = "none") +
      scale_x_continuous(breaks = seq(min(df_raster_fator$x), max(df_raster_fator$x), by = 0.005),
                         labels = function(x) round(x, 2)) +
      scale_y_continuous(breaks = seq(min(df_raster_fator$y), max(df_raster_fator$y), by = 0.005),
                         labels = function(y) round(y, 2))
    
    print(p)
    
    
    # Save the plot as a high-resolution image
    output_filename <- paste(output_dir, "/", gsub("\\W+", "", fator), ".png", sep = "")
    ggsave(output_filename, p, dpi = 300)
  }
}

# Specify the output directory where the images will be saved
output_directory <- "C:/Users/pedro.rajao/Desktop/AGEVAP/10. Resutados - C-CO2-PAF/2. SENTINEL 2/3. Maps Export R/2023/PAF_2023"

# Call the function to generate and save the maps
gerar_mapas_separados_1(shapefile_linha_vermelha2023, s2_2023, output_directory)



###################################################
##### MAPAS SEM LEGENDA DE USO DO SOLO
# Function to generate separate and salve maps for each factor CLIP: limites + PAF restauração 2023 

gerar_mapas_separados <- function(shapefile, raster, shapefile_vermelha, output_dir) {
  
  # Get unique values of NOME_PROP
  fatores <- unique(shapefile$NOME_PROP)
  
  # Define colors for each value of classification_2021
  cores <- c("0"="darkred", "1"="#006400", "2"="#0000ff", "3"='#ffd966', "4"="#45c2a5", "5"="#00ff00", "6"= '#D5D5E5')
  
  # Define labels for each class
  labels_classes <- c("1" = "floresta", "5" = "floresta regeneração", "4" = "agricultura","3" = "Pastagem", "6" = "rocha", "2" = "rios", "0" = "urbano")
  
  # Iterate over each factor and generate separate maps
  for (fator in fatores) {
    # Filter shapefile by the current factor
    shapefile_fator <- shapefile[shapefile$NOME_PROP == fator, ]
    
    # Crop and mask the raster using shapefile_fator
    raster_fator <- raster::crop(raster, shapefile_fator)
    raster_fator2 <- raster::mask(raster_fator, shapefile_fator)
    df_raster_fator <- as.data.frame(raster_fator2, xy = TRUE)
    
    # Convert shapefile_fator to sf object
    shapefile_fator_sf <- st_as_sf(shapefile_fator)
    
    # Filter shapefile_azul and shapefile_vermelha by the current factor
    shapefile_vermelha_fator <- st_as_sf(shapefile_vermelha[shapefile_vermelha$NOME_PROP == fator, ])
    
    # Generate the map
    p <- ggplot() +
      geom_raster(aes(x = x, y = y, fill = factor(df_raster_fator[[3]])), data = df_raster_fator) +
      geom_sf(data = shapefile_fator_sf, aes(geometry = geometry), color = "black", fill = "transparent", lwd = 0.007) +
      geom_sf(data = shapefile_vermelha_fator, aes(geometry = geometry), color = "#DC143C", fill = "transparent", lwd = 0.005) +
      scale_fill_manual(values = cores, labels=labels_classes, name = "classes de Uso do solo") +
      theme_cowplot() +
      ggtitle(paste(fator)) +
      labs(x = "latitude (º)", y = "longitude (º)") +
      theme(legend.text = element_text(size = 8), 
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 45, hjust = 1),
            axis.ticks.length = unit(0.15, "cm"),
            legend.position = "none") +
      scale_x_continuous(breaks = seq(min(df_raster_fator$x), max(df_raster_fator$x), by = 0.005),
                         labels = function(x) round(x, 2)) +
      scale_y_continuous(breaks = seq(min(df_raster_fator$y), max(df_raster_fator$y), by = 0.005),
                         labels = function(y) round(y, 2))
    
    print(p)
    
    
    # Save the plot as a high-resolution image
    output_filename <- paste(output_dir, "/", gsub("\\W+", "", fator), ".png", sep = "")
    ggsave(output_filename, p, dpi = 300)
  }
}

# Specify the output directory where the images will be saved
output_directory_1 <- "C:/Users/pedro.rajao/Desktop/AGEVAP/10. Resutados - C-CO2-PAF/2. SENTINEL 2/3. Maps Export R/2023/PAF"

# Call the function to generate and save the maps
gerar_mapas_separados(shapefile_limites, s2_2023, shapefile_linha_vermelha2023, output_directory_1)





install.packages("networkD3")
library(networkD3)

install.packages("googleVis")
library(googleVis)

s2_2023_PAF2023 <- raster::crop(s2_2023, shapefile_restauracao_2023)
s2_2023_PAF2023b <- as.data.frame(s2_2023_PAF2023, xy = TRUE)

s2_2015_PAF2023 <- raster::crop(s2_2015, shapefile_restauracao_2023)
s2_2015_PAF2023b <- as.data.frame(s2_2015_PAF2023, xy = TRUE)

fileInfo <- data.frame(nodeCol=1, s2_2015_PAF2023, rasterBand=1) %>%
  rbind(data.frame(nodeCol=2, s2_2023_PAF2023, rasterBand=1)) 
# Define colors for each value of classification_2021
cores <- c("0"="darkred", "1"="#006400", "2"="#0000ff", "3"='#ffd966', "4"="#45c2a5", "5"="#00ff00", "6"= '#D5D5E5')

nodeInfo <- data.frame(nodeName="2015 urban"       , nodeID=0,  mapClass=21, nodeCol=1, nodeGroup='a') %>%
  rbind(data.frame(nodeName="2015 forest"    , nodeID=1,  mapClass=22, nodeCol=1, nodeGroup='b')) %>%
  rbind(data.frame(nodeName="2015 water" , nodeID=2,  mapClass=23, nodeCol=1, nodeGroup='c')) %>%
  rbind(data.frame(nodeName="2015 pasture"   , nodeID=3,  mapClass=24, nodeCol=1, nodeGroup='d')) %>%
  rbind(data.frame(nodeName="2015 agriculture"                 , nodeID=4,  mapClass=31, nodeCol=1, nodeGroup='e')) %>%
  rbind(data.frame(nodeName="2015 forest2"                 , nodeID=5,  mapClass=52, nodeCol=1, nodeGroup='f')) %>%
  rbind(data.frame(nodeName="2015 rock"                 , nodeID=6,  mapClass=52, nodeCol=1, nodeGroup='g')) %>%
  
  rbind(data.frame(nodeName="2023 urban"       , nodeID=6,  mapClass=21, nodeCol=2, nodeGroup='a')) %>%
  rbind(data.frame(nodeName="2023 forest"    , nodeID=7,  mapClass=22, nodeCol=2, nodeGroup='b')) %>%
  rbind(data.frame(nodeName="2023 water" , nodeID=8,  mapClass=23, nodeCol=2, nodeGroup='c')) %>%
  rbind(data.frame(nodeName="2023 pasture"   , nodeID=9,  mapClass=24, nodeCol=2, nodeGroup='d')) %>%
  rbind(data.frame(nodeName="2023 agriculture"                 , nodeID=10, mapClass=31, nodeCol=2, nodeGroup='e')) %>%
  rbind(data.frame(nodeName="2023 forest2"                 , nodeID=11, mapClass=52, nodeCol=2, nodeGroup='f')) %>%
  rbind(data.frame(nodeName="2023 rock"                 , nodeID=6,  mapClass=52, nodeCol=1, nodeGroup='g'))
  
groupColor  <- c("darkred", "#006400", "#0000ff", '#ffd966', "#45c2a5", "#00ff00", '#D5D5E5')
fontSize <- 12
nodeWidth <- 30
fontFamily <- "sans-serif"

groupColor <- paste0('"',paste(groupColor, collapse = '", "'),'"')
nodeInfo <- dplyr::left_join(nodeInfo, fileInfo, by='nodeCol')
nodeInfo$nodeName <- as.character(nodeInfo$nodeName)
nodeInfo$rasterFile <- as.character(nodeInfo$rasterFile)

NodeCols <- sort(unique(nodeInfo$nodeCol))
linkInfo <- data.frame()
for(i in 1:(length(NodeCols)-1)){
  fromCol <- dplyr::filter(nodeInfo, nodeCol==NodeCols[i])
  toCol <- dplyr::filter(nodeInfo, nodeCol==NodeCols[i+1])
  fromR <- values(raster(fromCol$rasterFile[1], fromCol$rasterBand[1]))
  toR <- values(raster(toCol$rasterFile[1], toCol$rasterBand[1]))
  for(f in 1:nrow(fromCol)){
    for(t in 1:nrow(toCol)){
      nFromTo <- length(which(fromR == fromCol$mapClass[f] & toR == toCol$mapClass[t]))
      linkInfo <- rbind(linkInfo, data.frame(source=fromCol$nodeID[f], target=toCol$nodeID[t], value=nFromTo))
    }
  }
}
sankeyNetwork(Links = linkInfo, Nodes = nodeInfo,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "nodeName",
              NodeGroup = "nodeGroup",
              fontSize = fontSize,
              fontFamily = fontFamily,
              nodeWidth = nodeWidth,
              colourScale = paste0('d3.scaleOrdinal().range([',groupColor,'])'))



# Verificar valores duplicados em nodeCol
duplicate_nodeCol_nodeInfo <- nodeInfo[duplicated(nodeInfo$nodeCol), ]
duplicate_nodeCol_fileInfo <- fileInfo[duplicated(fileInfo$nodeCol), ]

if (nrow(duplicate_nodeCol_nodeInfo) > 0) {
  cat("Valores duplicados encontrados em nodeCol de nodeInfo:\n")
  print(duplicate_nodeCol_nodeInfo)
}

if (nrow(duplicate_nodeCol_fileInfo) > 0) {
  cat("Valores duplicados encontrados em nodeCol de fileInfo:\n")
  print(duplicate_nodeCol_fileInfo)
}

# Verificar correspondência entre valores em nodeCol
missing_values_nodeInfo <- nodeInfo[!(nodeInfo$nodeCol %in% fileInfo$nodeCol), ]

if (nrow(missing_values_nodeInfo) > 0) {
  cat("Valores em nodeCol de nodeInfo sem correspondência em fileInfo:\n")
  print(missing_values_nodeInfo)
}

# Verificar se cada valor em nodeCol é único
if (any(duplicated(nodeInfo$nodeCol))) {
  cat("Valores duplicados encontrados em nodeCol de nodeInfo:\n")
  print(nodeInfo[duplicated(nodeInfo$nodeCol), ])
}

if (any(duplicated(fileInfo$nodeCol))) {
  cat("Valores duplicados encontrados em nodeCol de fileInfo:\n")
  print(fileInfo[duplicated(fileInfo$nodeCol), ])
}

