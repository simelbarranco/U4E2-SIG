# Instalando y cargando librerías necesarias
install.packages("ggplot2", dep=T)
install.packages("sf", dep=T)
install.packages("readxl", dep=T)
install.packages("dplyr", dep=T)

library(ggplot2)
library(sf)
library(readxl)
library(dplyr)

# Configurando ruta global y espacio de trabajo
setwd("C:/Users/EmilB/OneDrive/Escritorio/U4A2-SIG/U4A2-SIG")

# Configurando la ubicación del shapefile (asegurarse de incluir todos los archivos: .shp, .shx, .dbf)
mapLocation <- "RD_PROV/RD_PROV.shp"

# Cargando dataset
data <- read.csv("matrimonios_dataset_modificado.csv", stringsAsFactors = FALSE)

# Limpiando nombres de columnas
colnames(data) <- gsub("[^A-Za-z0-9_]", "_", colnames(data))

# Asegurando que la columna 'Code' es numérica
data$Code <- as.integer(data$Code)

# Cargando shapefile
map <- st_read(mapLocation)

# Asegurando que la columna de unión es numérica
map$PROV <- as.integer(map$PROV)

# Uniendo los datos con el shapefile
mapWithData <- map %>% left_join(data, by = c("PROV" = "Code"))

# Verificar nombres de columnas después de la unión
print(colnames(mapWithData))

# Verificar si 'Total' es una columna válida
if(!"Total" %in% colnames(mapWithData)) {
  stop("Error: La columna 'Total' no se encuentra en el dataset unido. Revisa los nombres de columnas en el dataset original.")
}

# Convertir 'Total' en numérico
mapWithData$Total <- as.numeric(gsub(" ", "", mapWithData$Total))

# Creando la visualización del mapa
ggplot() +
  geom_sf(data = mapWithData, aes(fill = Total)) +
  scale_fill_gradientn(
    colors = c("darkblue", "cornflowerblue", "deepskyblue"),
    values = scales::rescale(c(min(mapWithData$Total, na.rm=TRUE), 
                               median(mapWithData$Total, na.rm=TRUE), 
                               max(mapWithData$Total, na.rm=TRUE))),
    name = "Cantidad de Matrimonios"
  ) +
  geom_sf_text(data = mapWithData,
               aes(label = Total),
               size = 2,
               color = "white", check_overlap = TRUE) +
  labs(title = "Matrimonios Registrados por Provincias entre 2001 y 2023",
       subtitle = "Realizado por Emil Pérez",
       caption = "A00160088") +
  theme_minimal()