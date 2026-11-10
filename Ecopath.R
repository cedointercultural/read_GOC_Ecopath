#To get the list of available Ewe models
#install.packages("RCurl")
#install.packages("XML")
#install.packages("plyr")
library(RCurl)
library(XML)
library(plyr)
library(dplyr)
library(stringr)


###Modelos golfo Califronia 
##numero: 242, 450, 543, 482, 483, 239, 425, 238, 325, 241, 307

# Descargar lista
h <- basicTextGatherer()

curlPerform(
  url = "https://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php",
  writefunction = h$update,
  httpheader = c("User-Agent" = "Mozilla/5.0")
)

# Parsear XML
data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)

# Convertir a data.frame
liste_mod <- ldply(xmlToList(data), data.frame) %>%
  filter(model.dissemination_allow == "true")

# Ver estructura
names(liste_mod)
head(liste_mod)
#Buscar modelos 

modelos_gc <- liste_mod %>%
  filter(
    str_detect(
      tolower(paste(
        model.model_name,
        model.description,
        model.geographic_extent
      )),
      "gulf of california|sea of cortez|cortez|baja california"
    )
  )
#Ver modelos del GOC
modelos_gc %>%
  select(model.model_number, model.model_name, model.geographic_extent)
##modelos del GOC, identificar modelos de GOC
ids_manual <- c(242, 450, 543, 482, 483, 239, 425, 238, 325, 241, 307)
ids_gc <- modelos_gc$model.model_number
ids_auto <- ids_gc
ids_final <- unique(c(ids_manual, as.numeric(ids_auto)))

#Buscar 

modelos_final <- liste_mod %>%
  filter(model.model_number %in% ids_final)

modelos_final %>%
  select(model.model_number, model.model_name, model.geographic_extent)
##Automatizar la busqeuda y descarga de inputs 
get_model_input <- function(id) {
  h <- basicTextGatherer()
  
  curlPerform(
    url = paste0(
      "https://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=",
      id
    ),
    writefunction = h$update
  )
  
  data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)
  
  groups <- xpathSApply(data, '//group', function(x) xmlToList(x))
  
  return(groups)
}
##Descraga de inputs los datos del modelo 

modelos_input <- lapply(ids_final, get_model_input)

names(modelos_input) <- ids_final
###Resultado de los modelos y sus inputs
modelos_input[["242"]]#Alto Golfo de California 
modelos_input[["450"]]#Norte Golfo de California 
modelos_input[["482"]]#no encontradoCentro del Golfo de Califronia
modelos_input[["483"]]#no encontradoCentro del Golfo de Califronia
modelos_input[["239"]]#Centro del Golfo de Califronia
modelos_input[["425"]]#no encontradoBahia de La Paz
modelos_input[["238"]]#no encontradoBahia de La Paz
modelos_input[["325"]]#Sur Sinaloa
modelos_input[["241"]]#Sinaloa 
modelos_input[["307"]]#Jalisco, Colima 
##Buscar modelos no dispinibles 
modelos_input[["543"]]##Modelo 543
h <- basicTextGatherer()

curlPerform(
  url = "https://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=543",
  writefunction = h$update
)

cat(h$value())##El modelo no esta disponible para R

modelos_input[["543"]]##Modelo 482
h <- basicTextGatherer()

curlPerform(
  url = "https://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=543",
  writefunction = h$update
)

cat(h$value())##El modelo no esta disponible para R

# Guardar datos de inputs
for(i in seq_along(ids_final)){
  saveRDS(modelos_input[[i]], paste0("modelo_", ids_final[i], ".rds"))
}
#Obtener los valores de outputs para el modelo mymodel
get_model_output <- function(id) {
  h <- basicTextGatherer()
  
  curlPerform(
    url = paste0(
      "https://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=",
      id
    ),
    writefunction = h$update,
    httpheader = c("User-Agent" = "Mozilla/5.0")
  )
  
  data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)
  
  output <- xpathSApply(data, '//group', function(x) xmlToList(x))
  
  return(output)
}
#Descargar outputs
modelos_output <- lapply(ids_final, get_model_output)
names(modelos_output) <- ids_final
###Resultado de los modelos y sus outputs
modelos_output[["242"]]#Alto Golfo de California 
modelos_output[["450"]]#Norte Golfo de California
modelos_output[["543"]]##no encontradoCentro del Golfo de Califronia
modelos_output[["482"]]#no encontradoCentro del Golfo de Califronia
modelos_output[["483"]]#no encontradoCentro del Golfo de Califronia
modelos_output[["239"]]#Centro del Golfo de Califronia
modelos_output[["425"]]#no encontrado#no encontradoBahia de La Paz
modelos_output[["238"]]#no encontrado#no encontradoBahia de La Paz
modelos_output[["325"]]#Sur Sinaloa
modelos_output[["241"]]#Sinaloa
modelos_output[["307"]]#Jalisco, Colima

#Guardar 
for(i in seq_along(ids_final)){
  saveRDS(modelos_output[[i]], paste0("output_modelo_", ids_final[i], ".rds"))
}
#Ver estructura de modelos
str(modelos_output[["242"]], max.level = 2)
modelos_output[["242"]][[1]]
names(modelos_output[["242"]][[1]])
length(modelos_output[["242"]])
modelos_output[["242"]][[1]]
###Convertir modelos a Tabla mejor vista, error al separar columnas
get_model_output <- function(id) {
  h <- basicTextGatherer()
  
  curlPerform(
    url = paste0(
      "https://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=",
      id
    ),
    writefunction = h$update,
    httpheader = c("User-Agent" = "Mozilla/5.0")
  )
  
  data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)
  
  # Extraer valores como texto
  output <- xpathSApply(data, '//group', xmlValue)
  
  return(output)
}


modelos_output <- lapply(ids_final, get_model_output)
names(modelos_output) <- ids_final

modelos_output[["242"]][1:10]

##Separar valores de la tabla 

separar_grupo <- function(x) {
  
  # nombre (texto inicial)
  nombre <- str_extract(x, "^[^0-9]+")
  
  # separar números correctamente
  numeros <- str_extract_all(x, "\\d+\\.\\d+|\\d+")[[1]]
  
  return(c(nombre, numeros))
}

##Aplicar a modelo 242
datos_242 <- lapply(modelos_output[["242"]], separar_grupo)
datos_242[[1]]
#Convertir a tabla 
df_242 <- bind_rows(lapply(datos_242, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
head(df_242)

##Asiganar nombres 
max_len <- max(sapply(datos_242, length))

datos_242_fix <- lapply(datos_242, function(x) {
  length(x) <- max_len
  return(x)
})

df_242 <- as.data.frame(do.call(rbind, datos_242_fix), stringsAsFactors = FALSE)
names(df_242)
df_242_limpio <- df_242[, 1:5]