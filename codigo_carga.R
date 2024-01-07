
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(dplyr)
library(tidyr)
library(openxlsx2)
library(openxlsx)
library(stringr)


# Cargo el filtro y las bases de actualizaciones y errores ----------------


tabla_errores<-read_excel("df4.xlsx")

tabla_actualizaciones<-read_excel("actualizaciones.xlsx")



# Intenta leer el archivo CSV
id_actualizar <- tryCatch({
  read.csv("filtro_series_a_actualizar.csv")
}, error = function(e) {
  # Si hay un error, devuelve NULL (o cualquier otro valor que desees)
  NULL
})

# Verifica si id_actualizar es NULL para decidir si saltar una parte del código
if (is.null(id_actualizar)) {
  # Código a ejecutar si hay un error al leer el archivo
  print("No se selecionó ninguna serie a actualizar")
} else {

# Base de actualizaciones----------------------------------------------
id_actualizar$x <- as.numeric(id_actualizar$x)

    #hago un nuevo id para esta actualización
id_actualizacion_actual <- as.numeric(max(tabla_actualizaciones$id_actualización))+1

    #esto es para que me lo guarde bien como fecha
fecha_hora_str <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")

#esto es para que me guarde bien las bases a actualizar
id_x_str <- paste(id_actualizar$x, collapse = ",")


vector_nuevoregistro_actualizciones <- c(id_actualizacion_actual, paste(id_actualizar$x, collapse = ","), fecha_hora_str)

tabla_actualizaciones <- rbind(tabla_actualizaciones, vector_nuevoregistro_actualizciones)

#Cargo los nombres de los archivos---------------------------------

datos_serie_cambionombre <- readxl::read_excel(
  path="base_series.xlsx",
  col_names=TRUE
)

datos_serie_cambionombre <- datos_serie_cambionombre[,c(1,13)]

for (i in 1:nrow(datos_serie_cambionombre)) {
  #i<-1# Construyendo el nombre de la variable (nombre_1, nombre_2, etc.)
  nombre_variable <- paste0("nombre_serie_", i)
  
  # Asignando el valor de la columna 'nombre_descarga' a la variable con el nombre construido
  assign(nombre_variable, gsub("\"", "", datos_serie_cambionombre$nombre_descarga[i]))
}


# IF UNITARIA/CONJUNTA --------------------------------------------------


if(nrow(id_actualizar)<2){
  
  if (1 == id_actualizar$x) {
    #id <- 1    # Ejecuta el código correspondiente para los IDs en id_actualizar
    # ID 1 --------------------------------------------------------------------
    
    
   ##Error al cargar archivos --------------------------------
    
    # Inicializamos la variable de descripción de error
    descripcion_error1 <- NULL
    id_serie <- 1
    
    # Intentamos leer el primer archivo
    tryCatch({
      tabla_id1 <- readxl::read_excel(
        path=nombre_serie_1,
        sheet="C 1"
      )
    }, error=function(e){
      id_error_reg1 <- 1
      descripcion_error1 <- "Error al cargar archivo nuevo"
      error_consola1 <- e$message
      nuevo_vector <- c(max(tabla_errores$id_error)+1, Sys.time(), id_actualizacion_actual, id_error_reg1, id_serie_actual, error_consola1)
      tabla_errores <- rbind(tabla_errores, nuevo_vector)
    })
    
    # Si no hubo error en el primer archivo, intentamos leer el segundo
    if (is.null(descripcion_error1)) {
      tryCatch({
        base_1_nuestra <- read.csv("series/1_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Estadísticas e indicadores nacionales_Mercado de trabajo_ Remuneración.csv")
        max_fecha_act <- tabla_id1[,1]
      }, error=function(e){
        id_error_reg2 <- 1
        descripcion_error2 <- "Error al cargar archivo viejo"
        error_consola2 <- e$message
      })
    }
    
    # Verificamos si hubo algún error
    if (!is.null(descripcion_error)) {
      
      # Crear un vector con 6 elementos y sumarlo al registro de errores
      nuevo_vector <- c(max(tabla_errores$id_error)+1, Sys.time(), id_actualizacion_actual, id_error_reg2, id_serie_actual, error_consola2)
      tabla_errores <- rbind(tabla_errores, nuevo_vector)
      
      # Aquí puedes agregar el código para saltarte las siguientes operaciones si lo deseas
    } else {
      
      
      max_fecha_act <- tabla_id1[,1]
      
      
      # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
      
      max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
      
      # Convertir el número serial de Excel a fecha
      max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
      
      if(max(max_fecha_act[[1]])>max(base_1_nuestra$fecha)) {
        
        tabla_id1 <-tabla_id1[c(-1,-2,-3,-4,-5,-6),c(1,3,4) ]
        
        
        # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 4 excepto en la fila 2 donde no tiene datos pero cuya fila necesito
        
        tabla_id1 <- tabla_id1 %>%
          filter(!is.na(`...4`) | row_number() == 1)
        
        # elimino la tercer columna que deje temporalmente
        
        tabla_id1 <- tabla_id1 %>%
          select(-3)
        
        
        # Cambio los nombres de las columnas
        
        colnames(tabla_id1) <- c("fecha","valor")
        
        
        
        # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
        
        
        reemplazar_meses <- function(texto) {
          texto <- gsub("ene", "jan", texto)
          texto <- gsub("abr", "apr", texto)
          texto <- gsub("ago", "aug", texto)
          texto <- gsub("dic", "dec", texto)
          return(texto)
        }
        
        tabla_id1$fechas2 <- reemplazar_meses(tabla_id1$fecha)
        
        fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
        
        fechas <- data.frame(Fecha = ymd(fechas))
        
        for(i in c(1:length(tabla_id1$fecha))) {
          if(is.na(as.numeric(tabla_id1[i,3]))){
            fechas[i,1] <-my(tabla_id1[[i,3]])
          }else{
            fechas[[i,1]]<-as.Date(as.numeric(tabla_id1[[i,3]]), origin = "1899/12/30")
          }
        }
        
        tabla_id1$fecha <- fechas$Fecha
        
        tabla_id1$fechas2 <- NULL
        
        
        # agrego una columna para el numero indice de la base
        
        tabla_id1 <- tabla_id1 %>%
          mutate(id_serie=1
          )
        
        # agrego una columna para que me aparezca el mes escrito en letras
        
        tabla_id1 <- tabla_id1 %>%
          mutate(mes=as.character(fecha,format="%B")
          )
        
        # agrego una columna para que me aparezca el año escrito en numeros
        
        tabla_id1 <- tabla_id1 %>%
          mutate(anio=as.character(fecha,format="%Y")
          )
        
        # reordeno las columnas
        
        tabla_id1 <- tabla_id1 %>%
          select(fecha,mes,anio,id_serie,valor)
        
        # convierno los valores de la columna valor en numericos
        
        tabla_id1$valor <- as.numeric(tabla_id1$valor)
        
        # redondeo los valores a 1 decimal
        
        tabla_id1 <- tabla_id1 %>% mutate(valor = round(valor, 0))
        
        
        
        
        
        
        
        
        
        # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
        
        
        base_1_nuestra<-read.csv("series/1_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Estadísticas e indicadores nacionales_Mercado de trabajo_ Remuneración.csv")
        
        #descarto los datos que ya tengo
        
        tabla_id1_bis <- tabla_id1 %>% dplyr::filter(fecha > max(unique(base_1_nuestra$fecha)))
        
        tabla_id1_act <- rbind(base_1_nuestra,tabla_id1_bis)
        
        #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
        
        
        
        #para guardar el archivo con el nombre
        
        datos_series <- read_excel("base_series.xlsx")
        
        
        id <- unique(tabla_id1$id_serie)
        bb <- datos_series %>% filter(id_serie == id)
        tt <- bb[1,8]
        
        tt <- unlist(strsplit(as.character(tt), ","))
        
        tt <- as.vector(tt)
        write.csv(tabla_id1, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding ="UTF-8")
        
        write.csv(base_1_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding ="UTF-8")
        
        
        
        #para actualizar la fecha del ultimo dato de la base_series
        
        diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id1)), as.data.frame(as.matrix(base_1_nuestra)))
        if (nrow(diff_cells) == 0) {
          print("Los dataframes son iguales en cada celda.")
        } else {
          print("Los dataframes tienen diferencias en las siguientes celdas:")
          print(diff_cells)
        }
        
        
        fecha_ult_act <- max(unique(tabla_id1$fecha))
        #le pego la ultima fecha de actualización a la base de series
        
        
        datos_series <- datos_series %>%
          mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id1$id_serie), 
                                        as.character(Sys.Date()), 
                                        as.character(fecha_ult_act))) %>%
          mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id1$id_serie),
                                        as.character(max(tabla_id1$fecha)),
                                        as.character(fecha_ult_dato)))
        
        write.xlsx(datos_series, file = "base_series.xlsx")
        
        print("base 1 ejecutada_1")
        
        
      }
      print("base 1 ejecutada_2")
       
    }
}  
  
  if(2 == id_actualizar) {
    
    # ID 2 --------------------------------------------------------------------
    
    
    tabla_id2 <- readxl::read_excel(
      path=nombre_serie_2,
      sheet="C 1",
      col_names=TRUE
    )
    
    base_2_nuestra<-read.csv("series/2_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Estadísticas e indicadores nacionales_Mercado de trabajo_ Remuneración.csv")
    max_fecha_act <- tabla_id2[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_2_nuestra$fecha)) {
    
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y sexta columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    
    
    tabla_id2 <-tabla_id2[c(-1,-2,-3,-4,-5,-6),c(1,6,4) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 4 excepto en la fila 2 donde no tiene datos pero cuya fila necesito
    
    tabla_id2 <- tabla_id2 %>%
      filter(!is.na(`...4`) | row_number() == 1)
    
    # elimino la tercer columna que deje temporalmente
    
    tabla_id2 <- tabla_id2 %>%
      select(-3)
    
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id2) <- c("fecha","valor")
    
    nombrecol<- c("fecha","valor")
    
    
    colnames(tabla_id2)<- nombrecol
    
    
    # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
    
    

    tabla_id2$fechas2 <- reemplazar_meses(tabla_id2$fecha)
    
    fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
    
    fechas <- data.frame(Fecha = ymd(fechas))
    
    for(i in c(1:length(tabla_id2$fecha))) {
      if(is.na(as.numeric(tabla_id2[i,3]))){
        fechas[i,1] <-my(tabla_id2[[i,3]])
      }else{
        fechas[[i,1]]<-as.Date(as.numeric(tabla_id2[[i,3]]), origin = "1899/12/30")
      }
    }
    
    tabla_id2$fecha <- fechas$Fecha
    
    tabla_id2$fechas2 <- NULL
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id2 <- tabla_id2 %>%
      mutate(id_serie=2
      )
    
    # agrego una columna para que me aparezca el mes escrito en letras
    
    tabla_id2 <- tabla_id2 %>%
      mutate(mes=as.character(fecha,format="%B")
      )
    
    # agrego una columna para que me aparezca el año escrito en numeros
    
    tabla_id2 <- tabla_id2 %>%
      mutate(anio=as.character(fecha,format="%Y")
      )
    
    # reordeno las columnas
    
    tabla_id2 <- tabla_id2 %>%
      select(fecha,mes,anio,id_serie,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id2$valor <- as.numeric(tabla_id2$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id2 <- tabla_id2 %>% mutate(valor = round(valor, 0))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
    
    
    #descarto los datos que ya tengo
    
    tabla_id2_bis <- tabla_id2 %>% dplyr::filter(fecha > max(unique(base_2_nuestra$fecha)))
    
    tabla_id2_act <- rbind(base_2_nuestra,tabla_id2_bis)
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id2$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id2, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE)
    
    write.csv(base_2_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE)
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id2)), as.data.frame(as.matrix(base_2_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    fecha_ult_act <- max(unique(tabla_id2$fecha))
    #le pego la ultima fecha de actualización a la base de series
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id2$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id2$id_serie),
                                    as.character(max(tabla_id2$fecha)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    
    
    print("base 2 ejecutada_1")
    }
    print("base 2 ejecutada_2")
  }
  
  if(3 == id_actualizar) {
  # ID 3 --------------------------------------------------------------------
    
    
    tabla_id3 <- readxl::read_excel(
      path=nombre_serie_3,
      sheet="T.2.1",
      col_names=TRUE
    )
    
    base_3_nuestra<-read.csv("series/3_ANSES_SIPA_Mercado de trabajo_ Asalariados.csv")
    
    max_fecha_act <- tabla_id3[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_3_nuestra$fecha)) {
    # elimino la primer fila que tiene los nombres de las variables en el excel, luego elimino las dos ultimas columnas, la ultima que no tiene datos y la anteultima que en el excel saparecen los totales
    
    
    tabla_id3 <-tabla_id3[-1,c(-8,-9) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id3 <- tabla_id3%>% drop_na ("...3")
    
    
    
    # 2do: Transformar la base de datos
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id3) <- c("fecha","Privado","Publico","Casas_particulares","Autonomos","Monotributistas","Monotributistas_sociales")
    
    
    
    # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
    
    
    
    
    tabla_id3$fechas2 <- reemplazar_meses(tabla_id3$fecha)
    
    fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
    
    fechas <- data.frame(Fecha = ymd(fechas))
    
    for(i in c(1:length(tabla_id3$fecha))) {
      if(is.na(as.numeric(tabla_id3[i,8]))){
        fechas[i,1] <-my(tabla_id3[[i,8]])
      }else{
        fechas[[i,1]]<-as.Date(as.numeric(tabla_id3[[i,8]]), origin = "1899/12/30")
      }
    }
    
    tabla_id3$fecha <- fechas$Fecha
    
    tabla_id3$fechas2 <- NULL
    
    tabla_id3_tidy <- tabla_id3 %>%
      pivot_longer(
        cols=-fecha,
        names_to="sector",
        values_to="valor"
      )
    
    # agrego una columna para el numero indice de la base
    
    tabla_id3_tidy <- tabla_id3_tidy %>%
      mutate(id_serie=3
      )
    
    # agrego una columna para que me aparezca el mes escrito en letras
    
    tabla_id3_tidy <- tabla_id3_tidy %>%
      mutate(mes=as.character(fecha,format="%B")
      )
    
    # agrego una columna para que me aparezca el año escrito en numeros
    
    tabla_id3_tidy <- tabla_id3_tidy %>%
      mutate(anio=as.character(fecha,format="%Y")
      )
    
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id3_tidy <- tabla_id3_tidy[order(tabla_id3_tidy[,2],tabla_id3_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id3_tidy <- tabla_id3_tidy %>%
      select(id_serie,fecha,mes,anio,sector,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id3_tidy$valor <- as.numeric(tabla_id3_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id3_tidy <- tabla_id3_tidy %>% mutate(valor = round(valor, 1))
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
    
    #descarto los datos que ya tengo
    
    tabla_id3_bis <- tabla_id3_tidy %>% dplyr::filter(fecha > max(unique(base_3_nuestra$fecha)))
    
    tabla_id3_act <- rbind(base_3_nuestra,tabla_id3_bis)
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id3_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id3_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_3_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id3_tidy)), as.data.frame(as.matrix(base_3_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    fecha_ult_act <- max(unique(tabla_id3_tidy$fecha))
    #le pego la ultima fecha de actualización a la base de series
    
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id3_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id3_tidy$id_serie),
                                    as.character(max(tabla_id3_tidy$fecha)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    
    print("base 3 ejecutada_1")
    }
    print("base 3 ejecutada_2")
  }
  
  if(4 == id_actualizar) {
    # ID 4 --------------------------------------------------------------------
    
    
    
    tabla_id4 <- readxl::read_excel(
      path=nombre_serie_4,
      sheet="T.2.2",
      col_names=TRUE
    )
    
    
    base_4_nuestra<-read.csv("series/4_ANSES_SIPA_Mercado de trabajo_ Asalariados.csv")
    
    max_fecha_act <- tabla_id4[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_4_nuestra$fecha)) {
    # elimino la primer fila que tiene los nombres de las variables en el excel, luego elimino las dos ultimas columnas, la ultima que no tiene datos y la anteultima que en el excel saparecen los totales
    
    
    tabla_id4 <-tabla_id4[-1,c(-8,-9) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id4 <- tabla_id4%>% drop_na ("...3")
    
    
    
    # 2do: Transformar la base de datos
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id4) <- c("fecha","Privado","Publico","Casas_particulares","Autonomos","Monotributistas","Monotributistas_sociales")
    
    nombrecol<- c("fecha","Privado","Publico","Casas_particulares","Autonomos","Monotributistas","Monotributistas_sociales")
    
    
    colnames(tabla_id4)<- nombrecol
    
    
    # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
    
    
    
    

    tabla_id4$fechas2 <- reemplazar_meses(tabla_id4$fecha)
    
    fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
    
    fechas <- data.frame(Fecha = ymd(fechas))
    
    for(i in c(1:length(tabla_id4$fecha))) {
      if(is.na(as.numeric(tabla_id4[i,8]))){
        fechas[i,1] <-my(tabla_id4[[i,8]])
      }else{
        fechas[[i,1]]<-as.Date(as.numeric(tabla_id4[[i,8]]), origin = "1899/12/30")
      }
    }
    
    tabla_id4$fecha <- fechas$Fecha
    
    tabla_id4$fechas2 <- NULL
    
    tabla_id4_tidy <- tabla_id4 %>%
      pivot_longer(
        cols=-fecha,
        names_to="sector",
        values_to="valor"
      )
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id4_tidy <- tabla_id4_tidy %>%
      mutate(id_serie=4
      )
    
    # agrego una columna para que me aparezca el mes escrito en letras
    
    tabla_id4_tidy <- tabla_id4_tidy %>%
      mutate(mes=as.character(fecha,format="%B")
      )
    
    # agrego una columna para que me aparezca el año escrito en numeros
    
    tabla_id4_tidy <- tabla_id4_tidy %>%
      mutate(anio=as.character(fecha,format="%Y")
      )
    
    
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id4_tidy <- tabla_id4_tidy[order(tabla_id4_tidy[,2],tabla_id4_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id4_tidy <- tabla_id4_tidy %>%
      select(id_serie,fecha,mes,anio,sector,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id4_tidy$valor <- as.numeric(tabla_id4_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id4_tidy <- tabla_id4_tidy %>% mutate(valor = round(valor, 0))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
    base_4_nuestra<-read.csv("series/4_ANSES_SIPA_Mercado de trabajo_ Asalariados.csv")
    
    
    
    #descarto los datos que ya tengo
    
    tabla_id4_bis <- tabla_id4_tidy %>% dplyr::filter(fecha > max(unique(base_4_nuestra$fecha)))
    
    tabla_id4_act <- rbind(base_4_nuestra,tabla_id4_bis)
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id4_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id4_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_4_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")

    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id4_tidy)), as.data.frame(as.matrix(base_4_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    fecha_ult_act <- max(unique(tabla_id4_tidy$fecha))
    #le pego la ultima fecha de actualización a la base de series
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id4_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id4_tidy$id_serie),
                                    as.character(max(tabla_id4_tidy$fecha)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    
    print("base 4 ejecutada_1")
    }
    print("base 4 ejecutada_2")
  }


  if(5 == id_actualizar) {
    # ID 5 --------------------------------------------------------------------
    tabla_id5 <- readxl::read_excel(
      path=nombre_serie_5,
      sheet="C 1.2.",
      col_names=TRUE
    )
    
    base_5_nuestra<-read.csv("series/5_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv", fileEncoding = "ISO-8859-1")
    
    max_fecha_act <- tabla_id5[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_5_nuestra$fecha)) {
    
    # elimino las 4  primeras filas que no sirven, luego elimino las 3 ultimas columnas que no tienen datos
    
    
    tabla_id5 <-tabla_id5[c(-1,-2,-3,-4),c(-12,-13,-14) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id5 <- tabla_id5%>% drop_na ("...3")
    
    
    
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id5) <- c("trim","Población Económicamente Activa - Mujeres","Población Económicamente Activa - Varones","Población Ocupada - Mujeres","Población Ocupada - Varones","Población Desocupada - Mujeres","Población Desocupada - Varones","Población Subocupada - Mujeres","Población Subocupada - Varones","Población Asalariada No Registrada - Mujeres","Población Asalariada No Registrada - Varones")
    
    
    
    # creo una columna llamada trimestre y otra para el año para que me aparezcan los valores correspondientes segun la colmna "trim" que posteriormente elimino
    
    tabla_id5 <- tabla_id5 %>%
      mutate(trimestre = as.integer(str_match(trim, "^(\\d+)° Trim")[, 2]),
             anio =  as.integer(str_extract(trim, "(?<=Trim )\\d+")))
    
    tabla_id5 <- tabla_id5 %>%
      select(-trim)
    
    # le sumo 2000 al año para que este en formato completo
    
    tabla_id5 <- tabla_id5 %>%
      mutate(anio = anio + 2000)
    
    # cambio el formato de la columna trimestre
    
    tabla_id5 <- tabla_id5 %>%
      mutate(trimestre = case_when(
        trimestre == 1 ~ "1º Trim",
        trimestre == 2 ~ "2º Trim",
        trimestre == 3 ~ "3º Trim",
        trimestre == 4 ~ "4º Trim",
        TRUE ~ as.character(trimestre)  # Mantener cualquier otro valor que no coincida con los anteriores
      ))
    
    
    # transformor la base en tidy
    
    
    
    
    tabla_id5_tidy <- tabla_id5 %>%
      pivot_longer(
        cols=-c(trimestre,anio),
        names_to="concepto",
        values_to="valor"
      )
    
    
    tabla_id5_tidy <- tabla_id5_tidy %>%
      separate(col = concepto, into = c("indicador", "sexo"), sep = " - ", remove = TRUE)
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id5_tidy <- tabla_id5_tidy %>%
      mutate(id_serie=5
      )
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id5_tidy <- tabla_id5_tidy[order(tabla_id5_tidy[,3],tabla_id5_tidy[,4],tabla_id5_tidy[,2],tabla_id5_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id5_tidy <- tabla_id5_tidy %>%
      select(id_serie,trimestre,anio,indicador,sexo,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id5_tidy$valor <- as.numeric(tabla_id5_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id5_tidy <- tabla_id5_tidy %>% mutate(valor = round(valor, 0))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
  
    
    #descarto los datos que ya tengo
    
    trimestre_a_numero <- function(trimestre) {
      case_when(
        grepl("1º Trim", trimestre) ~ 1,
        grepl("2º Trim", trimestre) ~ 2,
        grepl("3º Trim", trimestre) ~ 3,
        grepl("4º Trim", trimestre) ~ 4,
        TRUE ~ NA_integer_  # En caso de que no coincida con ningún patrón
      )
    }
    
    # Asume que la base "tabla_id5_tidy" tiene las columnas "trimestre" y "año"
    # y la base "base_5_nuestra" tiene las columnas "trimestre" y "año"
    
    # Combina las columnas de trimestre y año en una nueva columna de fecha en "tabla_id5_tidy"
    tabla_id5_tidy <- tabla_id5_tidy %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    # Combina las columnas de trimestre y año en una nueva columna de fecha en "base_5_nuestra"
    base_5_nuestra <- base_5_nuestra %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    # Ahora puedes aplicar el código original para filtrar las bases por fecha
    tabla_id5_bis <- tabla_id5_tidy %>%
      dplyr::filter(fecha > max(unique(base_5_nuestra$fecha)))
    
    
    tabla_id5_act <- rbind(base_5_nuestra,tabla_id5_bis)
    
    # elimino la columna fecha de la base tabla_id5_tidy y la base base_5_nuestra
    
    tabla_id5_tidy <- tabla_id5_tidy %>%
      select(-fecha)
    base_5_nuestra <- base_5_nuestra %>%
      select(-fecha)
    
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id5_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id5_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_5_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8" )
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id5_tidy)), as.data.frame(as.matrix(base_5_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    tabla_id5_tidy <- tabla_id5_tidy %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    
    fecha_ult_act <- max(unique(tabla_id5_tidy$fecha))
    #le pego la ultima fecha de actualización a la base de series
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id5_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id5_tidy$id_serie),
                                    as.character(max(tabla_id5_tidy$fecha)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    
    print("base 5 ejecutada_1")
    }
    print("base 5 ejecutada_2")
  }
  
  
  if(6 == id_actualizar) {
    # ID 6 --------------------------------------------------------------------
    
    
    
    tabla_id6 <- readxl::read_excel(
      path=nombre_serie_6,
      sheet="C 1.1.",
      col_names=TRUE
    )
    
    base_6_nuestra<-read.csv("series/6_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    max_fecha_act <- tabla_id6[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_6_nuestra$fecha)) {
    
    # elimino las 4  primeras filas que no sirven, luego elimino las 3 ultimas columnas que no tienen datos
    
    
    tabla_id6 <-tabla_id6[c(-1,-2,-3,-4),c(-12,-13,-14) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id6 <- tabla_id6%>% drop_na ("...3")
    
    
    
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id6) <- c("trim","Tasa de Actividad - Mujeres","Tasa de Actividad - Varones","Tasa de Empleo - Mujeres","Tasa de Empleo - Varones","Tasa de Desocupación - Mujeres","Tasa de Desocupación - Varones","Tasa de Subocupación - Mujeres","Tasa de Subocupación - Varones","Tasa de Empleo no Registrado - Mujeres","Tasa de Empleo no Registrado - Varones")
    
    
    
    # creo una columna llamada trimestre y otra para el año para que me aparezcan los valores correspondientes segun la colmna "trim" que posteriormente elimino
    
    tabla_id6 <- tabla_id6 %>%
      mutate(trimestre = as.integer(str_match(trim, "^(\\d+)° Trim")[, 2]),
             anio =  as.integer(str_extract(trim, "(?<=Trim )\\d+")))
    
    tabla_id6 <- tabla_id6 %>%
      select(-trim)
    
    # le sumo 2000 al año para que este en formato completo
    
    tabla_id6 <- tabla_id6 %>%
      mutate(anio = anio + 2000)
    
    # cambio el formato de la columna trimestre
    
    tabla_id6 <- tabla_id6 %>%
      mutate(trimestre = case_when(
        trimestre == 1 ~ "1º Trim",
        trimestre == 2 ~ "2º Trim",
        trimestre == 3 ~ "3º Trim",
        trimestre == 4 ~ "4º Trim",
        TRUE ~ as.character(trimestre)  # Mantener cualquier otro valor que no coincida con los anteriores
      ))
    
    
    # transformor la base en tidy
    
    
    
    
    tabla_id6_tidy <- tabla_id6 %>%
      pivot_longer(
        cols=-c(trimestre,anio),
        names_to="concepto",
        values_to="valor"
      )
    
    
    tabla_id6_tidy <- tabla_id6_tidy %>%
      separate(col = concepto, into = c("indicador", "sexo"), sep = " - ", remove = TRUE)
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id6_tidy <- tabla_id6_tidy %>%
      mutate(id_serie=6
      )
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id6_tidy <- tabla_id6_tidy[order(tabla_id6_tidy[,3],tabla_id6_tidy[,4],tabla_id6_tidy[,2],tabla_id6_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id6_tidy <- tabla_id6_tidy %>%
      select(id_serie,trimestre,anio,indicador,sexo,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id6_tidy$valor <- as.numeric(tabla_id6_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id6_tidy <- tabla_id6_tidy %>% mutate(valor = round(valor, 1))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
   
    
    
    
    #descarto los datos que ya tengo
    
    trimestre_a_numero <- function(trimestre) {
      case_when(
        grepl("1º Trim", trimestre) ~ 1,
        grepl("2º Trim", trimestre) ~ 2,
        grepl("3º Trim", trimestre) ~ 3,
        grepl("4º Trim", trimestre) ~ 4,
        TRUE ~ NA_integer_  # En caso de que no coincida con ningún patrón
      )
    }
    
    # Asume que la base "tabla_id5_tidy" tiene las columnas "trimestre" y "año"
    # y la base "base_5_nuestra" tiene las columnas "trimestre" y "año"
    
    # Combina las columnas de trimestre y año en una nueva columna de fecha en "tabla_id5_tidy"
    tabla_id6_tidy <- tabla_id6_tidy %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    # Combina las columnas de trimestre y año en una nueva columna de fecha en "base_5_nuestra"
    base_6_nuestra <- base_6_nuestra %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    # Ahora puedes aplicar el código original para filtrar las bases por fecha
    tabla_id6_bis <- tabla_id6_tidy %>%
      dplyr::filter(fecha > max(unique(base_6_nuestra$fecha)))
    
    
    tabla_id6_act <- rbind(base_6_nuestra,tabla_id6_bis)
    
    # elimino la columna fecha de la base tabla_id5_tidy y la base base_5_nuestra
    
    tabla_id6_tidy <- tabla_id6_tidy %>%
      select(-fecha)
    base_6_nuestra <- base_6_nuestra %>%
      select(-fecha)
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id6_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id6_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_6_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id6_tidy)), as.data.frame(as.matrix(base_6_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    tabla_id6_tidy <- tabla_id6_tidy %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    
    fecha_ult_act <- max(unique(tabla_id6_tidy$fecha))
    #le pego la ultima fecha de actualización a la base de series
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id6_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id6_tidy$id_serie),
                                    as.character(max(tabla_id6_tidy$fecha)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    print("base 6 ejecutada_1")
    }
    print("base 6 ejecutada_2")
  }
  

  if(7 == id_actualizar) {
    
#ID 7 --------------------------------------------------------------------
    
    
    
    tabla_id7 <- readxl::read_excel(
      path=nombre_serie_7,
      sheet=" C 2.1",
      col_names=TRUE
    )
    
    base_7_nuestra<-read.csv("series/7_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    
    max_fecha_act <- tabla_id7[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_7_nuestra$fecha)) {
    
    
    # elimino las 4  primeras filas que no sirven, luego elimino las ultimas cuatro columnas que tampoco sirven
    
    
    tabla_id7 <-tabla_id7[c(-1,-2,-3,-4),c(-6,-7,-8,-9) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id7 <- tabla_id7%>% drop_na (3)
    
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id7) <- c("anio","Mujeres","Varones","Sin definir","Total")
    
    
    
    # transformor la base en tidy
    
    
    
    
    tabla_id7_tidy <- tabla_id7 %>%
      pivot_longer(
        cols=-c(anio),
        names_to="sexo",
        values_to="valor"
      )
    
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id7_tidy <- tabla_id7_tidy %>%
      mutate(id_serie=7
      )
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id7_tidy <- tabla_id7_tidy[order(tabla_id7_tidy[,2],tabla_id7_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id7_tidy <- tabla_id7_tidy %>%
      select(id_serie,anio,sexo,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id7_tidy$valor <- as.numeric(tabla_id7_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id7_tidy <- tabla_id7_tidy %>% mutate(valor = round(valor, 0))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
    
    
    #descarto los datos que ya tengo
    
    tabla_id7_bis <- tabla_id7_tidy %>% dplyr::filter(anio > max(unique(base_7_nuestra$anio)))
    
    tabla_id7_act <- rbind(base_7_nuestra,tabla_id7_bis)
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id7_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id7_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_7_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id7_tidy)), as.data.frame(as.matrix(base_7_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    fecha_ult_act <- max(unique(tabla_id7_tidy$anio))
    #le pego la ultima fecha de actualización a la base de series
    
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id7_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id7_tidy$id_serie),
                                    as.character(max(tabla_id7_tidy$anio)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    
    
    print("base 7 ejecutada_1")
    
    }
    print("base 7 ejecutada_2")
  }
  
  if(8 == id_actualizar) {
  #ID 8 --------------------------------------------------------------------
    
    
    
    tabla_id8 <- readxl::read_excel(
      path=nombre_serie_8,
      sheet="C1.9",
      col_names=TRUE
    )
    
    base_8_nuestra<-read.csv("series/8_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    
    max_fecha_act <- tabla_id8[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_8_nuestra$fecha)) {
    
    # elimino las 4  primeras filas que no sirven, luego elimino las columnas de brecha y las que no tienen datos
    
    
    tabla_id8 <-tabla_id8[c(-1,-2,-3,-4),c(-4,-7,-10,-13,-14,-15) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id8 <- tabla_id8%>% drop_na ("...3")
    
    
    
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id8) <- c("trim","Ingreso ocupados - Mujeres","Ingreso ocupados - Varones","Ingreso ocupados plenos - Mujeres","Ingreso ocupados plenos - Varones","Ingreso asalariados - Mujeres","Ingreso asalariados - Varones","Ingreso asalariados plenos - Mujeres","Ingreso asalariados plenos - Varones")
    
    
    # creo una columna llamada trimestre y otra para el año para que me aparezcan los valores correspondientes segun la colmna "trim" que posteriormente elimino
    
    tabla_id8 <- tabla_id8 %>%
      mutate(trimestre = as.integer(str_match(trim, "^(\\d+)° Trim")[, 2]),
             anio =  as.integer(str_extract(trim, "(?<=Trim )\\d+")))
    
    tabla_id8 <- tabla_id8 %>%
      select(-trim)
    
    # le sumo 2000 al año para que este en formato completo
    
    tabla_id8 <- tabla_id8 %>%
      mutate(anio = anio + 2000)
    
    # cambio el formato de la columna trimestre
    
    tabla_id8 <- tabla_id8 %>%
      mutate(trimestre = case_when(
        trimestre == 1 ~ "1º Trim",
        trimestre == 2 ~ "2º Trim",
        trimestre == 3 ~ "3º Trim",
        trimestre == 4 ~ "4º Trim",
        TRUE ~ as.character(trimestre)  # Mantener cualquier otro valor que no coincida con los anteriores
      ))
    
    
    # transformor la base en tidy
    
    
    
    
    tabla_id8_tidy <- tabla_id8 %>%
      pivot_longer(
        cols=-c(trimestre,anio),
        names_to="concepto",
        values_to="valor"
      )
    
    
    tabla_id8_tidy <- tabla_id8_tidy %>%
      separate(col = concepto, into = c("indicador", "sexo"), sep = " - ", remove = TRUE)
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id8_tidy <- tabla_id8_tidy %>%
      mutate(id_serie=8
      )
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id8_tidy <- tabla_id8_tidy[order(tabla_id8_tidy[,3],tabla_id8_tidy[,4],tabla_id8_tidy[,2],tabla_id8_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id8_tidy <- tabla_id8_tidy %>%
      select(id_serie,trimestre,anio,indicador,sexo,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id8_tidy$valor <- as.numeric(tabla_id8_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id8_tidy <- tabla_id8_tidy %>% mutate(valor = round(valor, 0))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
    
    
    #descarto los datos que ya tengo
    
    trimestre_a_numero <- function(trimestre) {
      case_when(
        grepl("1º Trim", trimestre) ~ 1,
        grepl("2º Trim", trimestre) ~ 2,
        grepl("3º Trim", trimestre) ~ 3,
        grepl("4º Trim", trimestre) ~ 4,
        TRUE ~ NA_integer_  # En caso de que no coincida con ningún patrón
      )
    }
    
    # Asume que la base "tabla_id5_tidy" tiene las columnas "trimestre" y "año"
    # y la base "base_5_nuestra" tiene las columnas "trimestre" y "año"
    
    # Combina las columnas de trimestre y año en una nueva columna de fecha en "tabla_id5_tidy"
    tabla_id8_tidy <- tabla_id8_tidy %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    # Combina las columnas de trimestre y año en una nueva columna de fecha en "base_5_nuestra"
    base_8_nuestra <- base_8_nuestra %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    # Ahora puedes aplicar el código original para filtrar las bases por fecha
    tabla_id8_bis <- tabla_id8_tidy %>%
      dplyr::filter(fecha > max(unique(base_8_nuestra$fecha)))
    
    
    tabla_id8_act <- rbind(base_8_nuestra,tabla_id8_bis)
    
    # elimino la columna fecha de la base tabla_id5_tidy y la base base_5_nuestra
    
    tabla_id8_tidy <- tabla_id8_tidy %>%
      select(-fecha)
    base_8_nuestra <- base_8_nuestra %>%
      select(-fecha)
    
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id8_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id8_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_8_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id8_tidy)), as.data.frame(as.matrix(base_8_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    tabla_id8_tidy <- tabla_id8_tidy %>%
      mutate(fecha = as.Date(paste0(anio, "-",
                                    trimestre_a_numero(trimestre) * 3 - 2, "-01")))
    
    
    fecha_ult_act <- max(unique(tabla_id8_tidy$fecha))
    #le pego la ultima fecha de actualización a la base de series
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id8_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id8_tidy$id_serie),
                                    as.character(max(tabla_id8_tidy$fecha)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    print("base 8 ejecutada_1")
    
    } 
    print("base 8 ejecutada_2")
  }
  
  
  if(9 == id_actualizar) {
#ID 9 --------------------------------------------------------------------
    
    
    
    tabla_id9 <- readxl::read_excel(
      path=nombre_serie_9,
      sheet="C 3.1",
      col_names=TRUE
    )
    
    base_9_nuestra<-read.csv("series/9_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    
    max_fecha_act <- tabla_id9[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_9_nuestra$fecha)) {
    
    
    
    # elimino las 4  primeras filas que no sirven, luego elimino las ultimas tres columnas que tampoco sirven
    
    
    tabla_id9 <-tabla_id9[c(-1,-2,-3,-4),c(-4,-5,-6) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id9 <- tabla_id9%>% drop_na (3)
    
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id9) <- c("anio","Mujeres","Varones")
    
    
    
    # transformor la base en tidy
    
    
    
    
    tabla_id9_tidy <- tabla_id9 %>%
      pivot_longer(
        cols=-c(anio),
        names_to="sexo",
        values_to="valor"
      )
    
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id9_tidy <- tabla_id9_tidy %>%
      mutate(id_serie=9
      )
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id9_tidy <- tabla_id9_tidy[order(tabla_id9_tidy[,2],tabla_id9_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id9_tidy <- tabla_id9_tidy %>%
      select(id_serie,anio,sexo,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id9_tidy$valor <- as.numeric(tabla_id9_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id9_tidy <- tabla_id9_tidy %>% mutate(valor = round(valor, 1))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
    
    
    #descarto los datos que ya tengo
    
    tabla_id9_bis <- tabla_id9_tidy %>% dplyr::filter(anio > max(unique(base_9_nuestra$anio)))
    
    tabla_id9_act <- rbind(base_9_nuestra,tabla_id9_bis)
    
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id9_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id9_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_9_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id9_tidy)), as.data.frame(as.matrix(base_9_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    fecha_ult_act <- max(unique(tabla_id9_tidy$anio))
    #le pego la ultima fecha de actualización a la base de series
    
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id9_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id9_tidy$id_serie),
                                    as.character(max(tabla_id9_tidy$anio)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    print("base 9 ejecutada_1")
    }
    print("base 9 ejecutada_2")
  }
  
  
  
  
} else {
  
  
  if (1 %in% id_actualizar$x) {
    #id <- 1    # Ejecuta el código correspondiente para los IDs en id_actualizar
    #id <- 1    # Ejecuta el código correspondiente para los IDs en id_actualizar
    # ID 1 --------------------------------------------------------------------
    
    
    tabla_id1 <- readxl::read_excel(
      path=nombre_serie_1,
      sheet="C 1"
    )
    
    base_1_nuestra<-read.csv("series/1_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Estadísticas e indicadores nacionales_Mercado de trabajo_ Remuneración.csv")
    max_fecha_act <- tabla_id1[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_1_nuestra$fecha)) {
      
      tabla_id1 <-tabla_id1[c(-1,-2,-3,-4,-5,-6),c(1,3,4) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 4 excepto en la fila 2 donde no tiene datos pero cuya fila necesito
      
      tabla_id1 <- tabla_id1 %>%
        filter(!is.na(`...4`) | row_number() == 1)
      
      # elimino la tercer columna que deje temporalmente
      
      tabla_id1 <- tabla_id1 %>%
        select(-3)
      
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id1) <- c("fecha","valor")
      
      
      
      # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
      
      
      reemplazar_meses <- function(texto) {
        texto <- gsub("ene", "jan", texto)
        texto <- gsub("abr", "apr", texto)
        texto <- gsub("ago", "aug", texto)
        texto <- gsub("dic", "dec", texto)
        return(texto)
      }
      
      tabla_id1$fechas2 <- reemplazar_meses(tabla_id1$fecha)
      
      fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
      
      fechas <- data.frame(Fecha = ymd(fechas))
      
      for(i in c(1:length(tabla_id1$fecha))) {
        if(is.na(as.numeric(tabla_id1[i,3]))){
          fechas[i,1] <-my(tabla_id1[[i,3]])
        }else{
          fechas[[i,1]]<-as.Date(as.numeric(tabla_id1[[i,3]]), origin = "1899/12/30")
        }
      }
      
      tabla_id1$fecha <- fechas$Fecha
      
      tabla_id1$fechas2 <- NULL
      
      
      # agrego una columna para el numero indice de la base
      
      tabla_id1 <- tabla_id1 %>%
        mutate(id_serie=1
        )
      
      # agrego una columna para que me aparezca el mes escrito en letras
      
      tabla_id1 <- tabla_id1 %>%
        mutate(mes=as.character(fecha,format="%B")
        )
      
      # agrego una columna para que me aparezca el año escrito en numeros
      
      tabla_id1 <- tabla_id1 %>%
        mutate(anio=as.character(fecha,format="%Y")
        )
      
      # reordeno las columnas
      
      tabla_id1 <- tabla_id1 %>%
        select(fecha,mes,anio,id_serie,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id1$valor <- as.numeric(tabla_id1$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id1 <- tabla_id1 %>% mutate(valor = round(valor, 0))
      
      
      
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      base_1_nuestra<-read.csv("series/1_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Estadísticas e indicadores nacionales_Mercado de trabajo_ Remuneración.csv")
      
      #descarto los datos que ya tengo
      
      tabla_id1_bis <- tabla_id1 %>% dplyr::filter(fecha > max(unique(base_1_nuestra$fecha)))
      
      tabla_id1_act <- rbind(base_1_nuestra,tabla_id1_bis)
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id1$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id1, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding ="UTF-8")
      
      write.csv(base_1_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding ="UTF-8")
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id1)), as.data.frame(as.matrix(base_1_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      fecha_ult_act <- max(unique(tabla_id1$fecha))
      #le pego la ultima fecha de actualización a la base de series
      
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id1$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id1$id_serie),
                                      as.character(max(tabla_id1$fecha)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      
      print("base 1 ejecutada_1")
      
      
    }
    print("base 1 ejecutada_2")
    
    
  }
  
  
  if(2 %in% id_actualizar) {
    
    # ID 2 --------------------------------------------------------------------
    
    
    tabla_id2 <- readxl::read_excel(
      path=nombre_serie_2,
      sheet="C 1",
      col_names=TRUE
    )
    
    base_2_nuestra<-read.csv("series/2_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Estadísticas e indicadores nacionales_Mercado de trabajo_ Remuneración.csv")
    max_fecha_act <- tabla_id2[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_2_nuestra$fecha)) {
      
      # elimino las primeras filas que no sirven, y me quedo solo con la primer y sexta columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
      
      
      tabla_id2 <-tabla_id2[c(-1,-2,-3,-4,-5,-6),c(1,6,4) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 4 excepto en la fila 2 donde no tiene datos pero cuya fila necesito
      
      tabla_id2 <- tabla_id2 %>%
        filter(!is.na(`...4`) | row_number() == 1)
      
      # elimino la tercer columna que deje temporalmente
      
      tabla_id2 <- tabla_id2 %>%
        select(-3)
      
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id2) <- c("fecha","valor")
      
      nombrecol<- c("fecha","valor")
      
      
      colnames(tabla_id2)<- nombrecol
      
      
      # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
      
      
      reemplazar_meses <- function(texto) {
        texto <- gsub("ene", "jan", texto)
        texto <- gsub("abr", "apr", texto)
        texto <- gsub("ago", "aug", texto)
        texto <- gsub("dic", "dec", texto)
        return(texto)
      }
      
      tabla_id2$fechas2 <- reemplazar_meses(tabla_id2$fecha)
      
      fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
      
      fechas <- data.frame(Fecha = ymd(fechas))
      
      for(i in c(1:length(tabla_id2$fecha))) {
        if(is.na(as.numeric(tabla_id2[i,3]))){
          fechas[i,1] <-my(tabla_id2[[i,3]])
        }else{
          fechas[[i,1]]<-as.Date(as.numeric(tabla_id2[[i,3]]), origin = "1899/12/30")
        }
      }
      
      tabla_id2$fecha <- fechas$Fecha
      
      tabla_id2$fechas2 <- NULL
      
      
      # agrego una columna para el numero indice de la base
      
      tabla_id2 <- tabla_id2 %>%
        mutate(id_serie=2
        )
      
      # agrego una columna para que me aparezca el mes escrito en letras
      
      tabla_id2 <- tabla_id2 %>%
        mutate(mes=as.character(fecha,format="%B")
        )
      
      # agrego una columna para que me aparezca el año escrito en numeros
      
      tabla_id2 <- tabla_id2 %>%
        mutate(anio=as.character(fecha,format="%Y")
        )
      
      # reordeno las columnas
      
      tabla_id2 <- tabla_id2 %>%
        select(fecha,mes,anio,id_serie,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id2$valor <- as.numeric(tabla_id2$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id2 <- tabla_id2 %>% mutate(valor = round(valor, 0))
      
      
      
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      
      
      #descarto los datos que ya tengo
      
      tabla_id2_bis <- tabla_id2 %>% dplyr::filter(fecha > max(unique(base_2_nuestra$fecha)))
      
      tabla_id2_act <- rbind(base_2_nuestra,tabla_id2_bis)
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id2$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id2, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE)
      
      write.csv(base_2_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE)
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id2)), as.data.frame(as.matrix(base_2_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      fecha_ult_act <- max(unique(tabla_id2$fecha))
      #le pego la ultima fecha de actualización a la base de series
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id2$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id2$id_serie),
                                      as.character(max(tabla_id2$fecha)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      
      
      print("base 2 ejecutada_1")
    }
    print("base 2 ejecutada_2")
    
    
  }
  
  if(3 %in% id_actualizar) {
    # ID 3 --------------------------------------------------------------------
    
    

    
    tabla_id3 <- readxl::read_excel(
      path=nombre_serie_3,
      sheet="T.2.1",
      col_names=TRUE
    )
    
    base_3_nuestra<-read.csv("series/3_ANSES_SIPA_Mercado de trabajo_ Asalariados.csv")
    
    max_fecha_act <- tabla_id3[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_3_nuestra$fecha)) {
      # elimino la primer fila que tiene los nombres de las variables en el excel, luego elimino las dos ultimas columnas, la ultima que no tiene datos y la anteultima que en el excel saparecen los totales
      
      
      tabla_id3 <-tabla_id3[-1,c(-8,-9) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
      
      tabla_id3 <- tabla_id3%>% drop_na ("...3")
      
      
      
      # 2do: Transformar la base de datos
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id3) <- c("fecha","Privado","Publico","Casas_particulares","Autonomos","Monotributistas","Monotributistas_sociales")
      
      
      
      # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
      
      
      
      
      reemplazar_meses <- function(texto) {
        texto <- gsub("ene", "jan", texto)
        texto <- gsub("abr", "apr", texto)
        texto <- gsub("ago", "aug", texto)
        texto <- gsub("dic", "dec", texto)
        return(texto)
      }
      
      tabla_id3$fechas2 <- reemplazar_meses(tabla_id3$fecha)
      
      fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
      
      fechas <- data.frame(Fecha = ymd(fechas))
      
      for(i in c(1:length(tabla_id3$fecha))) {
        if(is.na(as.numeric(tabla_id3[i,8]))){
          fechas[i,1] <-my(tabla_id3[[i,8]])
        }else{
          fechas[[i,1]]<-as.Date(as.numeric(tabla_id3[[i,8]]), origin = "1899/12/30")
        }
      }
      
      tabla_id3$fecha <- fechas$Fecha
      
      tabla_id3$fechas2 <- NULL
      
      tabla_id3_tidy <- tabla_id3 %>%
        pivot_longer(
          cols=-fecha,
          names_to="sector",
          values_to="valor"
        )
      
      # agrego una columna para el numero indice de la base
      
      tabla_id3_tidy <- tabla_id3_tidy %>%
        mutate(id_serie=3
        )
      
      # agrego una columna para que me aparezca el mes escrito en letras
      
      tabla_id3_tidy <- tabla_id3_tidy %>%
        mutate(mes=as.character(fecha,format="%B")
        )
      
      # agrego una columna para que me aparezca el año escrito en numeros
      
      tabla_id3_tidy <- tabla_id3_tidy %>%
        mutate(anio=as.character(fecha,format="%Y")
        )
      
      
      # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
      
      tabla_id3_tidy <- tabla_id3_tidy[order(tabla_id3_tidy[,2],tabla_id3_tidy[,1]),]
      
      
      # reordeno las columnas
      
      tabla_id3_tidy <- tabla_id3_tidy %>%
        select(id_serie,fecha,mes,anio,sector,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id3_tidy$valor <- as.numeric(tabla_id3_tidy$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id3_tidy <- tabla_id3_tidy %>% mutate(valor = round(valor, 1))
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      
      #descarto los datos que ya tengo
      
      tabla_id3_bis <- tabla_id3_tidy %>% dplyr::filter(fecha > max(unique(base_3_nuestra$fecha)))
      
      tabla_id3_act <- rbind(base_3_nuestra,tabla_id3_bis)
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id3_tidy$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id3_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      write.csv(base_3_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id3_tidy)), as.data.frame(as.matrix(base_3_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      fecha_ult_act <- max(unique(tabla_id3_tidy$fecha))
      #le pego la ultima fecha de actualización a la base de series
      
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id3_tidy$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id3_tidy$id_serie),
                                      as.character(max(tabla_id3_tidy$fecha)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      
      print("base 3 ejecutada_1")
    }
    print("base 3 ejecutada_2")
  }
  
  if(4 %in% id_actualizar) {
    # ID 4 --------------------------------------------------------------------
    
    
    
    tabla_id4 <- readxl::read_excel(
      path=nombre_serie_4,
      sheet="T.2.2",
      col_names=TRUE
    )
    
    
    base_4_nuestra<-read.csv("series/4_ANSES_SIPA_Mercado de trabajo_ Asalariados.csv")
    
    max_fecha_act <- tabla_id4[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_4_nuestra$fecha)) {
      # elimino la primer fila que tiene los nombres de las variables en el excel, luego elimino las dos ultimas columnas, la ultima que no tiene datos y la anteultima que en el excel saparecen los totales
      
      
      tabla_id4 <-tabla_id4[-1,c(-8,-9) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
      
      tabla_id4 <- tabla_id4%>% drop_na ("...3")
      
      
      
      # 2do: Transformar la base de datos
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id4) <- c("fecha","Privado","Publico","Casas_particulares","Autonomos","Monotributistas","Monotributistas_sociales")
      
      nombrecol<- c("fecha","Privado","Publico","Casas_particulares","Autonomos","Monotributistas","Monotributistas_sociales")
      
      
      colnames(tabla_id4)<- nombrecol
      
      
      # Transformamos la primera columna de fecha (que tiene formato de fecha y hora) para que sea solo de fecha (sin la hora)
      
      
      
      
      reemplazar_meses <- function(texto) {
        texto <- gsub("ene", "jan", texto)
        texto <- gsub("abr", "apr", texto)
        texto <- gsub("ago", "aug", texto)
        texto <- gsub("dic", "dec", texto)
        return(texto)
      }
      
      tabla_id4$fechas2 <- reemplazar_meses(tabla_id4$fecha)
      
      fechas <- seq(as.Date("2023-01-01"), by = "1 day", length.out = 136)
      
      fechas <- data.frame(Fecha = ymd(fechas))
      
      for(i in c(1:length(tabla_id4$fecha))) {
        if(is.na(as.numeric(tabla_id4[i,8]))){
          fechas[i,1] <-my(tabla_id4[[i,8]])
        }else{
          fechas[[i,1]]<-as.Date(as.numeric(tabla_id4[[i,8]]), origin = "1899/12/30")
        }
      }
      
      tabla_id4$fecha <- fechas$Fecha
      
      tabla_id4$fechas2 <- NULL
      
      tabla_id4_tidy <- tabla_id4 %>%
        pivot_longer(
          cols=-fecha,
          names_to="sector",
          values_to="valor"
        )
      
      
      # agrego una columna para el numero indice de la base
      
      tabla_id4_tidy <- tabla_id4_tidy %>%
        mutate(id_serie=4
        )
      
      # agrego una columna para que me aparezca el mes escrito en letras
      
      tabla_id4_tidy <- tabla_id4_tidy %>%
        mutate(mes=as.character(fecha,format="%B")
        )
      
      # agrego una columna para que me aparezca el año escrito en numeros
      
      tabla_id4_tidy <- tabla_id4_tidy %>%
        mutate(anio=as.character(fecha,format="%Y")
        )
      
      
      
      # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
      
      tabla_id4_tidy <- tabla_id4_tidy[order(tabla_id4_tidy[,2],tabla_id4_tidy[,1]),]
      
      
      # reordeno las columnas
      
      tabla_id4_tidy <- tabla_id4_tidy %>%
        select(id_serie,fecha,mes,anio,sector,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id4_tidy$valor <- as.numeric(tabla_id4_tidy$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id4_tidy <- tabla_id4_tidy %>% mutate(valor = round(valor, 0))
      
      
      
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      base_4_nuestra<-read.csv("series/4_ANSES_SIPA_Mercado de trabajo_ Asalariados.csv")
      
      
      
      #descarto los datos que ya tengo
      
      tabla_id4_bis <- tabla_id4_tidy %>% dplyr::filter(fecha > max(unique(base_4_nuestra$fecha)))
      
      tabla_id4_act <- rbind(base_4_nuestra,tabla_id4_bis)
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id4_tidy$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id4_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      write.csv(base_4_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id4_tidy)), as.data.frame(as.matrix(base_4_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      fecha_ult_act <- max(unique(tabla_id4_tidy$fecha))
      #le pego la ultima fecha de actualización a la base de series
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id4_tidy$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id4_tidy$id_serie),
                                      as.character(max(tabla_id4_tidy$fecha)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      
      print("base 4 ejecutada_1")
    }
    print("base 4 ejecutada_2")
    
  }
  
  if(5 %in% id_actualizar) {
    # ID 5 --------------------------------------------------------------------
    tabla_id5 <- readxl::read_excel(
      path=nombre_serie_5,
      sheet="C 1.2.",
      col_names=TRUE
    )
    
    base_5_nuestra<-read.csv("series/5_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv", fileEncoding = "ISO-8859-1")
    
    max_fecha_act <- tabla_id5[,1]
    
    # Convertir el número serial de Excel a fecha
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_5_nuestra$fecha)) {
      
      # elimino las 4  primeras filas que no sirven, luego elimino las 3 ultimas columnas que no tienen datos
      
      
      tabla_id5 <-tabla_id5[c(-1,-2,-3,-4),c(-12,-13,-14) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
      
      tabla_id5 <- tabla_id5%>% drop_na ("...3")
      
      
      
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id5) <- c("trim","Población Económicamente Activa - Mujeres","Población Económicamente Activa - Varones","Población Ocupada - Mujeres","Población Ocupada - Varones","Población Desocupada - Mujeres","Población Desocupada - Varones","Población Subocupada - Mujeres","Población Subocupada - Varones","Población Asalariada No Registrada - Mujeres","Población Asalariada No Registrada - Varones")
      
      
      
      # creo una columna llamada trimestre y otra para el año para que me aparezcan los valores correspondientes segun la colmna "trim" que posteriormente elimino
      
      tabla_id5 <- tabla_id5 %>%
        mutate(trimestre = as.integer(str_match(trim, "^(\\d+)° Trim")[, 2]),
               anio =  as.integer(str_extract(trim, "(?<=Trim )\\d+")))
      
      tabla_id5 <- tabla_id5 %>%
        select(-trim)
      
      # le sumo 2000 al año para que este en formato completo
      
      tabla_id5 <- tabla_id5 %>%
        mutate(anio = anio + 2000)
      
      # cambio el formato de la columna trimestre
      
      tabla_id5 <- tabla_id5 %>%
        mutate(trimestre = case_when(
          trimestre == 1 ~ "1º Trim",
          trimestre == 2 ~ "2º Trim",
          trimestre == 3 ~ "3º Trim",
          trimestre == 4 ~ "4º Trim",
          TRUE ~ as.character(trimestre)  # Mantener cualquier otro valor que no coincida con los anteriores
        ))
      
      
      # transformor la base en tidy
      
      
      
      
      tabla_id5_tidy <- tabla_id5 %>%
        pivot_longer(
          cols=-c(trimestre,anio),
          names_to="concepto",
          values_to="valor"
        )
      
      
      tabla_id5_tidy <- tabla_id5_tidy %>%
        separate(col = concepto, into = c("indicador", "sexo"), sep = " - ", remove = TRUE)
      
      
      # agrego una columna para el numero indice de la base
      
      tabla_id5_tidy <- tabla_id5_tidy %>%
        mutate(id_serie=5
        )
      
      # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
      
      tabla_id5_tidy <- tabla_id5_tidy[order(tabla_id5_tidy[,3],tabla_id5_tidy[,4],tabla_id5_tidy[,2],tabla_id5_tidy[,1]),]
      
      
      # reordeno las columnas
      
      tabla_id5_tidy <- tabla_id5_tidy %>%
        select(id_serie,trimestre,anio,indicador,sexo,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id5_tidy$valor <- as.numeric(tabla_id5_tidy$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id5_tidy <- tabla_id5_tidy %>% mutate(valor = round(valor, 0))
      
      
      
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      
      
      #descarto los datos que ya tengo
      
      trimestre_a_numero <- function(trimestre) {
        case_when(
          grepl("1º Trim", trimestre) ~ 1,
          grepl("2º Trim", trimestre) ~ 2,
          grepl("3º Trim", trimestre) ~ 3,
          grepl("4º Trim", trimestre) ~ 4,
          TRUE ~ NA_integer_  # En caso de que no coincida con ningún patrón
        )
      }
      
      # Asume que la base "tabla_id5_tidy" tiene las columnas "trimestre" y "año"
      # y la base "base_5_nuestra" tiene las columnas "trimestre" y "año"
      
      # Combina las columnas de trimestre y año en una nueva columna de fecha en "tabla_id5_tidy"
      tabla_id5_tidy <- tabla_id5_tidy %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      # Combina las columnas de trimestre y año en una nueva columna de fecha en "base_5_nuestra"
      base_5_nuestra <- base_5_nuestra %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      # Ahora puedes aplicar el código original para filtrar las bases por fecha
      tabla_id5_bis <- tabla_id5_tidy %>%
        dplyr::filter(fecha > max(unique(base_5_nuestra$fecha)))
      
      
      tabla_id5_act <- rbind(base_5_nuestra,tabla_id5_bis)
      
      # elimino la columna fecha de la base tabla_id5_tidy y la base base_5_nuestra
      
      tabla_id5_tidy <- tabla_id5_tidy %>%
        select(-fecha)
      base_5_nuestra <- base_5_nuestra %>%
        select(-fecha)
      
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id5_tidy$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id5_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      write.csv(base_5_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8" )
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id5_tidy)), as.data.frame(as.matrix(base_5_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      tabla_id5_tidy <- tabla_id5_tidy %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      
      fecha_ult_act <- max(unique(tabla_id5_tidy$fecha))
      #le pego la ultima fecha de actualización a la base de series
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id5_tidy$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id5_tidy$id_serie),
                                      as.character(max(tabla_id5_tidy$fecha)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      
      print("base 5 ejecutada_1")
    }
    print("base 5 ejecutada_2")
  }
  
  if(6 %in% id_actualizar) {
    # ID 6 --------------------------------------------------------------------
    
    
    
    tabla_id6 <- readxl::read_excel(
      path=nombre_serie_6,
      sheet="C 1.1.",
      col_names=TRUE
    )
    
    base_6_nuestra<-read.csv("series/6_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    max_fecha_act <- tabla_id6[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_6_nuestra$fecha)) {
      
      # elimino las 4  primeras filas que no sirven, luego elimino las 3 ultimas columnas que no tienen datos
      
      
      tabla_id6 <-tabla_id6[c(-1,-2,-3,-4),c(-12,-13,-14) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
      
      tabla_id6 <- tabla_id6%>% drop_na ("...3")
      
      
      
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id6) <- c("trim","Tasa de Actividad - Mujeres","Tasa de Actividad - Varones","Tasa de Empleo - Mujeres","Tasa de Empleo - Varones","Tasa de Desocupación - Mujeres","Tasa de Desocupación - Varones","Tasa de Subocupación - Mujeres","Tasa de Subocupación - Varones","Tasa de Empleo no Registrado - Mujeres","Tasa de Empleo no Registrado - Varones")
      
      
      
      # creo una columna llamada trimestre y otra para el año para que me aparezcan los valores correspondientes segun la colmna "trim" que posteriormente elimino
      
      tabla_id6 <- tabla_id6 %>%
        mutate(trimestre = as.integer(str_match(trim, "^(\\d+)° Trim")[, 2]),
               anio =  as.integer(str_extract(trim, "(?<=Trim )\\d+")))
      
      tabla_id6 <- tabla_id6 %>%
        select(-trim)
      
      # le sumo 2000 al año para que este en formato completo
      
      tabla_id6 <- tabla_id6 %>%
        mutate(anio = anio + 2000)
      
      # cambio el formato de la columna trimestre
      
      tabla_id6 <- tabla_id6 %>%
        mutate(trimestre = case_when(
          trimestre == 1 ~ "1º Trim",
          trimestre == 2 ~ "2º Trim",
          trimestre == 3 ~ "3º Trim",
          trimestre == 4 ~ "4º Trim",
          TRUE ~ as.character(trimestre)  # Mantener cualquier otro valor que no coincida con los anteriores
        ))
      
      
      # transformor la base en tidy
      
      
      
      
      tabla_id6_tidy <- tabla_id6 %>%
        pivot_longer(
          cols=-c(trimestre,anio),
          names_to="concepto",
          values_to="valor"
        )
      
      
      tabla_id6_tidy <- tabla_id6_tidy %>%
        separate(col = concepto, into = c("indicador", "sexo"), sep = " - ", remove = TRUE)
      
      
      # agrego una columna para el numero indice de la base
      
      tabla_id6_tidy <- tabla_id6_tidy %>%
        mutate(id_serie=6
        )
      
      # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
      
      tabla_id6_tidy <- tabla_id6_tidy[order(tabla_id6_tidy[,3],tabla_id6_tidy[,4],tabla_id6_tidy[,2],tabla_id6_tidy[,1]),]
      
      
      # reordeno las columnas
      
      tabla_id6_tidy <- tabla_id6_tidy %>%
        select(id_serie,trimestre,anio,indicador,sexo,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id6_tidy$valor <- as.numeric(tabla_id6_tidy$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id6_tidy <- tabla_id6_tidy %>% mutate(valor = round(valor, 1))
      
      
      
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      
      
      
      
      #descarto los datos que ya tengo
      
      trimestre_a_numero <- function(trimestre) {
        case_when(
          grepl("1º Trim", trimestre) ~ 1,
          grepl("2º Trim", trimestre) ~ 2,
          grepl("3º Trim", trimestre) ~ 3,
          grepl("4º Trim", trimestre) ~ 4,
          TRUE ~ NA_integer_  # En caso de que no coincida con ningún patrón
        )
      }
      
      # Asume que la base "tabla_id5_tidy" tiene las columnas "trimestre" y "año"
      # y la base "base_5_nuestra" tiene las columnas "trimestre" y "año"
      
      # Combina las columnas de trimestre y año en una nueva columna de fecha en "tabla_id5_tidy"
      tabla_id6_tidy <- tabla_id6_tidy %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      # Combina las columnas de trimestre y año en una nueva columna de fecha en "base_5_nuestra"
      base_6_nuestra <- base_6_nuestra %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      # Ahora puedes aplicar el código original para filtrar las bases por fecha
      tabla_id6_bis <- tabla_id6_tidy %>%
        dplyr::filter(fecha > max(unique(base_6_nuestra$fecha)))
      
      
      tabla_id6_act <- rbind(base_6_nuestra,tabla_id6_bis)
      
      # elimino la columna fecha de la base tabla_id5_tidy y la base base_5_nuestra
      
      tabla_id6_tidy <- tabla_id6_tidy %>%
        select(-fecha)
      base_6_nuestra <- base_6_nuestra %>%
        select(-fecha)
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id6_tidy$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id6_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      write.csv(base_6_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id6_tidy)), as.data.frame(as.matrix(base_6_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      tabla_id6_tidy <- tabla_id6_tidy %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      
      fecha_ult_act <- max(unique(tabla_id6_tidy$fecha))
      #le pego la ultima fecha de actualización a la base de series
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id6_tidy$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id6_tidy$id_serie),
                                      as.character(max(tabla_id6_tidy$fecha)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      print("base 6 ejecutada_1")
    }
    print("base 6 ejecutada_2")
  }
  
  if(7 %in% id_actualizar) {
    
    # ID 7 --------------------------------------------------------------------
    
    
    
    
    tabla_id7 <- readxl::read_excel(
      path=nombre_serie_7,
      sheet=" C 2.1",
      col_names=TRUE
    )
    
    base_7_nuestra<-read.csv("series/7_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    
    max_fecha_act <- tabla_id7[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_7_nuestra$fecha)) {
      
      
      # elimino las 4  primeras filas que no sirven, luego elimino las ultimas cuatro columnas que tampoco sirven
      
      
      tabla_id7 <-tabla_id7[c(-1,-2,-3,-4),c(-6,-7,-8,-9) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
      
      tabla_id7 <- tabla_id7%>% drop_na (3)
      
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id7) <- c("anio","Mujeres","Varones","Sin definir","Total")
      
      
      
      # transformor la base en tidy
      
      
      
      
      tabla_id7_tidy <- tabla_id7 %>%
        pivot_longer(
          cols=-c(anio),
          names_to="sexo",
          values_to="valor"
        )
      
      
      
      # agrego una columna para el numero indice de la base
      
      tabla_id7_tidy <- tabla_id7_tidy %>%
        mutate(id_serie=7
        )
      
      # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
      
      tabla_id7_tidy <- tabla_id7_tidy[order(tabla_id7_tidy[,2],tabla_id7_tidy[,1]),]
      
      
      # reordeno las columnas
      
      tabla_id7_tidy <- tabla_id7_tidy %>%
        select(id_serie,anio,sexo,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id7_tidy$valor <- as.numeric(tabla_id7_tidy$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id7_tidy <- tabla_id7_tidy %>% mutate(valor = round(valor, 0))
      
      
      
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      
      
      #descarto los datos que ya tengo
      
      tabla_id7_bis <- tabla_id7_tidy %>% dplyr::filter(anio > max(unique(base_7_nuestra$anio)))
      
      tabla_id7_act <- rbind(base_7_nuestra,tabla_id7_bis)
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id7_tidy$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id7_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      write.csv(base_7_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id7_tidy)), as.data.frame(as.matrix(base_7_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      fecha_ult_act <- max(unique(tabla_id7_tidy$anio))
      #le pego la ultima fecha de actualización a la base de series
      
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id7_tidy$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id7_tidy$id_serie),
                                      as.character(max(tabla_id7_tidy$anio)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      
      
      print("base 7 ejecutada_1")
      
    }
    print("base 7 ejecutada_2")
    
  }
  
  
  if(8 %in% id_actualizar) {
    # ID 8 --------------------------------------------------------------------
    
    
    tabla_id8 <- readxl::read_excel(
      path=nombre_serie_8,
      sheet="C1.9",
      col_names=TRUE
    )
    
    base_8_nuestra<-read.csv("series/8_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    
    max_fecha_act <- tabla_id8[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_8_nuestra$fecha)) {
      
      # elimino las 4  primeras filas que no sirven, luego elimino las columnas de brecha y las que no tienen datos
      
      
      tabla_id8 <-tabla_id8[c(-1,-2,-3,-4),c(-4,-7,-10,-13,-14,-15) ]
      
      
      # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
      
      tabla_id8 <- tabla_id8%>% drop_na ("...3")
      
      
      
      
      # Cambio los nombres de las columnas
      
      colnames(tabla_id8) <- c("trim","Ingreso ocupados - Mujeres","Ingreso ocupados - Varones","Ingreso ocupados plenos - Mujeres","Ingreso ocupados plenos - Varones","Ingreso asalariados - Mujeres","Ingreso asalariados - Varones","Ingreso asalariados plenos - Mujeres","Ingreso asalariados plenos - Varones")
      
      
      # creo una columna llamada trimestre y otra para el año para que me aparezcan los valores correspondientes segun la colmna "trim" que posteriormente elimino
      
      tabla_id8 <- tabla_id8 %>%
        mutate(trimestre = as.integer(str_match(trim, "^(\\d+)° Trim")[, 2]),
               anio =  as.integer(str_extract(trim, "(?<=Trim )\\d+")))
      
      tabla_id8 <- tabla_id8 %>%
        select(-trim)
      
      # le sumo 2000 al año para que este en formato completo
      
      tabla_id8 <- tabla_id8 %>%
        mutate(anio = anio + 2000)
      
      # cambio el formato de la columna trimestre
      
      tabla_id8 <- tabla_id8 %>%
        mutate(trimestre = case_when(
          trimestre == 1 ~ "1º Trim",
          trimestre == 2 ~ "2º Trim",
          trimestre == 3 ~ "3º Trim",
          trimestre == 4 ~ "4º Trim",
          TRUE ~ as.character(trimestre)  # Mantener cualquier otro valor que no coincida con los anteriores
        ))
      
      
      # transformor la base en tidy
      
      
      
      
      tabla_id8_tidy <- tabla_id8 %>%
        pivot_longer(
          cols=-c(trimestre,anio),
          names_to="concepto",
          values_to="valor"
        )
      
      
      tabla_id8_tidy <- tabla_id8_tidy %>%
        separate(col = concepto, into = c("indicador", "sexo"), sep = " - ", remove = TRUE)
      
      
      # agrego una columna para el numero indice de la base
      
      tabla_id8_tidy <- tabla_id8_tidy %>%
        mutate(id_serie=8
        )
      
      # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
      
      tabla_id8_tidy <- tabla_id8_tidy[order(tabla_id8_tidy[,3],tabla_id8_tidy[,4],tabla_id8_tidy[,2],tabla_id8_tidy[,1]),]
      
      
      # reordeno las columnas
      
      tabla_id8_tidy <- tabla_id8_tidy %>%
        select(id_serie,trimestre,anio,indicador,sexo,valor)
      
      # convierno los valores de la columna valor en numericos
      
      tabla_id8_tidy$valor <- as.numeric(tabla_id8_tidy$valor)
      
      # redondeo los valores a 1 decimal
      
      tabla_id8_tidy <- tabla_id8_tidy %>% mutate(valor = round(valor, 0))
      
      
      
      
      
      
      
      
      
      # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
      
      
      
      
      #descarto los datos que ya tengo
      
      trimestre_a_numero <- function(trimestre) {
        case_when(
          grepl("1º Trim", trimestre) ~ 1,
          grepl("2º Trim", trimestre) ~ 2,
          grepl("3º Trim", trimestre) ~ 3,
          grepl("4º Trim", trimestre) ~ 4,
          TRUE ~ NA_integer_  # En caso de que no coincida con ningún patrón
        )
      }
      
      # Asume que la base "tabla_id5_tidy" tiene las columnas "trimestre" y "año"
      # y la base "base_5_nuestra" tiene las columnas "trimestre" y "año"
      
      # Combina las columnas de trimestre y año en una nueva columna de fecha en "tabla_id5_tidy"
      tabla_id8_tidy <- tabla_id8_tidy %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      # Combina las columnas de trimestre y año en una nueva columna de fecha en "base_5_nuestra"
      base_8_nuestra <- base_8_nuestra %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      # Ahora puedes aplicar el código original para filtrar las bases por fecha
      tabla_id8_bis <- tabla_id8_tidy %>%
        dplyr::filter(fecha > max(unique(base_8_nuestra$fecha)))
      
      
      tabla_id8_act <- rbind(base_8_nuestra,tabla_id8_bis)
      
      # elimino la columna fecha de la base tabla_id5_tidy y la base base_5_nuestra
      
      tabla_id8_tidy <- tabla_id8_tidy %>%
        select(-fecha)
      base_8_nuestra <- base_8_nuestra %>%
        select(-fecha)
      
      
      #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
      
      
      
      #para guardar el archivo con el nombre
      
      datos_series <- read_excel("base_series.xlsx")
      
      
      id <- unique(tabla_id8_tidy$id_serie)
      bb <- datos_series %>% filter(id_serie == id)
      tt <- bb[1,8]
      
      tt <- unlist(strsplit(as.character(tt), ","))
      
      tt <- as.vector(tt)
      write.csv(tabla_id8_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      write.csv(base_8_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
      
      
      
      #para actualizar la fecha del ultimo dato de la base_series
      
      diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id8_tidy)), as.data.frame(as.matrix(base_8_nuestra)))
      if (nrow(diff_cells) == 0) {
        print("Los dataframes son iguales en cada celda.")
      } else {
        print("Los dataframes tienen diferencias en las siguientes celdas:")
        print(diff_cells)
      }
      
      
      tabla_id8_tidy <- tabla_id8_tidy %>%
        mutate(fecha = as.Date(paste0(anio, "-",
                                      trimestre_a_numero(trimestre) * 3 - 2, "-01")))
      
      
      fecha_ult_act <- max(unique(tabla_id8_tidy$fecha))
      #le pego la ultima fecha de actualización a la base de series
      
      datos_series <- datos_series %>%
        mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id8_tidy$id_serie), 
                                      as.character(Sys.Date()), 
                                      as.character(fecha_ult_act))) %>%
        mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id8_tidy$id_serie),
                                      as.character(max(tabla_id8_tidy$fecha)),
                                      as.character(fecha_ult_dato)))
      
      write.xlsx(datos_series, file = "base_series.xlsx")
      print("base 8 ejecutada_1")
      
    } 
    print("base 8 ejecutada_2")
  }
  
  
  if(9 %in% id_actualizar) {
    # ID 9 --------------------------------------------------------------------
    
    
    
    tabla_id9 <- readxl::read_excel(
      path=nombre_serie_9,
      sheet="C 3.1",
      col_names=TRUE
    )
    
    base_9_nuestra<-read.csv("series/9_Ministerio de Trabajo, Empleo y Seguridad Social de la Nación_OEDE - Boletín de estadísticas laborales según sexo_Mercado de trabajo_NA.csv")
    
    
    max_fecha_act <- tabla_id9[,1]
    
    max_fecha_act <- max_fecha_act[!is.na(as.numeric(max_fecha_act[[1]])), ]
    # elimino las primeras filas que no sirven, y me quedo solo con la primer y tercer columna que tiene el periodo y el valor, la cuarta columna la dejo temporalmente para el siguiente paso y luego la elimino
    max_fecha_act[[1]] <- as.Date(as.numeric(max_fecha_act[[1]]), origin = "1899-12-30")
    
    if(max(max_fecha_act[[1]])>max(base_9_nuestra$fecha)) {
    
    
    
    # elimino las 4  primeras filas que no sirven, luego elimino las ultimas tres columnas que tampoco sirven
    
    
    tabla_id9 <-tabla_id9[c(-1,-2,-3,-4),c(-4,-5,-6) ]
    
    
    # elimino las ultimas filas que no tienen datos, pidiendo que elimine las filas que no tienen datos en la columna 3
    
    tabla_id9 <- tabla_id9%>% drop_na (3)
    
    
    # Cambio los nombres de las columnas
    
    colnames(tabla_id9) <- c("anio","Mujeres","Varones")
    
    
    
    # transformor la base en tidy
    
    
    
    
    tabla_id9_tidy <- tabla_id9 %>%
      pivot_longer(
        cols=-c(anio),
        names_to="sexo",
        values_to="valor"
      )
    
    
    
    # agrego una columna para el numero indice de la base
    
    tabla_id9_tidy <- tabla_id9_tidy %>%
      mutate(id_serie=9
      )
    
    # ordeno la base de datos para que me aparezca ordenado primero por sector y luego por fecha
    
    tabla_id9_tidy <- tabla_id9_tidy[order(tabla_id9_tidy[,2],tabla_id9_tidy[,1]),]
    
    
    # reordeno las columnas
    
    tabla_id9_tidy <- tabla_id9_tidy %>%
      select(id_serie,anio,sexo,valor)
    
    # convierno los valores de la columna valor en numericos
    
    tabla_id9_tidy$valor <- as.numeric(tabla_id9_tidy$valor)
    
    # redondeo los valores a 1 decimal
    
    tabla_id9_tidy <- tabla_id9_tidy %>% mutate(valor = round(valor, 1))
    
    
    
    
    
    
    
    
    
    # ME FALTA TODA ESTA PARTE DE GUARDAR LOS DATOS
    
    
    
    
    #descarto los datos que ya tengo
    
    tabla_id9_bis <- tabla_id9_tidy %>% dplyr::filter(anio > max(unique(base_9_nuestra$anio)))
    
    tabla_id9_act <- rbind(base_9_nuestra,tabla_id9_bis)
    
    
    #tabla_id1_act es la tabla que ya teniamos con los valores nuevos y tabla_id1 es la tabla que descargamos
    
    
    
    #para guardar el archivo con el nombre
    
    datos_series <- read_excel("base_series.xlsx")
    
    
    id <- unique(tabla_id9_tidy$id_serie)
    bb <- datos_series %>% filter(id_serie == id)
    tt <- bb[1,8]
    
    tt <- unlist(strsplit(as.character(tt), ","))
    
    tt <- as.vector(tt)
    write.csv(tabla_id9_tidy, paste0("series/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    write.csv(base_9_nuestra, paste0("backups/",bb[1,1],"_",bb[1,2],"_",bb[1,3],"_",tt[1],"_",tt[2],"_",gsub(":", "-", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),".csv"), row.names = FALSE, fileEncoding="UTF-8")
    
    
    
    #para actualizar la fecha del ultimo dato de la base_series
    
    diff_cells <- setdiff(as.data.frame(as.matrix(tabla_id9_tidy)), as.data.frame(as.matrix(base_9_nuestra)))
    if (nrow(diff_cells) == 0) {
      print("Los dataframes son iguales en cada celda.")
    } else {
      print("Los dataframes tienen diferencias en las siguientes celdas:")
      print(diff_cells)
    }
    
    
    fecha_ult_act <- max(unique(tabla_id9_tidy$anio))
    #le pego la ultima fecha de actualización a la base de series
    
    
    datos_series <- datos_series %>%
      mutate(fecha_ult_act = ifelse(id_serie %in% unique(tabla_id9_tidy$id_serie), 
                                    as.character(Sys.Date()), 
                                    as.character(fecha_ult_act))) %>%
      mutate(fecha_ult_dato= ifelse(id_serie %in% unique(tabla_id9_tidy$id_serie),
                                    as.character(max(tabla_id9_tidy$anio)),
                                    as.character(fecha_ult_dato)))
    
    write.xlsx(datos_series, file = "base_series.xlsx")
    print("base 9 ejecutada_1")
    }
    print("base 9 ejecutada_2")
  
  
}

}

}




