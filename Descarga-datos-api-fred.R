#------------------------------------------------------------------------------#
#                       Extracción Data FRED via API
#------------------------------------------------------------------------------#

# Descarga  ----
# Elaborado: Arturo González


# Carga de paquetes ----
# ---------------------------------------------------------------------------- #

pacman::p_load(dplyr, glue, janitor, readxl, tictoc, lubridate
               , httr, jsonlite, glue, fredr, ggplot2)


# Definición rutas ----
# ---------------------------------------------------------------------------- #
Datos_salida  <- "Data_salida/"
Datos_entrada <- "Datos_entrada/"


# 1. Crear una cuenta en FRED ---------------------------------------------
## 1.1 Con la cuenta creada solicitar un API key
## Ver ejemplos: https://fred.stlouisfed.org/docs/api/fred/

api_key <- "agregar tu API KEY (mandatory)"


## met 1 Consulta metadata ---------------------------------------------

## Define función para consulta GET

## función para topicos ----
search_fred_topic <- function(search_text, max_results = 30000) {
  all_series <- data.frame()  # Creación objeto vacio
  limit <- 1000   # número maximo que retorna cada consulta el API-FRED
  offset <- 0     # parametro para paginación
  
  while (offset < max_results) {
    base_url <- "https://api.stlouisfed.org/fred/series/search"
    response <- GET(url = base_url, query = list(search_text = search_text, 
                                                 api_key = api_key, 
                                                 file_type = "json", 
                                                 limit = limit,
                                                 offset = offset))
    
    if (status_code(response) == 200) {
      data <- content(response, as = "text")
      json_data <- fromJSON(data)
      

      # Validar si en la lista de retorno hay datos en el campo "seriess"
      if ("seriess" %in% names(json_data)) {
        current_series <- json_data$seriess  
        
        # sección para consultas con paginación
        if (is.data.frame(current_series) && nrow(current_series) > 0) {
          all_series <- rbind(all_series, current_series)  # pegar por filas los datos
          offset <- offset + limit                         # puntero para paginación
          print(limit)
        } else {
          break  # si no hay datos en la consulta -> sale del loop
        }
      } else {
        warning("No hay campo 'seriess' ")
        break
      }
    } else {
      warning("Falla en la extracción de datos: ", response$status_code)
      break  # salida del ciclo por un error
    }
  }
  
  return(all_series)
}

# Implementación ----
# Defina el tópico que se quiere consultar si hay series en el FRED
busqueda_series <- search_fred_topic("Colombia")
openxlsx::write.xlsx(busqueda_series,glue("{Datos_salida}Series_Colombia.xlsx"))


## Función para retornar datos con id -----------------------------------
# Función para obtener datos de FRED
obtener_datos_fred <- function(series_id, api_key) {
  url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", series_id, 
                "&api_key=", api_key, "&file_type=json")
  
  response <- GET(url)
  
  # Manejo de errores
  if (status_code(response) != 200) {
    stop(paste0("Error al obtener datos de FRED. Código de estado: ", status_code(response), 
                ". Mensaje: ", content(response, "text")))
  }
  
  data_json <- fromJSON(content(response, "text"))
  
  # Verificación de la estructura de los datos
  if (! "observations" %in% names(data_json)) {
    stop("No datos para el campo 'observations' en el Json")
  }
  
  if (length(data_json$observations) == 0) {
    warning("No hay series de tiempo :( ")
    return(NULL)
  }
  
  # Conversión a data.frame
  datos <- as.data.frame(data_json$observations)
  
  # Conversión de la columna 'date' a formato de fecha si es necesario
  if ("date" %in% names(datos)){
    datos$date <- as.Date(datos$date)
  }
  
  return(datos)
}

# Implementación 2 ------------------------------------------------------------
# Llamar a la función con el ID de la serie y tu clave de API
series_id <- "DCOILBRENTEU"
resultados <- obtener_datos_fred(series_id, api_key)
tail(resultados)
str(resultados)

depura <- resultados |> tidyr::separate(date,into = c("anho","mes","dia"),sep = "-") |> 
          select(-c(realtime_start,realtime_end)) |> cbind(resultados$date) |> 
          rename(Fecha="resultados$date") |> relocate(Fecha) |> 
          mutate(across(starts_with("value"),as.numeric)) |> na.omit() |> 
          mutate(var_yoy = (value/lag(value,n = 12) - 1))
tail(depura)



# 3 función para multiples Ids ------------------------------------------------
obtener_datos_fred_multiples <- function(series_ids, api_key) {
  resultados <- list()
  
  for (series_id in series_ids) {
    url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", series_id, 
      "&api_key=", api_key, "&file_type=json")
    
    response <- GET(url)
    
    if (status_code(response) != 200) {
      warning(paste0("Error al obtener datos de FRED para ", series_id, 
        ". Código de estado: ", status_code(response), 
        ". Mensaje: ", content(response, "text")))
      next
    }
    
    data_json <- fromJSON(content(response, "text"))
    
    if (! "observations" %in% names(data_json)) {
      warning("No  hay series de tiempo para la serie id :( ", series_id)
      next
    }
    
    if (length(data_json$observations) == 0) {
      warning("  ", series_id)
    } else {
      datos <- as.data.frame(data_json$observations)
      
      if ("date" %in% names(datos)){ 
        datos$date <- as.Date(datos$date)
      }
      
      resultados[[series_id]] <- datos
    }
  }
  
  return(resultados)
}

# Implementación ---------------------------------------------------------------
series_ids <- c("DCOILBRENTEU", "DGS10", "COLCCUSMA02STM") 
resultado_obtenido <- obtener_datos_fred_multiples(series_ids, api_key)
names(resultado_obtenido)


# 4 Graficar ------------------------------------------------------------------
graficar_datos_fred <- function(lista_datos) {
  for (series_id in names(lista_datos)) {
    datos <- lista_datos[[series_id]]
    
    datos$date <- as.Date(datos$date)
    
    # Datos para la leyenda en el gráfico
    ultimo_valor <- tail(datos, 1)
    fecha_ultimo_valor <- ultimo_valor$date
    valor_ultimo <- round(as.numeric(ultimo_valor$value), 2)
    etiqueta_ultimo <- paste("Último valor:", valor_ultimo, "\nFecha:", fecha_ultimo_valor)
    
    # Objeto gráfico
    grafico <- ggplot(datos, aes(x = date, y = as.numeric(value))) +
      geom_line(color = "blue") +
      labs(title = paste("Datos API FRED para el ID:", series_id),
        x = "",
        y = "Valor") +
      theme_minimal() +
      geom_text(aes(label = etiqueta_ultimo), 
        data = ultimo_valor, 
        vjust = -1, 
        color = "black", 
        size = 3,
        parse = FALSE)
    
    # Mostrar gráfico
    windows()
    print(grafico)
  }
}



# Uso de la función
graficar_datos_fred(resultado_obtenido)




# Metodo mediante libreria
# Metodo 2 libreria fredr -------------------------------------------------------
## Extracción con libreria FRED para R fredr
## Obtener el API key y autenticarse

fredr_set_key(api_key)

## Serie de Colombia contenidas en el FRED -------------------------------
test <- fredr_series_search_text("Colombia")
View(test)

metadata <- test |> filter(id=="WUICOL")

data_1 <- fredr_series_observations(series_id = "WUICOL")
names(data_1)



data_1_xts <- data_1 |> select(value) |> xts(order.by = as.Date(data_1$date))
TSstudio::ts_plot(data_1_xts)

data_2 <- fredr_series_observations(series_id = c("WUIMACOL"))
names(data_2)

data_2_xts <- data_2 |> select(value) |> xts(order.by = as.Date(data_2$date))
TSstudio::ts_plot(data_2_xts)




data_3_xts <- data_2 |> select(value) |> mutate(PM3 = rollmean(value,k = 3,align = "right", fill = NA),
                                                median3M = rollapply(value,width=3, FUN =median, fill=NA
                                                                     , align="right")) |> 
                                                  xts(order.by = as.Date(data_2$date))
TSstudio::ts_plot(data_3_xts)


# -----------------------------------------------------------------------------/
# FIN DE PROGRAMA ----
# -----------------------------------------------------------------------------/
