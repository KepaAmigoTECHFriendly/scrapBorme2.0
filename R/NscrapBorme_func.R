#' @title Scrapea los Borme de las provincias especificadas y envía el resultado a la plataforma Smart City.
#'
#' @description Scrapea los Borme en PDF de las provincias especificadas (en caso de introducir más de una provincia estas deben estar separadas por comas),
#' en base a un minicipio de referencia, un radio de distancia en km y de una fecha específica (por defecto el parámetro fecha es la fecha actual),
#' devuelve un json con con los campos reflejados por empresa en el borme y lo envía a la plataforma Smart City.
#'
#' @param provincias, radio, municipio, fecha
#'
#' @return json
#'
#' @examples  N_lectura_borme('Ermua', 30, 'Bizkaia, Gipuzkoa, Araba')
#'
#' @import jsonlite
#' pdftools
#' tidyverse
#' stringr
#' rvest
#' httr
#' geosphere
#' tm
#' anytime
#' xml2
#' purrr
#'
#' @export

N_lectura_borme <- function(municipio, radio, provincias, fecha = Sys.Date()){

  if(str_detect(provincias,",")){
    provincias <- str_trim(toupper(unlist(str_split(provincias,","))))
  }else{
    provincias <- toupper(provincias)
  }

  municipio <- municipio
  radio_ref <- radio

  url_general <- "https://www.boe.es/borme/dias/"
  if(str_detect(fecha,"-")){
    fecha_borme <- str_replace_all(fecha,"-","/")
  }else{
    fecha_borme <- fecha
  }

  url_fecha <- paste(url_general,fecha_borme, sep = "")

  #Envío JSON a plataforma
  TB_token <- "eFbps1EKXC6fpqksxNLX"
  TB_url   <- paste("http://78.47.39.122:8080/api/v1/",TB_token,"/telemetry",sep="")

  #Manejo de errores
  tryCatch({

    html <- read_html(url_fecha)  #Objeto documento html

    titulo <- html %>% html_nodes(".sumario ul")%>% html_nodes(".dispo p") %>% html_text() %>% str_trim() %>% unlist()
    titulo <- titulo[1:grep("ÍNDICE ALFABÉTICO DE SOCIEDADES", titulo)-1]

    url_pdf <- html %>% html_nodes(".puntoPDF") %>% html_nodes("a") %>% html_attr("href") %>% str_trim() %>% unlist()
    url_pdf <- url_pdf[1:length(titulo)+1]

    url_borme_gen <- "https://www.boe.es"

    #Extracción índice provincias en vector título
    posicion_urls <- c()
    for(i in 1:length(provincias)){
      if(any(grepl(provincias[i],titulo))){
        posicion_urls <- c(posicion_urls,grep(provincias[i],titulo))
      }
    }

    #Generación de error en caso de que no se encuentren Bormes de las provincias especificadas
    if(is_empty(posicion_urls)){
      stop("No se encuentran las provincias especificadas en el Borme de hoy")
    }

    #Tiempo de referencia para posterior suma con el objetivo de evitar pisados en el timestamp de la plataforma Smart City
    t_ref <- "00:00:00"

    #Bucle ejecución N Bormes
    for(k in 1:length(posicion_urls)){

      url <- paste(url_borme_gen,url_pdf[posicion_urls[k]],sep = "")
      provincia <- titulo[posicion_urls[k]]

      #Lógica para variar timestamp y evitar pisados en plataforma Smart City
      fecha_borme <- as.POSIXct(paste(fecha, t_ref)) + 3600*(k-1)

      print(fecha_borme)
      print(provincia)

      retorno <- scrapBorme::lectura_borme_municipio(url, municipio, radio_ref, provincia, fecha_borme)
    }

  },error = function(e){

    err <<- conditionMessage(e)
    retorno <<- err

    #Switch de errores
    switch(err,
           "HTTP error 404."={
             error_plataforma <- '{"ERROR": "NO HAY BORME PUBLICADO PARA LA FECHA DE HOY"}'
           },
           "No se encuentran las provincias especificadas en el Borme de hoy"={
             error_plataforma <- '{"ERROR": "NO SE ENCUENTRAN LAS PROVINCIAS ESPECIFICADAS PARA LA FECHA DE HOY"}'
           },
           {
             error_plataforma <- '{"ERROR"}'
           }
    )

    json_envio_plataforma <- error_plataforma
    #Envío JSON a plataforma
    POST(url=TB_url,body=json_envio_plataforma)
  })

  return(retorno)
}
