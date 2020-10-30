#' @title Scrapea los Borme de la provincia especificada y devuelve un json con con el resultado.
#'
#' @description Scrapea el Borme en PDF de la provincia especificada, en base a un minicipio de referencia, un radio de distancia en km y de una fecha específica,
#'  devuelve un json con con los campos reflejados por empresa en el borme y lo envía a la plataforma Smart City.
#'
#' @param url, municipio, radio, provincia, fecha_borme
#'
#' @return json
#'
#' @examples  lectura_borme_municipio('https://www.boe.es/borme/dias/2019/01/17/pdfs/BORME-A-2019-11-48.pdf', 'Ermua', 30, 'Bizkaia', "2019-12-19")
#'
#' @import jsonlite
#' pdftools
#' tidyverse
#' stringr
#' tidyr
#' dplyr
#' RSelenium
#' httr
#' tm
#' geosphere
#' anytime
#' RPostgres
#' RPostgreSQL
#' DBI
#'
#' @export

lectura_borme_municipio <- function(url, municipio, radio, provincia, fecha_borme){

  #Función que convierte a mayúsculas la primera letra de las palabras de un vector de caracteres
  letras_mayus <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),sep="", collapse=" ")
  }

  url <- as.character(url)
  municipio <- municipio
  radio_ref <- radio
  provincia <- provincia

  #Envío JSON a plataforma
  TB_token <- "eFbps1EKXC6fpqksxNLX"
  TB_url   <- paste("http://78.47.39.122:8080/api/v1/",TB_token,"/telemetry",sep="")

  pos_puntos <- gregexpr(pattern = "[[:punct:]]+",text = url)
  pos_barras <- gregexpr(pattern = "[/]+",text = url)
  #nombre_borme <- substr(url,pos_barras[[1]][length(pos_barras[[1]])]+1,pos_puntos[[1]][length(pos_puntos[[1]])]-1)

  archivo_temporal <- tempfile(pattern = "", tmpdir = tempdir(), fileext = ".pdf")
  # mode = wb es en binary
  download.file(url, destfile = archivo_temporal, mode = "wb")

  txt <- pdf_text(archivo_temporal)
  info <- pdf_info(archivo_temporal)
  #data<-pdf_data(archivo_temporal)
  pages<-info$pages
  documents <- strsplit(txt,"*.[0-9] - ", fixed=F)  #Split del txt PDF por "-". El número previo a "-" hace referencia al número de empresas del año presente.

  ###############################################

  docs<-{}
  index<-c(1)
  for ( i in 1:pages){
    s<-length(documents[[i]])
    l<-length(index)
    index<-append(index,s+index[l])
    for (j in 1:s){
      docs<-append(docs,documents[[i]][[j]])
    }}
  index<-index[-length(index)]

  #docs <- str_squish(docs)

  docs[index]<-docs[index]%>%gsub("BOLETÍN OFICIAL DEL REGISTRO MERCANTIL.*Pág\\. \\d\\d\\d\\d\r\n","",.)

  for (i in 2:length(index)){
    docs[index[i]-1]<-paste(docs[index[i]-1],docs[index[i]],collpase="")
  }
  docs<-docs[-index]

  #Bucle para evitar errores en la separación del texto (pdf) por empresa
  for(i in 1:length(docs)){
    valor_bool <- grepl("registrales", docs[i])

    if(!valor_bool){
      docs[i] <- paste(docs[i], docs[i+1], sep = "")
      docs<-docs[-(i+1)]
    }

    if(grepl("NANA", docs[i]) | grepl("ISSN:", docs[i])){
      docs<-docs[-i]
    }
  }
  docs <- na.omit(docs)

  ##Nombre de las empresas
  #`EMPRESA` <-sub('\\.\r\n.*', '', docs)
  `EMPRESA` <-sub('\\.\n.*', '', docs)


  ##Numero de registros realizados
  total_docs<-length(EMPRESA)
  docs<-docs%>%tolower()
  #docs<-docs%>%gsub("cve: (.*)","",.)

  #docs<-docs%>%gsub("cve: borme.* ","",.)%>%gsub("verificable en https://www.boe.es\n","",.)



  docs<-docs%>%gsub("\"","",.)
  docs<-str_replace_all(docs,"nombramientos","NOMBRAMIENTOS")
  docs<-str_replace_all(docs,"datos registrales","DATOS REGISTRALES")
  docs<-str_replace_all(docs,"ceses/dimisiones","CESES/DIMISIONES")
  docs<-str_replace_all(docs,"otros conceptos","OTROS CONCEPTOS")
  docs<-str_replace_all(docs,"disolución","DISOLUCIÓN")
  docs<-str_replace_all(docs,"extinción","EXTINCIÓN")
  docs<-str_replace_all(docs,"declaración de unipersonalidad","DECLARACIÓN DE UNIPERSONALIDAD")
  docs<-str_replace_all(docs,"ampliación de capital","AMPLIACIÓN DE CAPITAL")
  docs<-str_replace_all(docs,"reelecciones","REELECCIONES")
  docs<-str_replace_all(docs,"cambio de domicilio social","CAMBIO DE DOMICILIO SOCIAL")
  docs<-str_replace_all(docs,"cambio de objeto social","CAMBIO DE OBJETO SOCIAL")
  docs<-str_replace_all(docs,"modificaciones estatutarias","MODIFICACIONES ESTATUTARIAS")
  docs<-str_replace_all(docs,"revocaciones","REVOCACIONES")
  docs<-str_replace_all(docs,"constitución","CONSTITUCIÓN")
  docs<-str_replace_all(docs,"ampliacion del objeto social","AMPLIACIÓN DEL OBJETO SOCIAL")
  docs<-str_replace_all(docs,"reapertura hoja registral","REAPERTURA HOJA REGISTRAL")
  docs<-str_replace_all(docs,"reducción de capital","REDUCCIÓN DE CAPITAL")
  docs<-str_replace_all(docs,"fusión por absorción","FUSIÓN POR ABSORCIÓN")
  docs<-str_replace_all(docs,"cambio de denominación social","CAMBIO DE DENOMINACIÓN SOCIAL")
  docs<-str_replace_all(docs,"situación concursal","SITUACIÓN CONCURSAL")
  docs<-str_replace_all(docs,"escisión parcial","ESCISIÓN PARCIAL")
  docs<-str_replace_all(docs,"transformación de sociedad","TRANSFORMACIÓN DE SOCIEDAD")
  docs<-docs%>%str_squish()


  # TRANSFORMACIÓN DE SOCIEDAD
  transformacion <- str_extract(docs,"TRANSFORMACIÓN DE SOCIEDAD.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  Denom_y_forma <- transformacion %>% gsub(".*denominación y forma adoptada: ","",.)

  TRANSFORMACIÓN <- data.frame(Denom_y_forma,
                               stringsAsFactors = F)

  # ESCISIÓN PARCIAL
  escision <- str_extract(docs,"ESCISIÓN PARCIAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  Escision_parcial <- escision %>% gsub(".*ESCISIÓN PARCIAL. ","",.)

  ESCISIÓN <- data.frame(Escision_parcial,
                         stringsAsFactors = F)


  ##SITUACIÓN CONCURSAL
  situacion_concursal<-str_extract(docs,"SITUACIÓN CONCURSAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  Sit_conc_procedimiento <- situacion_concursal %>% str_extract("procedimiento concursal.*?\\.")%>%gsub("procedimiento concursal","",.)
  Sit_conc_firme <- situacion_concursal %>% str_extract("firme.*?\\,")%>%gsub("firme:","",.)%>%gsub(",","",.)
  Sit_conc_fecha_resolucion <- situacion_concursal %>% str_extract("fecha de resolución.*?\\.")%>%gsub("fecha de resolución","",.)
  Sit_conc_proceso <- situacion_concursal %>% str_extract("sit_conc_firme.*?\\.")%>%gsub("sit_conc_firme","",.)
  Sit_conc_juzgado <- situacion_concursal %>% str_extract("juzgado: num..*?\\.")%>%gsub("juzgado:","",.)
  Sit_conc_juez <- situacion_concursal %>% str_extract("juez.*?\\.")%>%gsub("juez:","",.)
  Sit_conc_resoluciones <- situacion_concursal %>% str_extract("resoluciones.*?\\.")%>%gsub("resoluciones:","",.)

  `SITUACIÓN CONCURSAL` <- data.frame(Sit_conc_procedimiento, Sit_conc_firme, Sit_conc_fecha_resolucion, Sit_conc_proceso, Sit_conc_juzgado, Sit_conc_juez, Sit_conc_resoluciones,
                                      stringsAsFactors = F)


  ##NOMBRAMIENTOS
  nombramientos<-str_extract(docs,"NOMBRAMIENTOS.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  Nombr_liquiSoli<-nombramientos%>%str_extract("liquisoli.*?\\.")%>%gsub("liquisoli:","",.)
  Nombr_apoderado<-nombramientos%>%str_extract("apoderado.*?\\.")%>%gsub("apoderado:","",.)
  Nombr_adminUnico<-nombramientos%>%str_extract("adm\\. unico.*?\\.")%>%gsub("adm\\. unico:","",.)
  Nombr_liquidador<-nombramientos%>%str_extract("liquidador:.*?\\.")%>%gsub("liquidador:","",.)
  Nombr_liquidador_mancom<-nombramientos%>%str_extract("liquidador m:.*?\\.")%>%gsub("liquidador m:","",.)
  Nombr_adminSolid<-nombramientos%>%str_extract("adm\\. solid\\..*?\\.")%>%gsub("adm\\. solid\\.:","",.)
  Nombr_socprof<-nombramientos%>%str_extract("soc\\.prof\\..*?\\.")%>%gsub("soc\\.prof\\.:","",.)
  Nombr_auditor<-nombramientos%>%str_extract("auditor.*?\\.")%>%gsub("auditor:","",.)
  Nombr_adminMan<-nombramientos%>%str_extract("adm\\. mancom\\..*?\\.")%>%gsub("adm\\. mancom\\.:","",.)
  Nombr_entidDeposit<-nombramientos%>%str_extract("entiddeposit.*?\\.")%>%gsub("entiddeposit:","",.)
  Nombr_entdPromo<-nombramientos%>%str_extract("entd\\.promo\\..*")%>%gsub("entd\\.promo\\.:","",.)
  Nombr_consejero<-nombramientos%>%str_extract("consejero.*?\\.")%>%gsub("consejero:","",.)
  Nombr_vicepresidente<-nombramientos%>%str_extract("vicepresid\\..*?\\.")%>%gsub("vicepresid\\.:","",.)
  Nombr_presidente<-nombramientos%>%str_extract("presidente.*?\\.")%>%gsub("presidente:","",.)
  Nombr_secretario<-nombramientos%>%str_extract("secretario.*?\\.")%>%gsub("secretario:","",.)

  NOMBRAMIENTOS<-data.frame(Nombr_liquiSoli,Nombr_apoderado,Nombr_adminUnico,Nombr_liquidador,Nombr_liquidador_mancom,Nombr_adminSolid,
                              Nombr_socprof,Nombr_auditor,Nombr_adminMan,Nombr_entidDeposit,
                              Nombr_entdPromo,Nombr_consejero,Nombr_vicepresidente,Nombr_presidente,
                              Nombr_secretario,stringsAsFactors=FALSE)


  #Llamada a función letras_mayus para conversión a maysuculas primera letra de los nombres y apellidos
  for(i in 1:nrow(NOMBRAMIENTOS)){
    for(j in 1:ncol(NOMBRAMIENTOS)){
      if(is.na(NOMBRAMIENTOS[i,j])){
        next
      }else{
        NOMBRAMIENTOS[i,j] <- NOMBRAMIENTOS[i,j] %>% gsub(";",", ",.) %>% letras_mayus()
      }
    }
  }



  ##CESES
  ceses<-str_extract(docs,"CESES/DIMISIONES.*?[A-Z]")
  ceses<-ceses%>%gsub("[A-Z]$","",.)

  Ceses_liquiSoli<-ceses%>%str_extract("liquisoli.*?\\.")%>%gsub("liquisoli:","",.)
  Ceses_apoderado<-ceses%>%str_extract("apoderado.*")%>%gsub("apoderado:","",.)
  Ceses_adminUnico<-ceses%>%str_extract("adm\\. unico.*?\\.")%>%gsub("adm\\. unico:","",.)
  Ceses_liquidador<-ceses%>%str_extract("liquidador:.*?\\.")%>%gsub("liquidador:","",.)
  Ceses_liquidador_mancom<-ceses%>%str_extract("liquidador m:.*?\\.")%>%gsub("liquidador m:","",.)
  Ceses_adminSolid<-ceses%>%str_extract("adm\\. solid\\..*?\\.")%>%gsub("adm\\. solid\\.:","",.)
  Ceses_adminMan<-ceses%>%str_extract("adm\\. mancom\\..*?\\.")%>%gsub("adm\\. mancom\\.:","",.)
  Ceses_socprof<-ceses%>%str_extract("soc\\.prof\\..*?\\.")%>%gsub("soc\\.prof\\..*:","",.)
  Ceses_depositorio<-ceses%>%str_extract("depositario.*?\\.")%>%gsub("depositario:","",.)
  Ceses_entidDeposit<-ceses%>%str_extract("entiddeposit.*?\\.")%>%gsub("entiddeposit:","",.)
  Ceses_entdPromo<-ceses%>%str_extract("entd\\.promo\\..*")%>%gsub("entd\\.promo\\.:","",.)
  Ceses_consejero<-ceses%>%str_extract("consejero.*?\\.")%>%gsub("consejero:","",.)
  Ceses_vicepresidente<-ceses%>%str_extract("vicepresid\\..*?\\.")%>%gsub("vicepresid\\.:","",.)
  Ceses_presidente<-ceses%>%str_extract("presidente.*?\\.")%>%gsub("presidente:","",.)
  Ceses_secretario<-ceses%>%str_extract("secretario.*?\\.")%>%gsub("secretario:","",.)

  CESES<-data.frame(Ceses_liquiSoli,Ceses_apoderado,Ceses_adminUnico,Ceses_liquidador,Ceses_liquidador_mancom,
                      Ceses_adminSolid,Ceses_adminMan,Ceses_socprof,Ceses_depositorio,
                      Ceses_entidDeposit,Ceses_entdPromo,Ceses_consejero,
                      Ceses_vicepresidente,Ceses_presidente,Ceses_secretario,stringsAsFactors = FALSE)

  #Llamada a función letras_mayus para conversión a maysuculas primera letra de los nombres y apellidos
  for(i in 1:nrow(CESES)){
    for(j in 1:ncol(CESES)){
      if(is.na(CESES[i,j])){
        next
      }else{
        CESES[i,j] <- CESES[i,j] %>% gsub(";",", ",.) %>% letras_mayus()
      }
    }
  }

  ##AMPLIACION CAPITAL
  ampliacionCapital<-str_extract(docs,"AMPLIACIÓN DE CAPITAL.*?[A-Z]")
  ampliacionCapital<-ampliacionCapital%>%gsub("[A-Z]$","",.)

  Ampl_Cap_suscrito<-ampliacionCapital%>%str_extract("suscrito.*?euros\\.")%>%gsub("suscrito:","",.)
  Ampl_Cap_resultante_suscrito<-ampliacionCapital%>%str_extract("resultante suscrito.*?euros\\.")%>%gsub("resultante suscrito:","",.)
  Ampl_Cap_desembolsado<-ampliacionCapital%>%str_extract("desembolsado.*?euros\\.")%>%gsub("desembolsado:","",.)
  Ampl_Cap_resultante_desembolsado<-ampliacionCapital%>%str_extract("resultante desembolsado.*?euros\\.")%>%gsub("resultante desembolsado:","",.)
  Ampl_Cap_capital<-ampliacionCapital%>%str_extract("capital.*?euros\\.")%>%gsub("capital:","",.)

  `AMPLIACION CAPITAL`<-data.frame(Ampl_Cap_suscrito,Ampl_Cap_resultante_suscrito,Ampl_Cap_desembolsado,
                                   Ampl_Cap_resultante_desembolsado,Ampl_Cap_capital,stringsAsFactors=FALSE)
  ##REDUCCION
  reduccionCapital<-str_extract(docs,"REDUCCIÓN DE CAPITAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  Reduc_Cap_importe_reduccion<-reduccionCapital%>%str_extract("importe reducción.*?euros\\.")%>%gsub("importe reducción:","",.)
  Reduc_Cap_resultante_suscrito<-reduccionCapital%>%str_extract("resultante suscrito.*?euros\\.")%>%gsub("resultante suscrito:","",.)

  `REDUCCION CAPITAL`<-data.frame(Reduc_Cap_importe_reduccion,Reduc_Cap_resultante_suscrito,stringsAsFactors=FALSE)

  ##CONSTITUCION
  constitucion<-str_extract(docs,"CONSTITUCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  Const_comienzo_operaciones<-constitucion%>%str_extract("comienzo de operaciones.*?\\. ")%>%gsub("comienzo de operaciones:","",.)
  Const_objeto_social<-constitucion%>%str_extract("objeto social.*?\\. domicilio")%>%gsub("objeto social:","",.)
  Const_domicilio<-constitucion%>%str_extract("domicilio.*?\\)")%>%gsub("domicilio:","",.) %>% str_trim()
  Const_capital<-constitucion%>%str_extract("capital.*?euros\\.")%>%gsub("capital:","",.)

  CONSTITUCION<-data.frame(Const_comienzo_operaciones,Const_objeto_social,Const_domicilio,
                             Const_capital,stringsAsFactors=FALSE)

  ###CAMBIO DENOMINACIÓN SOCIAL
  cambioDenominacionSocial<-str_extract(docs,"CAMBIO DE DENOMINACIÓN SOCIAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  Cambio_denominacion_social<-cambioDenominacionSocial

  ###REELECCIONES
  reelecciones<-str_extract(docs,"REELECCIONES.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  Reelecciones_adminUnico<-reelecciones%>%str_extract("adm\\. único.*?\\.")%>%gsub("adm\\. único:","",.)
  for(i in 1:length(Reelecciones_adminUnico)){
    if(!is.na(Reelecciones_adminUnico[i])){
      Reelecciones_adminUnico[i] <-  letras_mayus(gsub(";",", ",Reelecciones_adminUnico[i]))
    }
  }

  Reelecciones_auditor<-reelecciones%>%str_extract("auditor.*?\\.")%>%gsub("auditor:","",.)
  Reelecciones_auditor_suplente<-reelecciones%>%str_extract("aud\\.supl\\..*?\\.")%>%gsub("aud\\.supl\\.:","",.)

  REELECCIONES<-data.frame(Reelecciones_adminUnico,Reelecciones_auditor, Reelecciones_auditor_suplente,stringsAsFactors=FALSE)

  ##REVOCACIONES
  revocaciones<-str_extract(docs,"REVOCACIONES.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  Revocaciones_auditor<-revocaciones%>%str_extract("auditor.*?\\.")%>%gsub("auditor:","",.)
  Revocaciones_apoderado<-revocaciones%>%str_extract("apoderado.*?\\.")%>%gsub("apoderado:","",.)
  Revocaciones_apoderadoMAn<-revocaciones%>%str_extract("apo\\.man\\.soli.*?\\.")%>%gsub("apo\\.man\\.soli:","",.)
  Revocaciones_apoderadoSol<-revocaciones%>%str_extract("apo\\.sol\\..*?\\.")%>%gsub("apo\\.sol\\.:","",.)

  REVOCACIONES<-data.frame(Revocaciones_auditor,Revocaciones_apoderado,Revocaciones_apoderadoMAn,
                             Revocaciones_apoderadoSol,stringsAsFactors=FALSE)

  #Llamada a función letras_mayus para conversión a maysuculas primera letra de los nombres y apellidos
  for(i in 1:nrow(REVOCACIONES)){
    for(j in 1:ncol(REVOCACIONES)){
      if(is.na(REVOCACIONES[i,j])){
        next
      }else{
        REVOCACIONES[i,j] <- REVOCACIONES[i,j] %>% gsub(";",", ",.) %>% letras_mayus()
      }
    }
  }

  ###FUSIÓN POR ABSORCIÓN

  fusion<-str_extract(docs,"FUSIÓN POR ABSORCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  Fusion_sociedades_absorbidas<-fusion%>%str_extract("sociedades absorbidas.*?\\.")

  ##MODIFICACIONES ESTATUTARIAS
  Modificaciones_estatutarias<-str_extract(docs,"MODIFICACIONES ESTATUTARIAS.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("MODIFICACIONES ESTATUTARIAS\\.","",.)

  ##CAMBIO DOMICILIO SOCIAL
  Cambio_domicilio_social<-str_extract(docs,"CAMBIO DE DOMICILIO SOCIAL.*?\\)")%>%gsub("CAMBIO DE DOMICILIO SOCIAL.","",.) %>% str_trim()

  ##CAMBIO OBJETO SOCIAL
  Cambio_objeto_social<-str_extract(docs,"CAMBIO DE OBJETO SOCIAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("CAMBIO DE OBJETO SOCIAL\\.","",.)

  ##EXTINCION
  Extincion <-str_extract(docs,"EXTINCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("EXTINCIÓN\\.","",.)
  for(i in 1:length(Extincion)){
    if(!is.na(Extincion[i]) & nchar(Extincion[i]) < 2) {
      Extincion[i] <- 1
    }
  }

  ##DISOLUCION
  Disolucion <-str_extract(docs,"DISOLUCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("DISOLUCIÓN\\.","",.)

  ##DECLARACIÓN UNIPERSONALIDAD

  declaracionUnipersonalidad<-str_extract(docs,"DECLARACIÓN DE UNIPERSONALIDAD.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  declaracion_unipersonalidad_socio_unico<-declaracionUnipersonalidad%>%str_extract("socio único.*?\\.")%>%gsub("socio único:","",.)
  Declaracion_unipersonalidad<-data.frame(declaracion_unipersonalidad_socio_unico,stringsAsFactors=FALSE)

  ##NO SE DIVIDEN
  Otros_conceptos<-str_extract(docs,"OTROS CONCEPTOS.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("OTROS CONCEPTOS:","",.)
  Datos_registrales<-str_extract(docs,"DATOS REGISTRALES.*")%>%gsub("\\.$","",.)%>%gsub("DATOS REGISTRALES\\.","",.)

  data<-data.frame(EMPRESA,Fusion_sociedades_absorbidas,Modificaciones_estatutarias,Cambio_denominacion_social,Cambio_domicilio_social,
                   Cambio_objeto_social,CESES,NOMBRAMIENTOS,`AMPLIACION CAPITAL`,Declaracion_unipersonalidad,
                   `REDUCCION CAPITAL`,REELECCIONES,REVOCACIONES,`SITUACIÓN CONCURSAL`, ESCISIÓN, TRANSFORMACIÓN, Disolucion,Extincion,CONSTITUCION,Otros_conceptos,Datos_registrales,stringsAsFactors=FALSE)
  s<-0
  ncol<-ncol(data)
  BBDD<-data.frame(EMPRESA,stringsAsFactors=F)

  #for(j in 2:ncol){
  # l<-{}
  # a<-data[j]%>%lapply(.,function(x) str_extract_all(x,";"))
  # for(i in 1:length(a[[1]])){
  #   if(is.na(a[[1]][[i]]) || identical(a[[1]][[i]],character(0))){
  #     l2<-0
  #   }else{
  #     l2<-length(a[[1]][[i]])
  #   }
  #   l<-append(l,l2)
  # }
  # max<-max(l)
  # if(max!=0){
  #   nombres_columnas_nuevas <- c()
  #   for(k in 1:(max+1)){
  #     nombres_columnas_nuevas <- c(nombres_columnas_nuevas,paste(colnames(data[j]),k,sep=""))
  #   }
  #   dat<-separate(data[j],colnames(data[j]),nombres_columnas_nuevas,sep=";")
  #   BBDD<-cbind(BBDD,dat)
  # }else{BBDD<-cbind(BBDD,data[j])}
  # s<-s+max
  #}

  #####################################################################################
  # CÁLCULO DISTANCIAS ENTRE LONG., LAT. REFERENCIA Y DOMICILIOS CONSTITUCIÓN EMPRESAS
  #####################################################################################

  #Coordenadas de referencia del municipio con geocoder API
  #Endpoint geocoder API
  geocoder_endpoint <- "https://geocoder.ls.hereapi.com/6.2/geocode.json?apiKey=h8VwThvanUrJLPb-LHm12AA-PpcgtY31b57qx4066N0&searchtext="

  coordenadas_ref_municipio <- jsonlite::fromJSON(paste(geocoder_endpoint,URLencode(municipio),"%20(Espa%C3%B1a)",sep = ""))
  coordenadas_ref_municipio <- coordenadas_ref_municipio$Response$View$Result %>% as.data.frame()
  longitud_ref_municipio <- coordenadas_ref_municipio$Location$DisplayPosition$Longitude
  latitud_ref_municipio <- coordenadas_ref_municipio$Location$DisplayPosition$Latitude
  coor_referencia <- c(longitud_ref_municipio, latitud_ref_municipio)

  #Bucle coordenadas y municipio localización empresa
  variables_domicilio <- c("Const_domicilio", "Cambio_domicilio_social")

  #Cálculo de coordenadas (long, lat) de cada una de las empresas constituidas
  #Obtención coordenadas con geocoder API
  longitud_domicilio_m <- matrix(nrow = length(data$Const_domicilio), ncol = length(variables_domicilio))
  latitud_domicilio_m <- matrix(nrow = length(data$Const_domicilio), ncol = length(variables_domicilio))
  distancia_geometrica_coordenadas_m <- matrix(nrow = length(data$Const_domicilio), ncol = length(variables_domicilio))
  empresa_dentro_del_radio_m <- matrix(nrow = length(data$Const_domicilio), ncol = length(variables_domicilio))
  coordenadas_empresa_m <- matrix(nrow = length(data$Const_domicilio), ncol = length(variables_domicilio))
  municipio_empresa_m <- matrix(nrow = length(data$Const_domicilio), ncol = length(variables_domicilio))
  lat <- list()
  long <- list()

  for(k in 1:length(variables_domicilio)){
    for(i in 1:length(data[,grep(variables_domicilio[k],names(data))])){
      domicilio <- stripWhitespace(data[,grep(variables_domicilio[k],names(data))][i])
      domicilio <- gsub(" ","%20",domicilio)
      domicilio <- iconv(domicilio,from="UTF-8",to="ASCII//TRANSLIT")

      coordenadas_domicilios <- jsonlite::fromJSON(paste(geocoder_endpoint, domicilio,sep=""))
      coordenadas_domicilios <- coordenadas_domicilios$Response$View$Result %>% as.data.frame()

      if(is.na(domicilio) | is.null(coordenadas_domicilios$Location$DisplayPosition$Longitude[1])){
        longitud_domicilio_m[i,k] <- NA
        latitud_domicilio_m[i,k] <- NA
        municipio_empresa_m[i,k] <- NA
        distancia_geometrica_coordenadas_m[i,k] <- NA
        empresa_dentro_del_radio_m[i,k] <- NA
        next
      }

      longitud_domicilio_m[i,k] <- coordenadas_domicilios$Location$DisplayPosition$Longitude[1]
      latitud_domicilio_m[i,k] <- coordenadas_domicilios$Location$DisplayPosition$Latitude[1]
      municipio_empresa_m[i,k] <- coordenadas_domicilios$Location$Address$City[1]


      if(is.null(unlist(longitud_domicilio_m[i,k]))){
        distancia_geometrica_coordenadas_m[i,k] <- NA
        empresa_dentro_del_radio_m[i,k] <- NA
        municipio_empresa_m[i,k] <- NA
        next
      }

      distancia_geometrica_coordenadas_m[i,k] <- distm(coor_referencia, c(longitud_domicilio_m[[i,k]], latitud_domicilio_m[[i,k]]), fun = distGeo)/1000

      if(distancia_geometrica_coordenadas_m[[i,k]] <= radio_ref){
        empresa_dentro_del_radio_m[i,k] <- "SÍ"
      }else{
        empresa_dentro_del_radio_m[i,k] <- "NO"
      }
    }
  }


  #Combinación columnas matriz en lista
  longitud_domicilio <- list()
  latitud_domicilio <- list()
  distancia_geometrica_coordenadas <- list()
  empresa_dentro_del_radio <- list()
  coordenadas_empresa <- list()
  municipio_empresa <- list()

  for(k in 1:(length(variables_domicilio)-1)){
    for(i in 1:length(data[,grep(variables_domicilio[k],names(data))])){

      if(is.na(longitud_domicilio_m[i,k]) & !is.na(longitud_domicilio_m[i,k+1])){
        longitud_domicilio[i] <- longitud_domicilio_m[i,k+1]
        latitud_domicilio[i] <- latitud_domicilio_m[i,k+1]
        distancia_geometrica_coordenadas[i] <- distancia_geometrica_coordenadas_m[i,k+1]
        empresa_dentro_del_radio[i] <- empresa_dentro_del_radio_m[i,k+1]
        coordenadas_empresa[i] <- coordenadas_empresa_m[i,k+1]
        municipio_empresa[i] <- municipio_empresa_m[i,k+1]
      }else{
        longitud_domicilio[i] <- longitud_domicilio_m[i,k]
        latitud_domicilio[i] <- latitud_domicilio_m[i,k]
        distancia_geometrica_coordenadas[i] <- distancia_geometrica_coordenadas_m[i,k]
        empresa_dentro_del_radio[i] <- empresa_dentro_del_radio_m[i,k]
        coordenadas_empresa[i] <- coordenadas_empresa_m[i,k]
        municipio_empresa[i] <- municipio_empresa_m[i,k]
      }
    }
  }

  #Manejo de NAs
  coordenadas_empresa <- paste(longitud_domicilio,latitud_domicilio,sep = ", ")
  coordenadas_empresa <- str_replace_all(coordenadas_empresa,"NA, NA", "NA")
  coordenadas_empresa <- str_replace_all(coordenadas_empresa,"NULL, NULL", "NA")
  for (i in 1:length(coordenadas_empresa)){
    if(nchar(coordenadas_empresa[i]) < 5){
      coordenadas_empresa[i] <- as.numeric(coordenadas_empresa[i])
      #longitud_domicilio[[i]] <- as.numeric(longitud_domicilio[i])
      #latitud_domicilio[[i]] <- as.numeric(latitud_domicilio[i])
    }

    if(is.na(coordenadas_empresa[i])){
      long[i] <- coordenadas_empresa[i]
      lat[i] <- coordenadas_empresa[i]
    }else{
      long[i] <- as.numeric(str_split(coordenadas_empresa[i],",")[[1]][1])
      lat[i] <- as.numeric(str_split(coordenadas_empresa[i],",")[[1]][2])
    }
  }


  data$`Coordenadas empresa` <- coordenadas_empresa
  data$Latitud <- unlist(lat)
  data$Longitud <- unlist(long)
  data$`Municipio empresa` <- unlist(municipio_empresa)
  data$`Distancia respecto municipio km` <- unlist(distancia_geometrica_coordenadas)
  data$`Dentro del radio de referencia km` <- unlist(empresa_dentro_del_radio)
  data$`Provincia Borme` <- provincia
  data$fecha <- rep(format(Sys.Date(),"%d/%m/%Y"),nrow(data))

  data[is.na(data)] <- "-"

  # ==========================================================
  # VOLCADO EN BBDD
  # ==========================================================

  # 1) CONEXIÓN BBDD
  db          <- 'datawarehouse'
  host_db     <- '82.223.243.42'
  db_port     <- '5432'
  db_user     <- 'postgres'
  db_password <- 'postgressysadmin_2019'

  con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

  # 2) CREACIÓN TABLA TEMPORAL CON DATOS ACTUALES PARA EVITAR DUPLICADOS EN LA TABLA PRINCIPAL
  dbWriteTable(con, 'borme_temporal',data, temporary = TRUE)

  consulta_evitar_duplicados <- 'INSERT INTO borme SELECT * FROM borme_temporal a WHERE NOT EXISTS (SELECT 0 FROM borme b where b."EMPRESA" = a."EMPRESA" AND b.fecha = a.fecha)'


  # 2) ESCRITURA EN TABLA PRINCIPAL COMPARANDO CON LA TEMPORAL
  #inicio_consulta_evitar_duplicados <- paste('UPDATE borme SET')
  #seleccion_columnas_tablas <- paste(paste('"',colnames(data), '"',paste(' = borme_temporal."',colnames(data),'"',sep = ""),sep = ""), collapse = ", ")
  #final_consulta_evitar_duplicados <- 'FROM borme_temporal WHERE borme_temporal."EMPRESA" != borme."EMPRESA" OR borme_temporal.fecha != borme.fecha'
  #consulta_evitar_duplicados <- paste(inicio_consulta_evitar_duplicados,
  #                                    seleccion_columnas_tablas,
  #                                    final_consulta_evitar_duplicados)



  dbGetQuery(con, consulta_evitar_duplicados)  # Ejecución consulta
  dbRemoveTable(con,"borme_temporal")   # Eliminación tabla temporal

  return(data)
}
