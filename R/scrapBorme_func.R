#' @title Scrapea el Borme en PDF y devuelve un json con con el resultado.
#'
#' @description Scrapea el Borme en PDF, devuelve un json con con los campos reflejados por empresa en el borme y lo envía a la plataforma smart city.
#'
#' @param url
#'
#' @return json
#'
#' @examples  lectura_borme('https://www.boe.es/borme/dias/2019/01/17/pdfs/BORME-A-2019-11-48.pdf')
#'
#' @import jsonlite
#' pdftools
#' tidyverse
#' stringr
#' tidyr
#' dplyr
#' RSelenium
#' httr
#'
#' @export

lectura_borme <- function(url=""){

  #Función que convierte a mayúsculas la primera letra de las palabras de un vector de caracteres
  letras_mayus <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),sep="", collapse=" ")
  }

  url <- as.character(url)

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
  documents <- strsplit(txt,"*.[0-9] - ", fixed=F) #Split del txt PDF por "-". El número previo a "-" hace referencia al número de empresas del año presente.
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
  `EMPRESA` <-sub('\\.\n.*', '', docs)

  ##Numero de registros realizados
  var_total_docs<-length(EMPRESA)
  docs<-docs%>%tolower()
  #docs<-docs%>%gsub("cve: (.*)","",.)
  docs<-docs%>%gsub("cve: .*\n ","",.)%>%gsub("verificable en https://www.boe.es\n","",.)

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
  var_nombramientos<-str_extract(docs,"NOMBRAMIENTOS.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  var_nombramientos_liquiSoli<-var_nombramientos%>%str_extract("liquisoli.*?\\.")%>%gsub("liquisoli:","",.)
  var_nombramientos_apoderado<-var_nombramientos%>%str_extract("apoderado.*?\\.")%>%gsub("apoderado:","",.)
  var_nombramientos_adminUnico<-var_nombramientos%>%str_extract("adm\\. unico.*?\\.")%>%gsub("adm\\. unico:","",.)
  var_nombramientos_liquidador<-var_nombramientos%>%str_extract("liquidador:.*?\\.")%>%gsub("liquidador:","",.)
  var_nombramientos_liquidador_mancom<-var_nombramientos%>%str_extract("liquidador m:.*?\\.")%>%gsub("liquidador m:","",.)
  var_nombramientos_adminSolid<-var_nombramientos%>%str_extract("adm\\. solid\\..*?\\.")%>%gsub("adm\\. solid\\.:","",.)
  var_nombramientos_socprof<-var_nombramientos%>%str_extract("soc\\.prof\\..*?\\.")%>%gsub("soc\\.prof\\.:","",.)
  var_nombramientos_auditor<-var_nombramientos%>%str_extract("auditor.*?\\.")%>%gsub("auditor:","",.)
  var_nombramientos_adminMan<-var_nombramientos%>%str_extract("adm\\. mancom\\..*?\\.")%>%gsub("adm\\. mancom\\.:","",.)
  var_nombramientos_entidDeposit<-var_nombramientos%>%str_extract("entiddeposit.*?\\.")%>%gsub("entiddeposit:","",.)
  var_nombramientos_entdPromo<-var_nombramientos%>%str_extract("entd\\.promo\\..*")%>%gsub("entd\\.promo\\.:","",.)
  var_nombramientos_consejero<-var_nombramientos%>%str_extract("consejero.*?\\.")%>%gsub("consejero:","",.)
  var_nombramientos_vicepresidente<-var_nombramientos%>%str_extract("vicepresid\\..*?\\.")%>%gsub("vicepresid\\.:","",.)
  var_nombramientos_presidente<-var_nombramientos%>%str_extract("presidente.*?\\.")%>%gsub("presidente:","",.)
  var_nombramientos_secretario<-var_nombramientos%>%str_extract("secretario.*?\\.")%>%gsub("secretario:","",.)

  NOMBRAMIENTOS<-data.frame(var_nombramientos_liquiSoli,var_nombramientos_apoderado,var_nombramientos_adminUnico,var_nombramientos_liquidador,var_nombramientos_liquidador_mancom,var_nombramientos_adminSolid,
                            var_nombramientos_socprof,var_nombramientos_auditor,var_nombramientos_adminMan,var_nombramientos_entidDeposit,
                            var_nombramientos_entdPromo,var_nombramientos_consejero,var_nombramientos_vicepresidente,var_nombramientos_presidente,
                            var_nombramientos_secretario,stringsAsFactors=FALSE)

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
  var_ceses<-str_extract(docs,"CESES/DIMISIONES.*?[A-Z]")
  var_ceses<-var_ceses%>%gsub("[A-Z]$","",.)

  var_ceses_liquiSoli<-var_ceses%>%str_extract("liquisoli.*?\\.")%>%gsub("liquisoli:","",.)
  var_ceses_apoderado<-var_ceses%>%str_extract("apoderado.*")%>%gsub("apoderado:","",.)
  var_ceses_adminUnico<-var_ceses%>%str_extract("adm\\. unico.*?\\.")%>%gsub("adm\\. unico:","",.)
  var_ceses_liquidador<-var_ceses%>%str_extract("liquidador:.*?\\.")%>%gsub("liquidador:","",.)
  var_ceses_liquidador_mancom<-var_ceses%>%str_extract("liquidador m:.*?\\.")%>%gsub("liquidador m:","",.)
  var_ceses_adminSolid<-var_ceses%>%str_extract("adm\\. solid\\..*?\\.")%>%gsub("adm\\. solid\\.:","",.)
  var_ceses_adminMan<-var_ceses%>%str_extract("adm\\. mancom\\..*?\\.")%>%gsub("adm\\. mancom\\.:","",.)
  var_ceses_socprof<-var_ceses%>%str_extract("soc\\.prof\\..*?\\.")%>%gsub("soc\\.prof\\..*:","",.)
  var_ceses_depositorio<-var_ceses%>%str_extract("depositario.*?\\.")%>%gsub("depositario:","",.)
  var_ceses_entidDeposit<-var_ceses%>%str_extract("entiddeposit.*?\\.")%>%gsub("entiddeposit:","",.)
  var_ceses_entdPromo<-var_ceses%>%str_extract("entd\\.promo\\..*")%>%gsub("entd\\.promo\\.:","",.)
  var_ceses_consejero<-var_ceses%>%str_extract("consejero.*?\\.")%>%gsub("consejero:","",.)
  var_ceses_vicepresidente<-var_ceses%>%str_extract("vicepresid\\..*?\\.")%>%gsub("vicepresid\\.:","",.)
  var_ceses_presidente<-var_ceses%>%str_extract("presidente.*?\\.")%>%gsub("presidente:","",.)
  var_ceses_secretario<-var_ceses%>%str_extract("secretario.*?\\.")%>%gsub("secretario:","",.)

  CESES<-data.frame(var_ceses_liquiSoli,var_ceses_apoderado,var_ceses_adminUnico,var_ceses_liquidador,var_ceses_liquidador_mancom,
                    var_ceses_adminSolid,var_ceses_adminMan,var_ceses_socprof,var_ceses_depositorio,
                    var_ceses_entidDeposit,var_ceses_entdPromo,var_ceses_consejero,
                    var_ceses_vicepresidente,var_ceses_presidente,var_ceses_secretario,stringsAsFactors = FALSE)

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
  var_ampliacionCapital<-str_extract(docs,"AMPLIACIÓN DE CAPITAL.*?[A-Z]")
  var_ampliacionCapital<-var_ampliacionCapital%>%gsub("[A-Z]$","",.)

  var_ampliacionCapital_suscrito<-var_ampliacionCapital%>%str_extract("suscrito.*?euros\\.")%>%gsub("suscrito:","",.)
  var_ampliacionCapital_resultanteSuscrito<-var_ampliacionCapital%>%str_extract("resultante suscrito.*?euros\\.")%>%gsub("resultante suscrito:","",.)
  var_ampliacionCapital_desembolsado<-var_ampliacionCapital%>%str_extract("desembolsado.*?euros\\.")%>%gsub("desembolsado:","",.)
  var_ampliacionCapital_resultanteDesembolsado<-var_ampliacionCapital%>%str_extract("resultante desembolsado.*?euros\\.")%>%gsub("resultante desembolsado:","",.)
  var_ampliacionCapital_capital<-var_ampliacionCapital%>%str_extract("capital.*?euros\\.")%>%gsub("capital:","",.)
  AMPLIACIONCAPITAL<-data.frame(var_ampliacionCapital_suscrito,var_ampliacionCapital_resultanteSuscrito,var_ampliacionCapital_desembolsado,
                                var_ampliacionCapital_resultanteDesembolsado,var_ampliacionCapital_capital,stringsAsFactors=FALSE)
  ##REDUCCION
  var_reduccionCapital<-str_extract(docs,"REDUCCIÓN DE CAPITAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  var_reduccionCapital_importeReduccion<-var_reduccionCapital%>%str_extract("importe reducción.*?euros\\.")%>%gsub("importe reducción:","",.)
  var_reduccionCapital_resultante_suscrito<-var_reduccionCapital%>%str_extract("resultante suscrito.*?euros\\.")%>%gsub("resultante suscrito:","",.)

  REDUCCIONCAPITAL<-data.frame(var_reduccionCapital_importeReduccion,var_reduccionCapital_resultante_suscrito,stringsAsFactors=FALSE)

  ##CONSTITUCION
  var_constitucion<-str_extract(docs,"CONSTITUCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  var_constitucion_comienzoOperaciones<-var_constitucion%>%str_extract("comienzo de operaciones.*?\\. ")%>%gsub("comienzo de operaciones:","",.)
  var_constitucion_objetoSocial<-var_constitucion%>%str_extract("objeto social.*?\\. domicilio")%>%gsub("objeto social:","",.)
  var_constitucion_domicilio<-var_constitucion%>%str_extract("domicilio.*?\\)")%>%gsub("domicilio:","",.)
  var_constitucion_capital<-var_constitucion%>%str_extract("capital.*?euros\\.")%>%gsub("capital:","",.)

  CONSTITUCION<-data.frame(var_constitucion_comienzoOperaciones,var_constitucion_objetoSocial,var_constitucion_domicilio,
                           var_constitucion_capital,stringsAsFactors=FALSE)
  ###CAMBIO DENOMINACIÓN SOCIAL

  var_cambioDenominacionSocial<-str_extract(docs,"CAMBIO DE DENOMINACIÓN SOCIAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  CAMBIOSDENOMINACIONSOCIAL<-var_cambioDenominacionSocial
  ###REELECCIONES
  var_reelecciones<-str_extract(docs,"REELECCIONES.*?[A-Z]")%>%gsub("[A-Z]$","",.)

  var_reelecciones_adminUnico<-var_reelecciones%>%str_extract("adm\\. unico.*?\\.")%>%gsub("adm\\. unico:","",.)
  for(i in 1:length(var_reelecciones_adminUnico)){
    if(!is.na(var_reelecciones_adminUnico[i])){
      var_reelecciones_adminUnico[i] <-  letras_mayus(gsub(";",", ",var_reelecciones_adminUnico[i]))
    }
  }
  var_reelecciones_auditor<-var_reelecciones%>%str_extract("auditor.*?\\.")%>%gsub("auditor:","",.)
  var_reelecciones_auditor_supl<-var_reelecciones%>%str_extract("aud\\.supl\\..*?\\.")%>%gsub("aud\\.supl\\.:","",.)
  REELECCIONES<-data.frame(var_reelecciones_adminUnico,var_reelecciones_auditor, var_reelecciones_auditor_supl,stringsAsFactors=FALSE)

  ##REVOCACIONES
  var_revocaciones<-str_extract(docs,"REVOCACIONES.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  var_revocaciones_auditor<-var_revocaciones%>%str_extract("auditor.*?\\.")%>%gsub("auditor:","",.)
  var_revocaciones_apoderado<-var_revocaciones%>%str_extract("apoderado.*?\\.")%>%gsub("apoderado:","",.)
  var_revocaciones_apoderadoMAn<-var_revocaciones%>%str_extract("apo\\.man\\.soli.*?\\.")%>%gsub("apo\\.man\\.soli:","",.)
  var_revocaciones_apoderadoSol<-var_revocaciones%>%str_extract("apo\\.sol\\..*?\\.")%>%gsub("apo\\.sol\\.:","",.)

  REVOCACIONES<-data.frame(var_revocaciones_auditor,var_revocaciones_apoderado,var_revocaciones_apoderadoMAn,
                           var_revocaciones_apoderadoSol,stringsAsFactors=FALSE)

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

  var_fusion<-str_extract(docs,"FUSIÓN POR ABSORCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  var_fusion_sociedadesAbsorbidas<-var_fusion%>%str_extract("sociedades absorbidas.*?\\.")
  FUSION_SOCIEDADESABSORBIDAS<-var_fusion_sociedadesAbsorbidas
  ##MODIFICACIONES ESTATUTARIAS
  var_modificacionEstatuto<-str_extract(docs,"MODIFICACIONES ESTATUTARIAS.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("MODIFICACIONES ESTATUTARIAS\\.","",.)
  MODIFICACIONES_ESTATUARIAs<-var_modificacionEstatuto
  ##CAMBIO DOMICILIO FISCAL
  CAMBIO_DOMICILIO_SOCIAL<-str_extract(docs,"CAMBIO DE DOMICILIO SOCIAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  ##CAMBIO OBJETO SOCIAL
  CAMBIO_OBJETO_SOCIAL<-str_extract(docs,"CAMBIO DE OBJETO SOCIAL.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("CAMBIO DE OBJETO SOCIAL\\.","",.)
  ##EXTINCION
  EXTINCION<-str_extract(docs,"EXTINCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("EXTINCIÓN\\.","",.)

  ##DISOLUCION
  DISOLUCION<-str_extract(docs,"DISOLUCIÓN.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("DISOLUCIÓN\\.","",.)

  ##DECLARACIÓN UNIPERSONALIDAD

  var_declaracionUnipersonalidad<-str_extract(docs,"DECLARACIÓN DE UNIPERSONALIDAD.*?[A-Z]")%>%gsub("[A-Z]$","",.)
  var_declaracionUnipersonalidad_socioUnico<-var_declaracionUnipersonalidad%>%str_extract("socio único.*?\\.")%>%gsub("socio único:","",.)
  DECLARACIONUNIPERSONALIDAD<-data.frame(var_declaracionUnipersonalidad_socioUnico,stringsAsFactors=FALSE)
  ##NO SE DIVIDEN
  OTROS_CONCEPTOS<-str_extract(docs,"OTROS CONCEPTOS.*?[A-Z]")%>%gsub("[A-Z]$","",.)%>%gsub("OTROS CONCEPTOS:","",.)
  DATOS_REGISTRALES<-str_extract(docs,"DATOS REGISTRALES.*")%>%gsub("\\.$","",.)%>%gsub("DATOS REGISTRALES\\.","",.)

  data<-data.frame(EMPRESA,FUSION_SOCIEDADESABSORBIDAS,MODIFICACIONES_ESTATUARIAs,CAMBIOSDENOMINACIONSOCIAL,CAMBIO_OBJETO_SOCIAL,CESES,NOMBRAMIENTOS,AMPLIACIONCAPITAL,DECLARACIONUNIPERSONALIDAD,REDUCCIONCAPITAL,REELECCIONES,REVOCACIONES, `SITUACIÓN CONCURSAL`, ESCISIÓN, TRANSFORMACIÓN, DISOLUCION,EXTINCION,CONSTITUCION,OTROS_CONCEPTOS,DATOS_REGISTRALES,stringsAsFactors=FALSE)
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

  nombreArchivo<-info$keys$Subject
  #write.csv(BBDD,paste('C:\\TechFriendly\\IZARRA\\Borme\\',paste(nombreArchivo,".csv",collapse=""),collapse=""),row.names=F)
  #write_json(BBDD,paste('C:\\TechFriendly\\IZARRA\\Borme\\paquete_borme\\',paste(nombreArchivo,".json",collapse=""),collapse=""),pretty=T)

  json_borme <- toJSON(data,pretty=T)

  #Envío JSON a plataforma
  #TB_token <- "eFbps1EKXC6fpqksxNLX"
  #TB_url   <- paste("http://78.47.39.122:8080/api/v1/",TB_token,"/telemetry",sep="")
  #POST(url=TB_url,body=json_borme)

  return(json_borme)
}
