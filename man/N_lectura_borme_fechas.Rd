% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NscrapBorme_fechas_func.R
\name{N_lectura_borme_fechas}
\alias{N_lectura_borme_fechas}
\title{Scrapea los Borme para el rango de fechas y las provincias especificadas. Envía el resultado a la plataforma Smart City.}
\usage{
N_lectura_borme_fechas(municipio, radio, provincias, fecha = Sys.Date())
}
\arguments{
\item{provincias, }{radio, municipio, rfechas}
}
\value{
json
}
\description{
Scrapea los Borme en PDF en el rango de fechas especificado y de las provincias especificadas (en caso de introducir más de una provincia estas deben estar separadas por comas),
en base a un minicipio de referencia y un radio de distancia en km.
Devuelve un json con con los campos reflejados por empresa en el borme y lo envía a la plataforma Smart City.
}
\examples{
 N_lectura_borme_fechas('Ermua', 30, 'Bizkaia, Gipuzkoa, Araba', "2020-03-01, 2020-03-30")

}
