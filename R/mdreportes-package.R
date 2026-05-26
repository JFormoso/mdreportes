#' mdreportes: Plantillas institucionales de MetaDocencia para Quarto
#'
#' El paquete `mdreportes` proporciona plantillas Quarto con la identidad
#' institucional de MetaDocencia (colores, tipografías, logo) para crear
#' reportes HTML, reportes PDF y presentaciones RevealJS de manera consistente.
#'
#' @section Funciones principales:
#' - [mdreportes::nuevo_documento()]: Crea un nuevo proyecto Quarto a partir de una plantilla institucional.
#' - [mdreportes::actualizar_brand()]: Actualiza el `_brand.yml` en proyectos existentes.
#'
#' @section Tipos de documento disponibles:
#' - `"reporte-html"`: Reporte HTML autocontenido con TOC y numeración de secciones.
#' - `"reporte-pdf"`: Reporte PDF via XeLaTeX con encabezado y pie institucional.
#' - `"presentacion"`: Presentación RevealJS con tema institucional.
#'
#' @docType package
#' @name mdreportes
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  ggplot2::theme_set(mdreportes:::theme_md())
}

