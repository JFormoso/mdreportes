#' Crear un nuevo documento Quarto con plantilla institucional de MetaDocencia
#'
#' Crea un nuevo directorio con todos los archivos necesarios para generar
#' un documento Quarto con la identidad institucional de MetaDocencia:
#' colores, tipografías, logo y estructura de secciones sugerida.
#'
#' @param nombre Nombre del proyecto/documento. Se usará como nombre del
#'   directorio y del archivo `.qmd` principal. Por defecto `"documento"`.
#' @param tipo Tipo de documento a crear. Uno de:
#'   - `"reporte-html"`: Reporte HTML autocontenido.
#'   - `"reporte-pdf"`: Reporte PDF via XeLaTeX.
#'   - `"presentacion"`: Presentación RevealJS.
#' @param directorio Ruta del directorio donde se creará el proyecto.
#'   Por defecto el directorio de trabajo actual `"."`.
#' @param abrir Si `TRUE` (por defecto), abre el proyecto en RStudio/Positron
#'   si está disponible.
#'
#' @return Ruta al directorio creado, de forma invisible.
#'
#' @examples
#' # Reporte HTML
#' nuevo_documento("informe-anual", tipo = "reporte-html")
#'
#' # Reporte PDF
#' nuevo_documento("memoria-2025", tipo = "reporte-pdf")
#'
#' # Presentación
#' nuevo_documento("reunion-equipo", tipo = "presentacion")
#'
#' # En un directorio específico
#' nuevo_documento("informe-q3", tipo = "reporte-html", directorio = "~/reportes")
#'
#'
#' @export
nuevo_documento <- function(
    nombre     = "documento",
    tipo       = c("reporte-html", "reporte-pdf", "presentacion"),
    directorio = ".",
    abrir      = TRUE
) {
  tipo <- match.arg(tipo)

  # Validar nombre (sin espacios ni caracteres especiales)
  if (grepl("[^a-zA-Z0-9_\\-]", nombre)) {
    cli::cli_abort(c(
      "El nombre {.val {nombre}} contiene caracteres no permitidos.",
      "i" = "Usa solo letras, números, guiones ({.code -}) y guiones bajos ({.code _})."
    ))
  }

  origen  <- system.file("templates", tipo, "skeleton",
                         package = "mdreportes",
                         mustWork = TRUE)
  destino <- fs::path(directorio, nombre)

  # Verificar que no exista ya
  if (fs::dir_exists(destino)) {
    cli::cli_abort(c(
      "Ya existe un directorio con ese nombre: {.path {destino}}",
      "i" = "Elegí un nombre diferente o eliminá el directorio existente."
    ))
  }

  cli::cli_progress_step("Creando {tipo} en {.path {destino}}")

  # Copiar el skeleton completo
  fs::dir_copy(origen, destino)

  # Copiar _brand.yml desde la fuente central del paquete
  brand_origen <- system.file("brand", "_brand.yml",
                              package = "mdreportes",
                              mustWork = TRUE)
  fs::file_copy(brand_origen, fs::path(destino, "_brand.yml"), overwrite = TRUE)

  # Copiar logo desde la fuente central
  logo_origen <- system.file("brand", "logo.png",
                             package = "mdreportes",
                             mustWork = TRUE)
  fs::file_copy(logo_origen, fs::path(destino, "assets", "logo.png"), overwrite = TRUE)

  # Renombrar skeleton.qmd → nombre.qmd
  fs::file_move(
    fs::path(destino, "skeleton.qmd"),
    fs::path(destino, paste0(nombre, ".qmd"))
  )

  cli::cli_progress_done()
  cli::cli_alert_success("Documento creado: {.path {destino}}")
  cli::cli_bullets(c(
    "*" = "Abrí {.file {nombre}.qmd} para empezar a editar.",
    "*" = "Renderizá con {.code quarto render {nombre}.qmd} o el botón Render del IDE."
  ))

  # Abrir proyecto en RStudio/Positron si está disponible
  if (abrir && rstudioapi::isAvailable()) {
    rstudioapi::openProject(as.character(destino), newSession = TRUE)
  }

  invisible(as.character(destino))
}
