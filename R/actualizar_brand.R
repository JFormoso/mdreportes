#' Actualizar el `_brand.yml` en proyectos existentes
#'
#' Reemplaza el archivo `_brand.yml` en uno o varios proyectos con la versi\u00f3n
#' actual del paquete. \u00fatil cuando la identidad institucional cambia y se necesita
#' propagar las actualizaciones a proyectos ya creados.
#'
#' @param proyectos Vector de rutas a directorios de proyectos Quarto.
#'   Por defecto usa el directorio de trabajo actual `"."`.
#' @param confirmar Si `TRUE` (por defecto), pide confirmaci\u00f3n interactiva
#'   antes de sobreescribir cada archivo.
#'
#' @return Vector de rutas actualizadas, de forma invisible.
#'
#' @examples
#' \dontrun{
#' # Actualizar el proyecto actual
#' actualizar_brand()
#'
#' # Actualizar varios proyectos
#' actualizar_brand(
#'   proyectos = c("~/reportes/informe-q3", "~/reportes/memoria-anual")
#' )
#'
#' # Sin confirmaci\u00f3n (para scripts)
#' actualizar_brand(proyectos = fs::dir_ls("~/reportes"), confirmar = FALSE)
#'}
#'
#' @export
actualizar_brand <- function(proyectos = ".", confirmar = TRUE) {

  brand_nuevo <- system.file("brand", "_brand.yml",
                             package = "mdreportes",
                             mustWork = TRUE)
  hash_nuevo  <- unname(tools::md5sum(brand_nuevo))

  actualizados <- character(0)
  omitidos     <- character(0)

  for (proyecto in proyectos) {

    proyecto <- fs::path_expand(proyecto)

    # Verificar que el directorio existe
    if (!fs::dir_exists(proyecto)) {
      cli::cli_warn("No existe el directorio: {.path {proyecto}}")
      next
    }

    brand_destino <- fs::path(proyecto, "_brand.yml")

    # Verificar si ya est<c3><a1> actualizado
    if (fs::file_exists(brand_destino)) {
      hash_local <- unname(tools::md5sum(as.character(brand_destino)))
      if (hash_local == hash_nuevo) {
        cli::cli_alert_info("{.path {proyecto}}: ya est\u00e1 actualizado, omitiendo.")
        next
      }
    }

    # Pedir confirmaci\u00f3n si corresponde
    if (confirmar && fs::file_exists(brand_destino)) {
      respuesta <- readline(
        paste0("  \u00bfSobreescribir _brand.yml en '", proyecto, "'? [s/n]: ")
      )
      if (!tolower(trimws(respuesta)) %in% c("s", "si", "s\u00ed", "y", "yes")) {
        cli::cli_alert_warning("Omitido: {.path {proyecto}}")
        omitidos <- c(omitidos, proyecto)
        next
      }
    }

    fs::file_copy(brand_nuevo, brand_destino, overwrite = TRUE)
    cli::cli_alert_success("Actualizado: {.path {proyecto}}")
    actualizados <- c(actualizados, proyecto)
  }

  # Resumen final
  cli::cli_rule()
  cli::cli_bullets(c(
    "v" = "{length(actualizados)} proyecto(s) actualizado(s).",
    "!" = "{length(omitidos)} proyecto(s) omitido(s)."
  ))

  invisible(actualizados)
}
