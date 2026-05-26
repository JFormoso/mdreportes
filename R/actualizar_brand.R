#' Actualizar el `_brand.yml` en proyectos existentes
#'
#' Reemplaza el archivo `_brand.yml` en uno o varios proyectos con la versión
#' actual del paquete. Útil cuando la identidad institucional cambia y se necesita
#' propagar las actualizaciones a proyectos ya creados.
#'
#' @param proyectos Vector de rutas a directorios de proyectos Quarto.
#'   Por defecto usa el directorio de trabajo actual `"."`.
#' @param confirmar Si `TRUE` (por defecto), pide confirmación interactiva
#'   antes de sobreescribir cada archivo.
#'
#' @return Vector de rutas actualizadas, de forma invisible.
#'
#' @examples
#'
#' # Actualizar el proyecto actual
#' actualizar_brand()
#'
#' # Actualizar varios proyectos
#' actualizar_brand(
#'   proyectos = c("~/reportes/informe-q3", "~/reportes/memoria-anual")
#' )
#'
#' # Sin confirmación (para scripts)
#' actualizar_brand(proyectos = fs::dir_ls("~/reportes"), confirmar = FALSE)
#'
#'
#' @export
actualizar_brand <- function(proyectos = ".", confirmar = TRUE) {

  brand_nuevo <- system.file("brand", "_brand.yml",
                             package = "metadocencia",
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

    # Verificar si ya está actualizado
    if (fs::file_exists(brand_destino)) {
      hash_local <- unname(tools::md5sum(as.character(brand_destino)))
      if (hash_local == hash_nuevo) {
        cli::cli_alert_info("{.path {proyecto}}: ya está actualizado, omitiendo.")
        next
      }
    }

    # Pedir confirmación si corresponde
    if (confirmar && fs::file_exists(brand_destino)) {
      respuesta <- readline(
        paste0("  ¿Sobreescribir _brand.yml en '", proyecto, "'? [s/N]: ")
      )
      if (!tolower(trimws(respuesta)) %in% c("s", "si", "sí", "y", "yes")) {
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
