#' Diagnosticar estado del `_brand.yml` en proyectos existentes
#'
#' Compara el `_brand.yml` de uno o varios proyectos con la versión actual
#' del paquete e informa si están actualizados, desactualizados o ausentes.
#'
#' @param proyectos Vector de rutas a directorios de proyectos Quarto.
#'   Por defecto usa el directorio de trabajo actual `"."`.
#'
#' @return Un `data.frame` con columnas `proyecto` y `estado`
#'   (`"actualizado"`, `"desactualizado"`, `"ausente"`), de forma invisible.
#'
#' @examples
#'
#' diagnosticar_brand(proyectos = fs::dir_ls("~/reportes", type = "directory"))
#'
#'
#' @export
diagnosticar_brand <- function(proyectos = ".") {

  brand_paquete <- system.file("brand", "_brand.yml",
                               package = "metadocencia",
                               mustWork = TRUE)
  hash_paquete  <- unname(tools::md5sum(brand_paquete))

  resultados <- lapply(proyectos, function(p) {
    p <- fs::path_expand(p)
    brand_local <- fs::path(p, "_brand.yml")

    estado <- if (!fs::file_exists(brand_local)) {
      "ausente"
    } else if (unname(tools::md5sum(as.character(brand_local))) == hash_paquete) {
      "actualizado"
    } else {
      "desactualizado"
    }

    data.frame(proyecto = as.character(p), estado = estado,
               stringsAsFactors = FALSE)
  })

  resultado_df <- do.call(rbind, resultados)

  # Mostrar resumen con colores en consola
  for (i in seq_len(nrow(resultado_df))) {
    switch(resultado_df$estado[i],
           "actualizado"     = cli::cli_alert_success("{resultado_df$proyecto[i]}"),
           "desactualizado"  = cli::cli_alert_warning("{resultado_df$proyecto[i]}"),
           "ausente"         = cli::cli_alert_danger("{resultado_df$proyecto[i]}")
    )
  }

  invisible(resultado_df)
}
