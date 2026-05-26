#' Paleta de colores institucional de MetaDocencia
#'
#' Vector con los cinco colores institucionales en orden recomendado
#' para uso en gráficos categóricos.
#'
#' @format Vector de caracteres con códigos hex.
#'
#' @examples
#' paleta_md
#' scales::show_col(paleta_md)
#'
#' @export
paleta_md <- c(
  "#C83737",  # 1 — Rojo MD       (Pantone 180C)
  "#C86E37",  # 2 — Rojo-naranja  (análogo cálido)
  "#00506F",  # 3 — Azul MD       (Pantone 3025C)
  "#F77B20",  # 4 — Naranja MD    (Pantone 021C)
  "#00788F"   # 5 — Azul-verde    (análogo frío)
)


#' Escala de color discreta con paleta MetaDocencia
#'
#' Para usar con `ggplot2::scale_color_*` y `scale_fill_*`.
#'
#' @param ... Argumentos adicionales pasados a [ggplot2::discrete_scale()].
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(mpg, aes(displ, hwy, color = class)) +
#'   geom_point(size = 3) +
#'   scale_color_md() +
#'   theme_md()
#'
#'
#' @export
scale_color_md <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette    = function(n) {
      if (n > length(paleta_md)) {
        cli::cli_warn(c(
          "La paleta MetaDocencia tiene {length(paleta_metadocencia)} colores,",
          "pero se solicitaron {n}. Los colores se reciclar\u00e1n."
        ))
      }
      rep_len(paleta_md, n)
    },
    ...
  )
}


#' @rdname scale_color_md
#' @export
scale_colour_md <- scale_color_md


#' Escala de relleno discreta con paleta MetaDocencia
#'
#' @inheritParams scale_color_md
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(class, fill = class)) +
#'   geom_bar() +
#'   scale_fill_md() +
#'   theme_md()
#'
#'
#' @export
scale_fill_md <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette    = function(n) {
      if (n > length(paleta_md)) {
        cli::cli_warn(c(
          "La paleta MetaDocencia tiene {length(paleta_metadocencia)} colores,",
          "pero se solicitaron {n}. Los colores se reciclar\u00e1n."
        ))
      }
      rep_len(paleta_md, n)
    },
    ...
  )
}


#' Tema ggplot2 institucional de MetaDocencia
#'
#' Aplica la identidad visual de MetaDocencia a gráficos ggplot2:
#' tipografía Barlow para títulos y ejes, Open Sans para el resto,
#' y estilo limpio con grilla horizontal suave.
#'
#' @param base_size Tamaño base de fuente en puntos. Por defecto `12`.
#' @param base_family Familia tipográfica base. Por defecto `"Open Sans"`.
#'
#' @return Un objeto `theme` de ggplot2.
#'
#' library(ggplot2)
#'
#' ggplot(mpg, aes(displ, hwy, color = class)) +
#'   geom_point(size = 3) +
#'   labs(
#'     title    = "Desplazamiento vs eficiencia",
#'     subtitle = "Datos de automóviles",
#'     x        = "Desplazamiento del motor (L)",
#'     y        = "Eficiencia en autopista (mpg)"
#'   ) +
#'   scale_color_md() +
#'   theme_md()
#'
#'
#' @export
theme_md <- function(base_size = 12, base_family = "Open Sans") {

  # Colores institucionales
  azul  <- "#00506F"
  rojo  <- "#C83737"
  gris_texto <- "#404040"
  gris_suave <- "#F5F8FA"
  gris_linea <- "#DDDDDD"

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(

      # --- Texto base ---
      text = ggplot2::element_text(
        family = "Open Sans",
        color  = gris_texto
      ),

      # --- Título principal: Barlow bold, color gris institucional ---
      plot.title = ggplot2::element_text(
        family = "Barlow",
        face   = "bold",
        size   = ggplot2::rel(1.4),
        color  = gris_texto,
        margin = ggplot2::margin(b = 4)
      ),

      # --- Subtítulo: Barlow regular, azul MD ---
      plot.subtitle = ggplot2::element_text(
        family = "Barlow",
        face   = "plain",
        size   = ggplot2::rel(1.0),
        color  = azul,
        margin = ggplot2::margin(b = 12)
      ),

      # --- Caption: Open Sans pequeño ---
      plot.caption = ggplot2::element_text(
        family = "Open Sans",
        size   = ggplot2::rel(0.75),
        color  = "#888888",
        hjust  = 1,
        margin = ggplot2::margin(t = 8)
      ),

      # --- Títulos de ejes: Barlow bold, azul MD ---
      axis.title = ggplot2::element_text(
        family = "Barlow",
        face   = "bold",
        size   = ggplot2::rel(0.95),
        color  = azul
      ),

      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 8)
      ),

      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 8)
      ),

      # --- Etiquetas de ejes: Open Sans ---
      axis.text = ggplot2::element_text(
        family = "Open Sans",
        size   = ggplot2::rel(0.85),
        color  = gris_texto
      ),

      # --- Ticks de ejes ---
      axis.ticks = ggplot2::element_line(color = gris_linea, linewidth = 0.3),

      # --- Grilla: solo horizontal, suave ---
      panel.grid.major.y = ggplot2::element_line(
        color     = gris_linea,
        linewidth = 0.4
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),

      # --- Fondo del panel ---
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),

      # --- Eje x: línea base azul MD ---
      axis.line.x = ggplot2::element_line(color = azul, linewidth = 0.6),
      axis.line.y = ggplot2::element_blank(),

      # --- Leyenda: Open Sans, compacta ---
      legend.title = ggplot2::element_text(
        family   = "Barlow",
        face     = "bold",
        size     = ggplot2::rel(0.9),
        color    = gris_texto
      ),
      legend.text = ggplot2::element_text(
        family = "Open Sans",
        size   = ggplot2::rel(0.85),
        color  = gris_texto
      ),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key        = ggplot2::element_rect(fill = "white", color = NA),
      legend.position   = "bottom",

      # --- Facetas ---
      strip.text = ggplot2::element_text(
        family = "Barlow",
        face   = "bold",
        size   = ggplot2::rel(0.9),
        color  = "white"
      ),
      strip.background = ggplot2::element_rect(fill = azul, color = NA),

      # --- Márgenes del plot ---
      plot.margin = ggplot2::margin(16, 16, 12, 16)
    )
}
