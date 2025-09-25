#' Formats a flextable to match DPRID template
#'
#' @param data A data.frame or tibble.
#' @param header_bg Header background colour (default matches your example).
#' @param header_bold Logical; bold header text?
#' @param fontname Font family name to use everywhere (Word/PPT must have it installed).
#' @param digits Number of decimal places for numeric columns.
#' @param big.mark Thousands separator.
#' @param decimal.mark Decimal point character.
#' @param pad_top Cell padding (pts).
#' @param pad_bottom Cell padding (pts).
#' @param pad_left Cell padding (pts).
#' @param pad_right Cell padding (pts).
#' @param line_space Line spacing inside cells (1 = single).
#' @param max_width Maximum width to fit to (default 19). Use NULL to skip.
#' @param unit Units for max_width ("cm" or "in").
#' @param lock_width Logical; if TRUE, lock column widths (layout = "fixed").
#' @return A formatted table
#' @export
#'
#' @examples
#' # Example dataset
#'  df_example <- data.frame(
#'  Species = c("Snapper", "Bream", "Flathead"),
#'  Count   = c(1200, 25000, 345678),
#'  Biomass = c(1234.56, 78901.23, 4567890.12)
#')
#' # Create a formatted flextable
#'ft_example <- DPIRD_flextable(
#'  data = df_example,
#'  digits = 2,          # 2 decimal places for numeric columns
#'  max_width = 16,      # Fit table to 16 cm width
#'  unit = "cm"
#')
#'ft_example

DPIRD_flextable <- function(
    data,
    header_bg    = "#CBEDFD",
    header_bold  = TRUE,
    fontname     = "Public Sans Light",
    digits       = 2,
    big.mark     = ",",
    decimal.mark = ".",
    pad_top      = 1,
    pad_bottom   = 1,
    pad_left     = 2,
    pad_right    = 2,
    line_space   = 0.9,
    max_width    = 19,
    unit         = c("cm", "in"),
    lock_width   = FALSE
) {
  unit <- match.arg(unit)

  # Allow tables/matrices as input
  if (inherits(data, "table") || inherits(data, "ftable")) {
    data <- as.data.frame(data)
  } else if (is.matrix(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  # Build flextable
  ft <- flextable::flextable(data)

  # Header styling
  ft <- flextable::bg(ft, part = "header", bg = header_bg)
  if (isTRUE(header_bold)) {
    ft <- flextable::bold(ft, part = "header", bold = TRUE)
  }

  # Number formatting for all numeric columns
  num_cols <- names(Filter(is.numeric, data))
  if (length(num_cols) > 0) {
    ft <- flextable::colformat_num(
      x            = ft,
      j            = num_cols,
      big.mark     = big.mark,
      decimal.mark = decimal.mark,
      digits       = digits
    )
  }

  # Font + compact spacing
  ft <- flextable::font(ft, part = "all", fontname = fontname)
  ft <- flextable::padding(
    ft, part = "all",
    padding.top = pad_top, padding.bottom = pad_bottom,
    padding.left = pad_left, padding.right = pad_right
  )
  ft <- flextable::line_spacing(ft, part = "all", space = line_space)

  # Size to content first
  ft <- flextable::autofit(ft)

  # Cap to max width (shrinks only if needed)
  if (!is.null(max_width)) {
    ft <- flextable::fit_to_width(ft, max_width = max_width, unit = unit)
  }

  # Toggle width locking for Word/PPT
  ft <- flextable::set_table_properties(
    ft,
    layout = if (isTRUE(lock_width)) "fixed" else "autofit"
  )

  ft
}


