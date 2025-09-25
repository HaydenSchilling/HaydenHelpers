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
#' @param max_width Optional total table width to fit to (numeric). If NULL, no fit.
#' @param unit Units for max_width ("in" or "cm").#' @usage DPIRD_flextable(df)
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

#' @return A flextable object.
#'
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
    max_width    = NULL,
    unit         = c("cm","in")
) {
  unit <- match.arg(unit)

  # Build the table
  ft <- flextable::flextable(data)

  # Header styling
  ft <- flextable::bg(ft, part = "header", bg = header_bg)
  if (isTRUE(header_bold)) {
    ft <- flextable::bold(ft, part = "header", bold = TRUE)
  }

  # Number formatting (all numeric columns)
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

  # Font + compact layout
  ft <- flextable::font(ft, part = "all", fontname = fontname)
  ft <- flextable::padding(
    ft, part = "all",
    padding.top = pad_top, padding.bottom = pad_bottom,
    padding.left = pad_left, padding.right = pad_right
  )
  ft <- flextable::line_spacing(ft, part = "all", space = line_space)

  # Let flextable size based on final content
  ft <- flextable::autofit(ft)

  # Optionally fit the entire table to a target width
  if (!is.null(max_width)) {
    ft <- flextable::fit_to_width(ft, max_width = max_width, unit = unit)
  }

  ft
}





