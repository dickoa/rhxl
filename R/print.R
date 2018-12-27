#' @export
#' @rdname formatting
#' @importFrom stats setNames
trunc_mat_hxl <- function(x, n = NULL, width = NULL, n_extra = NULL) {
  rows <- nrow(x)

  if (is.null(n)) {
    if (is.na(rows) || rows > tibble_opt("print_max")) {
      n <- tibble_opt("print_min")
    } else {
      n <- rows
    }
  }
  n_extra <- n_extra %||% tibble_opt("max_extra_cols")

  df <- as.data.frame(head(x, n))
  width <- tibble_width(width)

  shrunk <- shrink_mat(df, width, rows, n, star = has_rownames(x))
  trunc_info <- list(width = width,
                    rows_total = rows,
                    rows_min = nrow(df),
                    n_extra = n_extra,
                    summary = tbl_sum(x),
                    schema = )

  structure(c(shrunk, trunc_info), class = "trunc_mat_hxl")
}

print.trunc_mat_hxl <- function (x, ...) {
    print_summary_hxl(x)
    tibble::print_table(x)
    tibble::print_extra(x)
    print_hxl_schema(x)
    invisible(x)
}
