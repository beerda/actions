#'
#' @return
#' @author Michal Burda
#' @export
prepare_table <- function(cols, min_size = 1, total_size = console_width()) {
    sizes <- lapply(cols, nchar)
    sizes <- unlist(lapply(sizes, max))
    widths <- col_widths(sizes, min_size, total_size)

    for (i in seq_along(cols)) {
        v <- cols[[i]]
        v <- str_trunc(v, widths[i], ellipsis = '…')
        v <- str_pad(v, widths[i], side = 'right')
        cols[[i]] <- v
    }

    cols
}
