#'
#' @return
#' @author Michal Burda
col_widths <- function(cols, sep, minimum, total) {
    total <- total - sep * (length(cols) - 1)
    res <- pmin(cols, minimum)

    while (sum(res) < total) {
        satisfied <- cols <= res
        if (all(satisfied))
            break

        remaining <- total - sum(res)
        add <- floor(remaining / sum(!satisfied))
        if (add > 0) {
            res[!satisfied] <- pmin(cols[!satisfied], res[!satisfied] + add)
        } else {
            res[!satisfied][seq_len(remaining)] <- 1 + res[!satisfied][seq_len(remaining)]
        }
    }

    res
}


