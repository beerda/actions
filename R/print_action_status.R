#'
#' @return
#' @author Michal Burda
#' @export
print_action_status <- function(action,
                                type = c('depends', 'targets')) {
    assert_that(is_action(action))
    assert_that(is.string(type))

    type <- match.arg(type)
    icon_map <- list(`newer` = '• ', `older` = '  ', `non-existent` = '✖ ')
    color_map <- list(`newer` = col_br_green, `older` = col_br_white, `non-existent` = col_br_red)

    res <- NULL
    if (type == 'depends') {
        item <- action$depends
        state <- status(action, 'depends')
        res <- col_white('── Dependencies ──\n')
    } else {
        item <- action$targets
        state <- status(action, 'targets')
        res <- col_white('── Targets ──\n')
    }

    tbl <- prepare_table(list(item, state), 12, console_width() - 5)
    for (i in seq_along(item)) {
        color <- color_map[[state[i]]]
        icon <- icon_map[[state[i]]]
        res <- c(res,
                 color(icon),
                 color(tbl[[1]][i]),
                 col_white(' - '),
                 col_white(tbl[[2]][i]),
                 '\n')
    }

    cat(paste0(res, collapse = ''))
}
