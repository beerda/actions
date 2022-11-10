#'
#' @return
#' @author Michal Burda
#' @export
print_status <- function(action,
                         type = c('depends', 'targets')) {
    assert_that(is_action(action))
    assert_that(is.string(type))

    type <- match.arg(type)
    icon_map <- c(`newer` = '• ', `older` = '  ', `non-existent` = '✖ ')

    if (type == 'depends') {
        item <- action$depends
        state <- status(action, 'depends')
        cat('── Dependencies ──\n')
    } else {
        item <- action$targets
        state <- status(action, 'targets')
        cat('── Targets ──\n')
    }

    tbl <- prepare_table(list(item, state), 12, console_width() - 3)

    for (i in seq_along(item)) {
        cat(icon_map[state[i]])
        cat(col_br_red(tbl[[1]][i]))
        cat(' ')
        cat(col_br_white(tbl[[2]][i]))
        cat('\n')
    }
}
