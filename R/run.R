#'
#' @return
#' @author Michal Burda
#' @export
run <- function(action) {
    assert_that(is_action(action))

    pre_time <- file.mtime(action$targets)
    res <- list(label = action$label,
                nonexistent_deps = nonexistent_deps(action))

    cat(rule(left = 'Build started', right = action$label, line = 2), '\n')
    cat(rule(left = 'Dependencies', right = action$label), '\n')
    item <- action$depends
    state <- status(action, 'depends')
    tbl <- prepare_table(list(item, state), 12, console_width() - 1)
    for (i in seq_along(item)) {
        cat(col_br_white(tbl[[1]][i]))
        cat(' ')
        cat(col_br_white(tbl[[2]][i]))
        cat('\n')
    }
    cat('\n')

    res$value <- try(silent = FALSE, {
        do.call(action$fun, action$args)
    })

    if (inherits(res$value, 'try-error')) {
        res$ok <- FALSE

    } else {
        res$ok <- TRUE
        res$nonupdated_targets <- nonupdated_targets(action, pre_time)
    }

    cat(rule(left = 'Targets', right = action$label), '\n')
    item <- action$targets
    state <- status(action, 'targets')
    tbl <- prepare_table(list(item, state), 12, console_width() - 1)
    for (i in seq_along(item)) {
        cat(col_br_white(tbl[[1]][i]))
        cat(' ')
        cat(col_br_white(tbl[[2]][i]))
        cat('\n')
    }
    cat(rule(left = 'Build finished', right = action$label, line = 2), '\n')

    return(res)
}

