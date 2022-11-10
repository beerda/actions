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
    print_status(action, 'depends')
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

    print_status(action, 'targets')
    cat(rule(left = 'Build finished', right = action$label, line = 2), '\n')

    return(res)
}

