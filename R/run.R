#'
#' @return
#' @author Michal Burda
#' @export
run <- function(action) {
    assert_that(is_action(action))

    pre_time <- file.mtime(action$targets)
    res <- list(label = action$label,
                nonexistent_deps = nonexistent_deps(action))

    cat(rule(left = paste0(style_bold('Action started'), ' (', Sys.time(), ')'),
             right = action$label,
             line = 1,
             col = col_white))
    print_action_status(action, 'depends')
    cat('\n')

    start_time <- Sys.time()
    res$value <- try(silent = FALSE, {
        do.call(action$fun, action$args)
    })
    end_time <- Sys.time()
    diff_time <- as.numeric(end_time - start_time)
    diff_time <- round(diff_time, 1)


    if (inherits(res$value, 'try-error')) {
        res$ok <- FALSE

    } else {
        res$ok <- TRUE
        res$nonupdated_targets <- nonupdated_targets(action, pre_time)
    }

    print_action_status(action, 'targets')
    cat(rule(left = paste0(style_bold('Action finished'), ' (', diff_time, ' s)'),
             right = action$label,
             line = 1,
             col = col_white))

    return(res)
}

