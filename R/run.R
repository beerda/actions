#'
#' @return
#' @author Michal Burda
#' @export
run <- function(action) {
    assert_that(is_action(action))

    if (!is.null(action$depends)) {
        nonexistent <- !file.exists(action$depends)
        nonexistent <- action$depends[nonexistent]
        if (length(nonexistent) > 0) {
            nonexistent <- paste(nonexistent, sep = ', ')
            cli_warn('Prerequisite files do not exist: {nonexistent}')
        }
    }

    label <- paste(action$targets, sep = ', ')
    pretime <- file.mtime(action$targets)

    res <- try(silent = FALSE, {
        do.call(action$fun, action$args)
    })

    if (inherits(res, 'try-error')) {
        return(FALSE)
    }

    posttime <- file.mtime(action$targets)
    updated <- (is.na(pretime) & !is.na(posttime)) |
        (!is.na(pretime) & !is.na(posttime) & pretime < posttime)

    if (!all(updated)) {
        nonupdated <- action$targets[!updated]
        cli_warn('Targets not updated: {nonupdated}')
    }

    return(TRUE)
}
