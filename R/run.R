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
            cli_abort('Prerequisite files do not exist: {nonexistent}')
        }
    }

    pretime <- file.mtime(action$targets)

    cli_inform('Running action')

    posttime <- file.mtime(action$targets)
    updated <- (is.na(pretime) & !is.na(posttime)) |
        (!is.na(pretime) & !is.na(posttime) & pretime < posttime)

    if (!all(updated)) {
        nonupdated <- action$targets[!updated]
        cli_abort('Targets not updated: {nonupdated}')
    }
}
