#'
#' @return
#' @author Michal Burda
#' @export
is_obsolete <- function(action) {
    assert_that(is_action(action))

    if (!all(file.exists(action$targets))) {
        return(TRUE)
    }

    if (is.null(action$depends)) {
        return(FALSE)
    }

    if (!all(file.exists(action$depends))) {
        return(TRUE)
    }

    dependsMtime <- file.mtime(action$depends)
    targetsMtime <- file.mtime(action$targets)

    return(min(targetsMtime) < max(dependsMtime))
}
