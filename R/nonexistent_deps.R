#'
#' @return
#' @author Michal Burda
#' @export
nonexistent_deps <- function(action) {
    assert_that(is_action(action))

    res <- NULL
    if (!is.null(action$depends)) {
        nonexistent <- !file.exists(action$depends)
        res <- action$depends[nonexistent]
    }

    res
}
