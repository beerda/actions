#'
#' @return
#' @author Michal Burda
#' @export
targets <- function(job) {
    assert_that(is_job(job))

    res <- lapply(job, function(action) action$targets)
    res <- unique(unlist(res))

    if (is.null(res)) {
        return(character(0))
    }

    res
}
