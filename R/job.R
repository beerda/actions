#'
#' @return
#' @author Michal Burda
#' @export
job <- function(...) {
    res <- list(...)
    res <- structure(res,
                     class = 'job')

    assert_that(is_job(res))

    res
}
