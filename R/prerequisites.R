#'
#' @return
#' @author Michal Burda
#' @export
prerequisites <- function(job) {
    assert_that(is_job(job))

    indices <- seq_along(job)

    outer(indices, indices, Vectorize(function(x, y) {
        length(intersect(job[[x]]$targets, job[[y]]$depends))
    }))
}
