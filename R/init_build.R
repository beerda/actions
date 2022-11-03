#'
#' @return
#' @author Michal Burda
#' @export
init_build <- function(job) {
    assert_that(is_job(job))

    res <- lapply(job, function(a) {
        a$status <- 'Not executed'
        a$succeeded <- FALSE
        a$failed <- FALSE
        a$finished <- FALSE

        a
    })
    attr(res, 'init_build') <- TRUE

    res
}
