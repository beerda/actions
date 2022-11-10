#'
#' @return
#' @author Michal Burda
#' @export
init_build <- function(job) {
    assert_that(is_job(job))

    res <- lapply(job, function(a) {
        a$status <- 'not executed'
        a$succeeded <- FALSE
        a$failed <- FALSE
        a$finished <- FALSE

        a
    })
    attr(res, 'class') <- 'job'
    attr(res, 'init_build') <- TRUE

    res
}
