#'
#' @return
#' @author Michal Burda
#' @export
mark_failed_deps <- function(job) {
    assert_that(is_job(job, TRUE))

    failed <- unlist(props(job, 'failed'))
    deps <- dependants(job, failed)
    deps <- deps & !failed
    props(job, 'status')[deps] <- 'Failed prerequisite action'
    props(job, 'failed')[deps] <- TRUE
    props(job, 'finished')[deps] <- TRUE
    attr(job, 'init_build') <- TRUE

    job
}
