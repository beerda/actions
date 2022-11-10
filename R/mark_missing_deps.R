#'
#' @return
#' @author Michal Burda
#' @export
mark_missing_deps <- function(job) {
    assert_that(is_job(job, TRUE))

    miss <- missing_deps(job)

    res <- lapply(seq_along(job), function(i) {
        a <- job[[i]]
        m <- miss[[i]]
        if (length(m) > 0) {
            a$status <- paste0('no action to build: ', paste0(m, collapse = ', '))
            a$failed <- TRUE
            a$finished <- TRUE
        }

        a
    })
    attr(res, 'class') <- 'job'
    attr(res, 'init_build') <- TRUE

    res
}
