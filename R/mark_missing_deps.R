#'
#' @return
#' @author Michal Burda
#' @export
mark_missing_deps <- function(job) {
    assert_that(is_job(job))

    miss <- missing_deps(job)

    lapply(seq_along(job), function(i) {
        a <- job[[i]]
        m <- miss[[i]]
        if (length(m) > 0) {
            a$status <- paste0('No action to build: ', paste0(m, collapse = ', '))
        }

        a
    })
}