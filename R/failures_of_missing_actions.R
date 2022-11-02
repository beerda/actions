#'
#' @return
#' @author Michal Burda
#' @export
failures_of_missing_actions <- function(job) {
    assert_that(is_job(job))

    miss <- missing_deps(job)

    lapply(miss, function(deps) {
        if (length(deps) <= 0) {
            return(NULL)
        }
        paste0('Missing action for: ', paste0(deps, collapse = ', '))
    })
}
