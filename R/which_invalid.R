#'
#' @return
#' @author Michal Burda
#' @export
which_invalid <- function(job) {
    assert_that(is_job(job))

    obsolete <- sapply(job, is_obsolete)

    dependants(job, obsolete)
}
