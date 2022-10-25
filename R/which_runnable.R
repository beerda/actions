#'
#' @return
#' @author Michal Burda
#' @export
which_runnable <- function(job) {
    assert_that(is_job(job))

    prereq <- prerequisites(job) > 0
    invalid <- which_invalid(job)
    valid_prereq <- (invalid %*% prereq) == 0
    valid_prereq <- as.vector(valid_prereq)

    invalid & valid_prereq
}
