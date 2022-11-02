#' Return a logical vector indicating `job` actions that are dependent on actions indicated
#' with the `what` logical vector.
#'
#' @return
#' @author Michal Burda
#' @export
dependants <- function(job, what) {
    assert_that(is_job(job))
    assert_that(is.vector(what) && is.logical(what))

    what <- rep_len(what, length(job))
    what <- matrix(what, nrow = 1)
    prereq <- prerequisites(job) > 0
    while (TRUE) {
        old <- what
        what <- (what + what %*% prereq) > 0
        if (all(old == what))
            break
    }

    as.vector(what)
}
