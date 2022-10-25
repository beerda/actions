#'
#' @return
#' @author Michal Burda
#' @export
which_invalid <- function(job) {
    assert_that(is_job(job))

    obsolete <- sapply(job, is_obsolete)
    invalid <- matrix(obsolete, nrow = 1)
    prereq <- prerequisites(job) > 0
    while (TRUE) {
        old <- invalid
        invalid <- (invalid + invalid %*% prereq) > 0
        if (all(old == invalid))
            break
    }

    as.vector(invalid)
}

