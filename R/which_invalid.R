#'
#' @return
#' @author Michal Burda
#' @export
which_invalid <- function(job) {
    assert_that(is_job(job))

    obsoletes <- sapply(job, is_obsolete)
    invalids <- matrix(obsoletes, nrow = 1)
    prereq <- prerequisites(job) > 0
    while (TRUE) {
        old <- invalids
        invalids <- (invalids + invalids %*% prereq) > 0
        if (all(old == invalids))
            break
    }

    as.vector(invalids)
}

