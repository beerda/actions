#'
#' @return
#' @author Michal Burda
#' @export
which_runnable <- function(job,
                           running = rep(FALSE, length(job))) {
    assert_that(is_job(job))
    assert_that(is.vector(running) && is.logical(running) && length(running) == length(job))

    prereq <- prerequisites(job) > 0
    invalid <- which_invalid(job)
    valid_prereq <- (invalid %*% prereq) == 0
    valid_prereq <- as.vector(valid_prereq)
    running_prereq <- dependants(job, running)

    invalid & !running & !running_prereq & valid_prereq
}
