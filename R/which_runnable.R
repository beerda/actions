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

    running_prereq <- matrix(running, nrow = 1)
    while (TRUE) {
        old <- running_prereq
        running_prereq <- (running_prereq + running_prereq %*% prereq) > 0
        if (all(old == running_prereq))
            break
    }
    running_prereq <- as.vector(running_prereq)

    valid_prereq <- (invalid %*% prereq) == 0
    valid_prereq <- as.vector(valid_prereq)

    invalid & !running & !running_prereq & valid_prereq
}
