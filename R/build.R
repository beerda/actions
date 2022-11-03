#'
#' @return
#' @author Michal Burda
#' @export
build <- function(job, processes = 1, timeout = 100) {
    assert_that(is_job(job))
    assert_that(is.count(processes))

    job <- init_build(job)
    job <- mark_missing_deps(job)
    job <- mark_failed_deps(job)

    finished_actions <- unlist(props(job, 'finished'))
    runnable_actions <- which_runnable(job) & !finished_actions
    workers <- create_working_group(processes)
    alive_workers <- NULL

    while (any(runnable_actions) || any(alive_workers)) {
        workers <- dispatch(workers, job, runnable_actions)
        poll(workers, timeout)
        show_output(workers)

        alive_workers <- .alive(workers)

        if (!all(alive_workers)) {
            succeeded_ids <- .which_succeeded(workers)
            failed_ids <- .which_failed(workers)
            job <- mark_finished(job, succeeded_ids, TRUE)
            job <- mark_finished(job, failed_ids, FALSE)
            job <- mark_failed_deps(job)

            workers <- workers[alive_workers]

            failed_actions <- unlist(props(job, 'failed'))
            finished_actions <- unlist(props(job, 'finished'))

            alive_ids <- .which_alive(workers)
            running_actions <- rep_len(FALSE, length(job))
            running_actions[alive_ids] <- TRUE
            runnable_actions <- which_runnable(job, running_actions | failed_actions) & !finished_actions
        }

    }

    print(props(job, 'status'))
}


.alive <- function(workers) {
    vapply(workers, function(w) w$process$is_alive(), logical(1))
}


.which_alive <- function(workers) {
    alive <- .alive(workers)
    vapply(workers[alive], function(w) w$id, numeric(1))
}


.which_failed <- function(workers) {
    alive <- .alive(workers)
    res <- vapply(workers[!alive],
                  function(w) ifelse(w$process$get_result(), NA, w$id),
                  numeric(1))

    na.omit(res)
}


.which_succeeded <- function(workers) {
    alive <- .alive(workers)
    res <- vapply(workers[!alive],
                  function(w) ifelse(w$process$get_result(), w$id, NA),
                  numeric(1))

    na.omit(res)
}


