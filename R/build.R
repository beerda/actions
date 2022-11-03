#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom callr r_bg
#' @importFrom processx poll
build <- function(job, processes = 1, timeout = 1000) {
    assert_that(is_job(job))
    assert_that(is.count(processes))

    job <- init_build(job)
    job <- mark_missing_deps(job)
    job <- mark_failed_deps(job)

    finished_actions <- unlist(props(job, 'finished'))
    runnable_actions <- which_runnable(job) & !finished_actions
    workers <- list()
    alive_workers <- NULL

    while (any(runnable_actions) || any(alive_workers)) {
        start_ids <- .which_start(job, processes - length(workers), runnable_actions)
        workers <- c(workers, dispatch(job, start_ids))

        .poll(workers, timeout)
        .print(workers, FALSE)
        .print(workers, TRUE)

        alive_workers <- .alive(workers)
        if (!all(alive_workers)) {
            succeeded_ids <- .which_succeeded(workers)
            failed_ids <- .which_failed(workers)
            job <- mark_finished(job, succeeded_ids, TRUE)
            job <- mark_finished(job, failed_ids, FALSE)
            job <- mark_failed_deps(job)

            workers <- workers[alive_workers]

            alive_ids <- .which_alive(workers)
            running_actions <- rep_len(FALSE, length(job))
            running_actions[alive_ids] <- TRUE
            failed_actions <- unlist(props(job, 'failed'))
            finished_actions <- unlist(props(job, 'finished'))

            runnable_actions <- which_runnable(job, running_actions | failed_actions) & !finished_actions
        }
    }

    print(props(job, 'status'))
}


.which_start <- function(job, processes, runnable_actions) {
    running_actions <- runnable_actions & (cumsum(runnable_actions) <= processes)

    seq_along(job)[running_actions]
}


.poll <- function(workers, timeout) {
    processes <- lapply(workers, function(w) w$process)
    poll(processes, timeout)
}


.print <- function(workers, err = FALSE) {
    for (i in seq_along(workers)) {
        if (err) {
            txt <- workers[[i]]$process$read_error_lines()
            stream <- 'ERR'
        } else {
            txt <- workers[[i]]$process$read_output_lines()
            stream <- 'OUT'
        }
        if (length(txt) > 0) {
            txt <- paste0(workers[[i]]$action$label, ' ', stream, ': ', txt, collapse = '\n')
            cat(txt, '\n', sep = '')
        }
    }
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


