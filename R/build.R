#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom callr r_bg
#' @importFrom processx poll
build <- function(job, processes = 1, timeout = 1000) {
    assert_that(is_job(job))
    assert_that(is.count(processes))

    action_failures <- failures_of_missing_actions(job)
    failed_actions <- !sapply(action_failures, is.null)

    prereq_failures <- failures_of_failed_prerequisites(job, failed_actions)
    action_failures <- merge_failures(action_failures, prereq_failures)
    failed_actions <- !sapply(action_failures, is.null)

    finished_actions <- rep(FALSE, length(job))
    finished_actions[failed_actions] <- TRUE

    runnable_actions <- which_runnable(job) & !finished_actions
    workers <- list()
    alive_workers <- NULL

    while (any(runnable_actions) || any(alive_workers)) {
        start_ids <- .which_start(job, processes - length(workers), runnable_actions)
        workers <- c(workers, .dispatch(job, start_ids))

        .poll(workers, timeout)
        .print(workers, FALSE)
        .print(workers, TRUE)

        alive_workers <- .alive(workers)
        if (!all(alive_workers)) {
            failed_ids <- .which_failed(workers)
            action_failures[failed_ids] <- 'Execution failed'
            failed_actions <- !sapply(action_failures, is.null)
            prereq_failures <- failures_of_failed_prerequisites(job, failed_actions)
            action_failures <- merge_failures(action_failures, prereq_failures)
            failed_actions <- !sapply(action_failures, is.null)

            finished_ids <- .which_finished(workers)
            finished_actions[finished_ids] <- TRUE

            workers <- workers[alive_workers]

            alive_ids <- .which_alive(workers)
            running_actions <- rep_len(FALSE, length(job))
            running_actions[alive_ids] <- TRUE

            runnable_actions <- which_runnable(job, running_actions | failed_actions ) & !finished_actions
        }
    }

    action_failures[finished_actions & !failed_actions] <- 'Built successfully'

    print(action_failures)
}


.which_start <- function(job, processes, runnable_actions) {
    running_actions <- runnable_actions & (cumsum(runnable_actions) <= processes)

    seq_along(job)[running_actions]
}


.dispatch <- function(job, start_ids) {
    f <- function(action) {
        library(actions)
        run(action)
    }

    lapply(start_ids, function(id) {
        action <- job[[id]]
        list(id = id,
             action = action,
             process = r_bg(f, args = list(action = action), error = 'error'))
    })
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


.which_finished <- function(workers) {
    alive <- .alive(workers)
    vapply(workers[!alive], function(w) w$id, numeric(1))
}


.which_failed <- function(workers) {
    alive <- .alive(workers)
    res <- vapply(workers[!alive],
                  function(w) ifelse(w$process$get_result(), NA, w$id),
                  numeric(1))

    na.omit(res)
}


