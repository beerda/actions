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
        res <- poll(workers, timeout)

        if (!all(res$alive_workers)) {
            job <- mark_finished(job, res$succeeded_ids, TRUE)
            job <- mark_finished(job, res$failed_ids, FALSE)
            job <- mark_failed_deps(job)

            workers <- workers[res$alive_workers]

            failed_actions <- unlist(props(job, 'failed'))
            finished_actions <- unlist(props(job, 'finished'))
            runnable_actions <- which_runnable(job, res$running_actions | failed_actions) & !finished_actions
        }

    }

    print(props(job, 'status'))
}
