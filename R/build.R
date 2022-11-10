#'
#' @return
#' @author Michal Burda
#' @export
build <- function(job, processes = 1, timeout = 10000) {
    assert_that(is_job(job))
    assert_that(is.count(processes))

    job <- init_build(job)
    job <- mark_missing_deps(job)
    job <- mark_failed_deps(job)

    failed_actions <- unlist(props(job, 'failed'))
    finished_actions <- unlist(props(job, 'finished'))
    runnable_actions <- which_runnable(job) & !finished_actions
    workers <- create_working_group(processes)

    while (any(runnable_actions) || length(workers) > 0) {
        workers <- dispatch(workers, job, runnable_actions)
        res <- poll(workers, timeout)

        workers <- res$workers

        job <- mark_finished(job, res$succeeded_ids, TRUE)
        job <- mark_finished(job, res$failed_ids, FALSE)
        job <- mark_failed_deps(job)

        failed_actions <- unlist(props(job, 'failed'))
        finished_actions <- unlist(props(job, 'finished'))
        runnable_actions <- which_runnable(job, res$running_actions | failed_actions)
        runnable_actions <- runnable_actions & !finished_actions & !res$running_actions
    }

    print(props(job, 'status'))
}
