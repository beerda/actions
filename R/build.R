#'
#' @return
#' @author Michal Burda
#' @export
build <- function(job, processes = 1, timeout = 10000) {
    assert_that(is_job(job))
    assert_that(is.count(processes))

    cat(rule(left = paste0(style_bold('Build started'), ' (', Sys.time(), ')'),
             line = 2,
             col = col_white), '\n')
    print(job)

    job <- init_build(job)
    job <- mark_missing_deps(job)
    job <- mark_failed_deps(job)

    failed_actions <- unlist(props(job, 'failed'))
    finished_actions <- unlist(props(job, 'finished'))
    runnable_actions <- which_runnable(job) & !finished_actions
    workers <- create_working_group(processes)

    start_time <- Sys.time()
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
    end_time <- Sys.time()
    diff_time <- as.numeric(end_time - start_time)
    diff_time <- round(diff_time, 1)

    cat(rule(left = style_bold('Build summary'),
             line = 2,
             col = col_white), '\n')
    print(job)
    cat(rule(left = paste0(style_bold('Build finished'), ' (', diff_time, ' s)'),
             line = 2,
             col = col_white), '\n')
}
