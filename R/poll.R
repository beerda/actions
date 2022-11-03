#'
#' @return
#' @author Michal Burda
#' @export
poll <- function(workers, timeout) {
    assert_that(is_working_group(workers))

    processes <- lapply(workers, function(w) w$process)
    processx::poll(processes, timeout)

    for (w in workers) {
        text <- w$process$read_output_lines()
        text <- .format_output(w$action$label, 'OUT', text)
        jobAddOutput(w$rstudio_job, text, FALSE)

        text <- w$process$read_error_lines()
        text <- .format_output(w$action$label, 'ERR', text)
        jobAddOutput(w$rstudio_job, text, TRUE)
    }

    alive <- vapply(workers, function(w) w$process$is_alive(), logical(1))

    succeeded_ids <- na.omit(
        vapply(workers[!alive],
               function(w) ifelse(w$process$get_result(), w$id, NA),
               numeric(1)))

    failed_ids <- na.omit(
        vapply(workers[!alive],
               function(w) ifelse(w$process$get_result(), NA, w$id),
               numeric(1)))

    running_ids <- vapply(workers[alive],
                          function(w) w$id,
                          numeric(1))

    running_actions <- rep_len(FALSE, length(job))
    running_actions[running_ids] <- TRUE

    list(alive_workers = alive,
         succeeded_ids = succeeded_ids,
         failed_ids = failed_ids,
         running_actions = running_actions)
}


.format_output <- function(label, stream, text) {
    if (length(text) > 0) {
        text <- paste0(label, ' ', stream, ': ', text, collapse = '\n')
        text <- paste0(text, '\n')
    }

    text
}
