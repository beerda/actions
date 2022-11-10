#'
#' @return
#' @author Michal Burda
#' @export
poll <- function(workers, timeout) {
    assert_that(is_working_group(workers))

    processes <- lapply(workers, function(w) w$process)
    processx::poll(processes, timeout)

    alive <- vapply(workers, function(w) w$process$is_alive(), logical(1))

    for (i in seq_along(workers)) {
        show_output(workers[[i]])
    }

    succeeded_ids <- na.omit(
        vapply(workers[!alive],
               function(w) ifelse(w$process$get_result()$ok, w$id, NA),
               numeric(1)))

    failed_ids <- na.omit(
        vapply(workers[!alive],
               function(w) ifelse(w$process$get_result()$ok, NA, w$id),
               numeric(1)))

    running_ids <- vapply(workers[alive],
                          function(w) w$id,
                          numeric(1))

    running_actions <- rep_len(FALSE, length(job))
    running_actions[running_ids] <- TRUE

    workers <- create_working_group(attr(workers, 'size'), workers[alive])

    list(alive_workers = alive,
         succeeded_ids = succeeded_ids,
         failed_ids = failed_ids,
         running_actions = running_actions,
         workers = workers)
}
