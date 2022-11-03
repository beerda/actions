#'
#' @return
#' @author Michal Burda
#' @export
poll <- function(workers, timeout) {
    assert_that(is_working_group(workers))

    processes <- lapply(workers, function(w) w$process)

    processx::poll(processes, timeout)
}
