#'
#' @return
#' @author Michal Burda
#' @export
failures_of_failed_prerequisites <- function(job, failed_actions) {
    deps <- dependants(job, failed_actions)
    deps <- deps & !failed_actions
    res <- rep_len(list(NULL), length(job))
    res[deps] <- 'Failed prerequisite action'

    res
}
