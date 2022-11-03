#'
#' @return
#' @author Michal Burda
#' @export
is_job <- function(job, initialized = FALSE) {
    init <- attr(job, 'init_build')
    return(is.list(job) &&
               all(sapply(job, is_action)) &&
               (!initialized || (!is.null(init) && init)))
}
