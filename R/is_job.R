#'
#' @return
#' @author Michal Burda
#' @export
is_job <- function(job) {
    return(is.list(job) &&
               all(sapply(job, is_action)))
}
