#'
#' @return
#' @author Michal Burda
#' @export
is_worker <- function(worker) {
    is.list(worker) &&
        inherits(worker, 'worker')
}
