#'
#' @return
#' @author Michal Burda
#' @export
is_worker <- function(worker) {
    is_list(worker) &&
        inherits(worker, 'worker')
}
