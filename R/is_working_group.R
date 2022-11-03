#'
#' @return
#' @author Michal Burda
#' @export
is_working_group <- function(x) {
    is.list(x) &&
        inherits(x, 'working_group') &&
        all(sapply(x, is_worker))
}
