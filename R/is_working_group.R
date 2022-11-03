#'
#' @return
#' @author Michal Burda
#' @export
is_working_group <- function(x) {
    is.list(x) &&
        all(sapply(x, is_worker))
}
