#'
#' @return
#' @author Michal Burda
#' @export
is_action <- function(action) {
    is.list(action) &&
        inherits(action, 'action') &&
        !is.null(action$targets)
}
