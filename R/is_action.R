#'
#' @return
#' @author Michal Burda
#' @export
is_action <- function(action) {
    return(is.list(action) &&
               inherits(action, 'action') &&
               !is.null(action$targets))
}
