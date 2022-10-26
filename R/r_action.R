#'
#' @return
#' @author Michal Burda
#' @export
r_action <- function(targets,
                     script,
                     depends = NULL,
                     args = NULL) {

    action(targets = targets,
           depends = c(depends, script),
           fun = source,
           args = list(script))
}
