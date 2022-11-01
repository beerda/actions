#'
#' @return
#' @author Michal Burda
#' @export
action <- function(targets,
                   depends = NULL,
                   fun = function() { },
                   args = NULL,
                   label = paste(targets, collapse = ', ')) {
    assert_that(is.character(targets))
    assert_that(is.null(depends) || is.character(depends))
    assert_that(is.null(args) || is.list(args))

    structure(list(targets = targets,
                   depends = depends,
                   fun = fun,
                   args = args,
                   label = label),
              class = 'action')
}
