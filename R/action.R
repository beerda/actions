#'
#' @return
#' @author Michal Burda
#' @export
action <- function(targets,
                   depends = NULL,
                   params = NULL) {
    assert_that(is.character(targets))
    assert_that(is.null(depends) || is.character(depends))
    assert_that(is.null(params) || is.list(params))

    structure(list(targets = targets,
                   depends = depends,
                   params = params),
              class = 'action')
}
