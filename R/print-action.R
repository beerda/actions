#'
#' @return
#' @author Michal Burda
#' @export
print.action <- function(x, ...) {
    assert_that(is_action(x))

    print(job(x))
}
