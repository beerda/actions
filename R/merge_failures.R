#'
#' @return
#' @author Michal Burda
#' @export
merge_failures <- function(...) {
    dots <- list(...)
    assert_that(all(sapply(dots, is.list)))

    do.call(mapply, c(list(c), dots))
}
