#'
#' @return
#' @author Michal Burda
#' @export
create_working_group <- function(size, existing_workers = list()) {
    assert_that(is.list(existing_workers))
    assert_that(all(sapply(existing_workers, is_worker)))

    structure(existing_workers,
              class = 'working_group',
              size = size)
}
