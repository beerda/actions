#'
#' @return
#' @author Michal Burda
#' @export
nonupdated_targets <- function(action, pre_time) {
    assert_that(is_action(action))

    post_time <- file.mtime(action$targets)
    updated <- (is.na(pre_time) & !is.na(post_time)) |
        (!is.na(pre_time) & !is.na(post_time) & pre_time < post_time)

    res <- NULL
    if (!all(updated)) {
        res <- action$targets[!updated]
    }

    res
}
