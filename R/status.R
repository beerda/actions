#'
#' @return
#' @author Michal Burda
#' @export
status <- function(action, type = c('depends', 'targets')) {
    assert_that(is_action(action))
    type <- match.arg(type)

    res <- NULL
    if (type == 'depends') {
        item <- action$depends

        if (!is.null(item)) {
            res <- rep_len('modified', length(item))

            nonexistent <- !file.exists(item)
            res[nonexistent] <- 'non-existent'

            min_target_time <- min(file.mtime(action$targets))
            if (!is.na(min_target_time)) {
                item_time <- file.mtime(item)
                res[!is.na(item_time) & min_target_time >= item_time] <- 'old'
            }
        }

    } else {
        item <- action$targets

        if (!is.null(item)) {
            res <- rep_len('updated', length(item))

            nonexistent <- !file.exists(item)
            res[nonexistent] <- 'non-existent'

            if (!is.null(action$depends)) {
                max_dep_time <- max(file.mtime(action$depends))
                if (!is.na(max_dep_time)) {
                    item_time <- file.mtime(item)
                    res[!is.na(item_time) & max_dep_time >= item_time] <- 'obsolete'
                }
            }
        }
    }

    res
}
