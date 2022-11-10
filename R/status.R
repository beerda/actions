#'
#' @return
#' @author Michal Burda
#' @export
status <- function(action,
                   type = c('depends', 'targets'),
                   newer_text = 'newer',
                   nonexistent_text = 'non-existent',
                   older_text = 'older') {
    assert_that(is_action(action))
    assert_that(is.string(type))
    assert_that(is.string(newer_text))
    assert_that(is.string(nonexistent_text))
    assert_that(is.string(older_text))

    type <- match.arg(type)
    res <- NULL
    if (type == 'depends') {
        item <- action$depends

        if (!is.null(item)) {
            res <- rep_len(newer_text, length(item))

            nonexistent <- !file.exists(item)
            res[nonexistent] <- nonexistent_text

            min_target_time <- min(file.mtime(action$targets))
            if (!is.na(min_target_time)) {
                item_time <- file.mtime(item)
                res[!is.na(item_time) & min_target_time >= item_time] <- older_text
            }
        }

    } else {
        item <- action$targets

        if (!is.null(item)) {
            res <- rep_len(newer_text, length(item))

            nonexistent <- !file.exists(item)
            res[nonexistent] <- nonexistent_text

            if (!is.null(action$depends)) {
                max_dep_time <- max(file.mtime(action$depends))
                if (!is.na(max_dep_time)) {
                    item_time <- file.mtime(item)
                    res[!is.na(item_time) & max_dep_time >= item_time] <- older_text
                }
            }
        }
    }

    res
}
