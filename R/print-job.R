#'
#' @return
#' @author Michal Burda
#' @export
print.job <- function(job, ...) {
    assert_that(is_job(job, FALSE))


    no <- as.character(seq_along(job))
    deps <- .format(job, 'depends', function(d) d[-length(d)])
    script <- .format(job, 'depends', function(d) d[length(d)])
    targets <- .format(job, 'targets', identity)

    cols <- prepare_table(list(no, deps, script, targets),
                          min_size = 12,
                          total_size = console_width() - 3 * 3 - 2)

    init <- attr(job, 'init_build')
    if (is.null(init))
        init <- FALSE

    if (init) {
        failed <- unlist(props(job, 'failed'))
        finished <- unlist(props(job, 'finished'))
        icon <- ifelse(finished,
                       ifelse(failed, '✖ ', '✔ '),
                       '  ')
        for (i in seq_along(job)) {
            color <- if (finished[i]) if (failed[i]) col_br_red else col_br_green else col_white
            cat(color(icon[i]))
            cat(col_br_white('[', cols[[1]][i], '] '))
            cat(col_br_white(cols[[2]][i]))
            cat(col_br_white(' ▶ '))
            cat(color(cols[[3]][i]))
            cat(col_br_white(' ▶ '))
            cat(col_br_white(cols[[4]][i], '\n'))
        }

    } else {
        obsolete <- Vectorize(is_obsolete)(job)
        icon <- ifelse(obsolete, '• ', '  ')
        for (i in seq_along(job)) {
            color <- if (obsolete[i]) col_br_green else col_white

            cat(color(icon[i]))
            cat(col_br_white('[', cols[[1]][i], '] '))
            cat(col_br_white(cols[[2]][i]))
            cat(col_br_white(' ▶ '))
            cat(color(cols[[3]][i]))
            cat(col_br_white(' ▶ '))
            cat(col_br_white(cols[[4]][i], '\n'))
        }
    }
}


.format <- function(job, prop, fun) {
    prop <- props(job, prop)

    vapply(prop,
           function(d) paste0(fun(d), collapse = '|'),
           character(1))
}
