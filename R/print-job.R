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

    sizes <- list(no, deps, script, targets)
    sizes <- lapply(sizes, nchar)
    sizes <- unlist(lapply(sizes, max))
    widths <- col_widths(sizes, 3, 12, console_width())

    deps <- .pad(deps, widths[2])
    script <- .pad(script, widths[3])
    targets <- .pad(targets, widths[4])

    cat(paste0('[', no, '] ', deps, ' ▶ ', script, ' ▶ ', targets, '\n',
               sep = '', collapse = ''))
}


.format <- function(job, prop, fun) {
    prop <- props(job, prop)

    vapply(prop,
           function(d) paste0(fun(d), collapse = '|'),
           character(1))
}


.pad <- function(what, limit) {
    res <- str_trunc(what, limit, ellipsis = '…')

    str_pad(res, limit)
}
