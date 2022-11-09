#'
#' @return
#' @author Michal Burda
#' @export
props <- function(job, name) {
    assert_that(is_job(job))
    assert_that(is.string(name))

    lapply(job, function(a) a[[name]])
}


#' @export
`props<-` <- function(job, name, value) {
    assert_that(is_job(job))
    assert_that(is.string(name))

    value <- rep_len(value, length(job))

    res <- lapply(seq_along(job), function(i) {
        a <- job[[i]]
        a[[name]] <- value[[i]]

        a
    })
    attr(res, 'class') <- 'job'
    attr(res, 'init_build') <- attr(job, 'init_build')

    res
}
