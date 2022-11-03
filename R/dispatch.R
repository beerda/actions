#'
#' @return
#' @author Michal Burda
#' @export
dispatch <- function(workers, job, what) {
    assert_that(is_working_group(workers))
    assert_that(is_job(job, TRUE))
    assert_that(is.logical(what))
    assert_that(length(job) == length(what))

    f <- function(action) {
        library(actions)
        run(action)
    }

    size <- attr(workers, 'size')
    ids <- what & (cumsum(what) <= size - length(workers))
    ids <- seq_along(job)[ids]

    fresh <- lapply(ids, function(id) {
        action <- job[[id]]

        process <- r_bg(f,
                        args = list(action = action),
                        error = 'error')

        w <- list(id = id,
                  action = action,
                  process = process,
                  rstudio_job = jobAdd(name = action$label,
                                       status = 'running',
                                       actions = list(stop = .stop_button(process)),
                                       running = TRUE,
                                       autoRemove = TRUE,
                                       show = TRUE))

        structure(w, class = 'worker')
    })

    structure(c(workers, fresh),
              class = 'working_group',
              size = size)
}


.stop_button <- function(process) {
    function(id) {
        jobSetState(id, 'cancelled')
        process$interrupt()
    }
}
