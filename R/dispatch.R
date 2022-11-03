#'
#' @return
#' @author Michal Burda
#' @export
dispatch <- function(job, ids) {
    assert_that(is_job(job, TRUE))
    assert_that(is.numeric(ids))

    f <- function(action) {
        library(actions)
        run(action)
    }

    lapply(ids, function(id) {
        action <- job[[id]]

        process <- r_bg(f,
                        args = list(action = action),
                        error = 'error')

        worker <- list(id = id,
                       action = action,
                       process = process)

        worker$rstudio_job <- jobAdd(name = action$label,
                                     status = 'running',
                                     actions = list(stop = .stop_button(process)))

        worker
    })
}


.stop_button <- function(process) {
    function(id) {
        process$interrupt()
        jobSetState(id, 'cancelled')
    }
}
