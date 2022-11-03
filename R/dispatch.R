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

        w <- list(id = id,
                  action = action,
                  process = process,
                  rstudio_job = jobAdd(name = action$label,
                                       status = 'running',
                                       actions = list(stop = .stop_button(process))))

        structure(w, class = 'worker')
    })
}


.stop_button <- function(process) {
    function(id) {
        process$interrupt()
        jobSetState(id, 'cancelled')
    }
}
