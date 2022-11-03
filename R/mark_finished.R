#'
#' @return
#' @author Michal Burda
#' @export
mark_finished <- function(job, ids, success) {
    assert_that(is_job(job, TRUE))
    assert_that(is.numeric(ids))
    assert_that(is.flag(success))

    props(job, 'status')[ids] <- ifelse(success, 'Built successfully', 'Execution failed')
    props(job, 'succeeded')[ids] <- success
    props(job, 'failed')[ids] <- !success
    props(job, 'finished')[ids] <- TRUE

    job
}
