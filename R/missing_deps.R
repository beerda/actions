#'
#' @return
#' @author Michal Burda
#' @export
missing_deps <- function(job) {
    assert_that(is_job(job))

    targ <- targets(job)

    lapply(job, function(action) {
        if (is.null(action$depends)) {
            return(character(0))
        }
        existing <- file.exists(action$depends) | (action$depends %in% targ)
        action$depends[!existing]
    })
}
