#'
#' @return
#' @author Michal Burda
#' @export
show_output <- function(worker) {
    assert_that(is_worker(worker))

    text <- try(worker$process$read_output(), silent = TRUE)
    if (!inherits(text, 'try-error')) {
        if (length(text) > 0) {
            text <- paste0(text, '\n', collapse = '')
            cat(text)
            #-- rstudio binding:   jobAddOutput(w$rstudio_job, text, FALSE)
        }
    }
}
