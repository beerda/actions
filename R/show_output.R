#'
#' @return
#' @author Michal Burda
#' @export
show_output <- function(worker) {
    assert_that(is_worker(worker))

    text <- worker$process$read_output_lines()
    if (length(text) > 0) {
        text <- paste0(text, '\n', collapse = '')
        cat(text)
        #-- rstudio binding:   jobAddOutput(w$rstudio_job, text, FALSE)
    }
}
