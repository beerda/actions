#'
#' @return
#' @author Michal Burda
#' @export
show_output <- function(workers) {
    assert_that(is_working_group(workers))

    for (w in workers) {
        text <- w$process$read_output_lines()
        text <- .format_output(w$action$label, 'OUT', text)
        jobAddOutput(w$rstudio_job, text, FALSE)

        text <- w$process$read_error_lines()
        text <- .format_output(w$action$label, 'ERR', text)
        jobAddOutput(w$rstudio_job, text, TRUE)
    }
}


.format_output <- function(label, stream, text) {
    if (length(text) > 0) {
        text <- paste0(label, ' ', stream, ': ', text, collapse = '\n')
        text <- paste0(text, '\n')
    }

    text
}
