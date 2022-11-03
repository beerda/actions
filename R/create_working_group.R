#'
#' @return
#' @author Michal Burda
#' @export
create_working_group <- function(size) {
    structure(list(),
              class = 'working_group',
              size = size)
}
