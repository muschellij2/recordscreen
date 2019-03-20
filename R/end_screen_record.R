#' End Screen Record
#'
#' @param pid
#'
#' @return
#' @export
#'
#' @examples
end_screen_record = function(pid) {
  tools::pskill(pid)
}
