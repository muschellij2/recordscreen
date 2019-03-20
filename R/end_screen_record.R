#' End Screen Record
#'
#' @param pid Process ID from the screen recording
#'
#' @return A logical vector same length as `pid`
#' @export
#' @importFrom tools pskill
end_screen_record = function(pid) {
  tools::pskill(pid)
}
