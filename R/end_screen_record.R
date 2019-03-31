#' End Screen Record
#'
#' @param pid Process ID from the screen recording
#'
#' @return A logical vector same length as `pid`
#' @export
#' @importFrom tools pskill
end_screen_record = function(pid) {
  # tools::pskill(pid)
  hdl = ps::ps_handle(pid = pid)
  ps::ps_kill(hdl)
  !is_screen_recording(pid)
}

#' @export
#' @rdname end_screen_record
#' @importFrom ps ps_kill ps_handle ps_is_running
is_screen_recording = function(pid) {
  hdl = ps::ps_handle(pid = pid)
  ps::ps_is_running(hdl)
}
