#' End Screen Record
#'
#' @param pid Process ID from the screen recording or a
#' \code{screen_recording}
#'
#' @return A logical vector same length as `pid`
#' @export
end_screen_record = function(pid) {
  if (inherits(pid, "screen_recording")) {
    pid$process$interrupt()
    return(TRUE)
  }
  pid = as.integer(pid)
  tab = ps::ps()
  if (pid %in% tab$pid) {
    hdl = ps::ps_handle(pid = pid)
    ps::ps_interrupt(hdl)
  }
  !is_screen_recording(pid)
}

#' @export
#' @rdname end_screen_record
#' @importFrom ps ps_interrupt ps_handle ps_is_running
is_screen_recording = function(pid) {
  if (inherits(pid, "screen_recording")) {
    return(pid$process$is_alive())
  }
  pid = as.integer(pid)
  tab = ps::ps()
  if (pid %in% tab$pid) {
    hdl = ps::ps_handle(pid = pid)
    ps::ps_is_running(hdl)
  } else {
    FALSE
  }
}
