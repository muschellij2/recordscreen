
#' List `ffmpeg` Input Devices
#'
#' @param device Input `ffmpeg` device, usually from [list_output_devices]
#' @param args arguments to pass to [sys::exec_internal] to
#' `ffmpeg`
#'
#' @return A character vector of information
#' @export
#'
#' @examples
#' list_input_devices()
list_input_devices = function(
  device = guess_recording_device(),
  args = c("-hide_banner", "-f", device, "-list_devices", "true")
) {

  if (device %in% "dshow") {
    args = c(args, "-i", "dummy")
  }
  if (device %in% "avfoundation") {
    args = c(args, "-i", '""')
  }
  pid <- sys::exec_internal("ffmpeg",
                            args = args, error = FALSE)
  res = strsplit(rawToChar(pid$stderr), split = "\n")[[1]]
  res
}
