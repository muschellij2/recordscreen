#' Guess the Recording Device for `ffmpeg`
#'
#' @return A character string of a recording device
#' @export
#' @md
#'
#' @examples
#' guess_recording_device()
guess_recording_device = function() {
  rec_device = Sys.getenv("RECORDING_DEVICE")
  if (rec_device == "") {
    rec_device = switch(sys_type(),
         windows = "dshow",
         macos = "avfoundation",
         linux = "x11grab")
  }
  rec_device
}
