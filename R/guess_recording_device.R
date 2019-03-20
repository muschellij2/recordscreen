#' Guess the Recording Device for `ffmpeg`
#'
#' @return
#' @export
#' @md
#'
#' @examples
guess_recording_device = function() {
  switch(sys_type(),
         windows = "dshow",
         macos = "avfoundation",
         linux = "x11grab")
}
