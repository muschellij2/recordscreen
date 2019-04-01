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
  df = list_formats()
  if (!rec_device %in% df$device) {
    df = df$device[ grepl("input", df$description)]
    df = paste(df, collapse = " ")
    warning(paste0("Device ", rec_device),
            " not listed in formats, maybe try ", df)
  }
  rec_device
}
