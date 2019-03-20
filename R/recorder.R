# https://trac.ffmpeg.org/wiki/Capture/Desktop
#' Title
#'
#' @param device
#' @param outfile
#' @param args
#' @param overwrite
#' @param audio
#' @param video
#' @param run
#'
#' @return
#' @export
#' @md
#'
#' @examples
start_screen_record = function(device = guess_recording_device(),
                               outfile = tempfile(fileext = ".avi"),
                               args = NULL,
                               overwrite = TRUE,
                               audio = TRUE,
                               video = TRUE,
                               run = TRUE
) {
  if (file.exists(outfile)){
    if (!overwrite) {
      stop("outfile exists and overwrite = FALSE")
    }
    file.remove(outfile)
  }
  if (!audio & !video) {
    stop("Neither audio or video being recorded, exiting.")
  }

  windows_cap = paste0(ifelse(video, 'video="UScreenCapture"', ""),
                       ifelse(audio & video, ":", ""),
                       ifelse(audio, 'audio="Microphone"', ""))

  macos_cap = paste0(ifelse(video, "1", ""),
                     ifelse(audio & video, ":", ""),
                     ifelse(audio, "1", ""))
  macos_cap = paste0('"', macos_cap, '"')

  linux_cap = paste0(video, ":0.0", "")
  capturer = switch(sys_type(),
                     windows = I(windows_cap),
                     macos = I(macos_cap),
                     linux = "x11grab"
  )
  if (sys_type() == "linux") {
    args = c(args, "-f", "alsa", "-ac", "2", "-i", "hw:0")
  }
  args = c("-f", device,
           "-i", capturer,
           args,
           outfile)

  if (!run) {
    print(paste(args, collapse = " "))
  }
  return(args)
    # pid <- exec_background("ffmpeg", args = args,
  #                        std_err = FALSE)
  # pid
}



