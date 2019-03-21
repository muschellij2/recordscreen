#' Start Screen Recording
#'
#' @param device Device to record on
#' @param outfile Output filename (with extension)
#' @param overwrite Should output file be overwritten if it exists?
#' @param audio Should audio be captured
#' @param video Should video be captured
#' @param args additional arguments to pass to `ffmpeg`
#' @param run Should the code be run (default `TRUE`) or simply
#' have the arguments printed (`FALSE`)
#' @param verbose print diagnostic messages
#' @param show_std_err Show `STDERR` output, passed to [sys::exec_background].
#'
#' @return A list of the process ID and output file.
#' @importFrom sys exec_background
#' @note https://trac.ffmpeg.org/wiki/Capture/Desktop
#' @export
#'
#' @examples
#' res = Sys.which("ffmpeg")
#' stopifnot(res != "")
#' print(list_output_devices())
#' start_screen_record(run = FALSE)
#' out = start_screen_record(audio = FALSE, show_std_err = TRUE)
#' Sys.sleep(4)
#' res = end_screen_record(out$pid)
#' stopifnot(file.exists(out$outfile))
#' if (interactive()) {
#'    sys::exec_wait("open", args = out$outfile)
#' }
#'
start_screen_record = function(device = guess_recording_device(),
                               outfile = tempfile(fileext = ".avi"),
                               args = NULL,
                               overwrite = TRUE,
                               audio = FALSE,
                               video = TRUE,
                               run = TRUE,
                               verbose = TRUE,
                               show_std_err = FALSE
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
  # macos_cap = paste0('"', macos_cap, '"')

  linux_cap = if (video) {
    ":0.0"
    } else {
      NULL
    }
  capturer = switch(sys_type(),
                    windows = I(windows_cap),
                    macos = I(macos_cap),
                    linux = linux_cap
  )
  if (sys_type() == "linux") {
    if (audio) {
      args = c(args, "-f", "alsa", "-ac", "2", "-i", "hw:0")
    }
  }
  args = c(outfile,
           "-f", device,
           if (!is.null(capturer)) c("-i", capturer),
           args)

  if (!run) {
    print(paste(args, collapse = " "))
    return(args)
  }
  if (verbose > 1) {
    message("args are")
    print(args)
  }
  pid <- sys::exec_background("ffmpeg",
                              args = args,
                              std_err = show_std_err)
  if (verbose) {
    message("pid is ", pid)
  }
  L = list(pid = pid,
           outfile = outfile,
           args = args)
  return(L)
}



