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
#' @param duration record for a fixed duration, passed to the
#' `args`, in seconds
#' @param show_std_err Show `STDERR` output, passed to [sys::exec_background].
#' @importFrom hms hms
#' @importFrom processx process
#'
#' @return A list of the process ID and output file.
#' @note https://trac.ffmpeg.org/wiki/Capture/Desktop
#' @export
#'
#' @examples
#' res = Sys.which("ffmpeg")
#' stopifnot(res != "")
#' print(list_output_devices())
#' tempdir(check = TRUE)
#' start_screen_record(run = FALSE)
#' out = start_screen_record(audio = FALSE, show_std_err = TRUE)
#' Sys.sleep(2)
#' res = end_screen_record(out$pid)
#' stopifnot(file.exists(out$outfile))
#'
#' out = start_screen_record(audio = FALSE,
#' outfile = tempfile(fileext = ".gif"), show_std_err = TRUE)
#' Sys.sleep(2)
#' res = end_screen_record(out)
#' stopifnot(file.exists(out$outfile))
#'
#'
start_screen_record = function(device = guess_recording_device(),
                               outfile = tempfile(fileext = ".avi"),
                               duration = NULL,
                               args = NULL,
                               overwrite = TRUE,
                               audio = FALSE,
                               video = TRUE,
                               run = TRUE,
                               verbose = FALSE,
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
  xduration = duration
  if (!is.null(duration)) {
    duration = hms::hms(seconds = duration)
    duration = as.character(duration)
  }
  args = c(
    "-y", # overwrite
    if (!is.null(duration)) c("-t", duration),
    outfile,
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
  # pid <- sys::exec_background("ffmpeg",
  #                             args = args,
  #                             std_err = show_std_err)
  pid = processx::process$new(command = "ffmpeg", args = args)
  if (verbose) {
    if (inherits(pid, "process")) {
      message("pid is ", pid$get_pid())
    } else {
      message("pid is ", pid)
    }
  }
  L = list(pid = pid,
           process = pid)
  if (inherits(pid, "process")) {
    L = list(pid = pid$get_pid(),
             process = pid)
  }
  L$outfile = outfile
  L$args = args
  L$duration = xduration
  class(L) = "screen_recording"
  return(L)
}



#' Print method for screen_recording
#'
#' @return NULL
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods
#' @export
#'
#' @examples
#' x = list(process =  processx::process$new("ls"))
#' class(x) = "screen_recording"
#' print(x)
#' @method print screen_recording
print.screen_recording = function(x, ...) {
  print(x$process)
}
