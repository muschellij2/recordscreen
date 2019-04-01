
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
#' list_formats()
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
  # pid <- sys::exec_internal("ffmpeg",
  #                           args = args, error = FALSE)
  # res = strsplit(rawToChar(pid$stderr), split = "\n")[[1]]
  res = processx::process$new(command = "ffmpeg", args = args, stderr = "|")
  string = res$read_all_error()
  string = string[ grepl("device", string)]
  string
}

#' @export
#' @rdname list_input_devices
list_formats = function() {
  args = "-formats"
  res = processx::process$new(command = "ffmpeg",
                              args = args,
                              stderr = "|",
                              stdout = "|")

  devs = paste(res$read_output_lines(), collapse = "\n")
  devs = strsplit(devs, "\n")[[1]]
  devs = trimws(devs)
  breaker = grep("--", devs)
  if (length(breaker) == 1) {
    devs = devs[(breaker + 1):length(devs)]
  }
  devs = gsub("\\s+", " ", devs)
  devs = strsplit(devs, " ")
  devs = sapply(devs, function(x) {
    c(x[1:2], paste(x[3:length(x)], collapse = " "))
  })
  devs = t(devs)
  colnames(devs) = c("capabilities", "device", "description")
  devs = as.data.frame(devs, stringsAsFactors = FALSE)
  return(devs)
}
