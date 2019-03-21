#' `ffmpeg` Output Devices
#'
#' @return A `data.frame` of device names and descriptions
#' @param args arguments to pass to [sys::exec_internal] to
#' `ffmpeg`
#' @export
#' @importFrom sys exec_internal
#'
#' @examples
#' print(list_output_devices())
#'
list_output_devices = function(
  args = c("-devices")
) {

  pid <- sys::exec_internal("ffmpeg",
                              args = args)
  devs = rawToChar(pid$stdout)
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
  devs
}

