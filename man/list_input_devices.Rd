% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_input_devices.R
\name{list_input_devices}
\alias{list_input_devices}
\alias{list_formats}
\title{List \code{ffmpeg} Input Devices}
\usage{
list_input_devices(device = guess_recording_device(),
  args = c("-hide_banner", "-f", device, "-list_devices", "true"))

list_formats()
}
\arguments{
\item{device}{Input \code{ffmpeg} device, usually from \link{list_output_devices}}

\item{args}{arguments to pass to \link[sys:exec_internal]{sys::exec_internal} to
\code{ffmpeg}}
}
\value{
A character vector of information
}
\description{
List \code{ffmpeg} Input Devices
}
\examples{
list_input_devices()
list_formats()
}
