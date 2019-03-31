
# rstudio_context <- function() {
#   rstudioapi::getSourceEditorContext()
# }
#
# rstudio_selection <- function(context = rstudio_context()) {
#   text <- rstudioapi::primary_selection(context)[["text"]]
#   rstudio_text_tidy(text)
# }


#' Record Script Selection
#'
#' @return A filename of the output video
#' @param output_ext Extension of the output filename
#' @importFrom rstudioapi getSourceEditorContext sendToConsole
#' @importFrom clipr dr_clipr clipr_available write_clip
#' @export
record_script_selection = function(output_ext = getOption("record.ext", "gif")) {
  res = rstudioapi::getSourceEditorContext()
  # run whole document
  # contents = res$contents
  # evaluate::evaluate(input = contents)
  texts = lapply(res$selection, function(x) x$text)
  unlist(texts)
  tfile = tempfile(fileext = paste0(".", output_ext))
  out = start_screen_record(outfile = tfile, audio = FALSE,
                            verbose = FALSE)
  out$output_ext = output_ext
  Sys.sleep(4)
  sent = lapply(texts, function(code) {
    # eval = evaluate::evaluate()
    rstudioapi::sendToConsole(code)
    if (length(texts) > 1) {
      Sys.sleep(5)
    }
  })
  Sys.sleep(4)
  result = end_screen_record(out$pid)
  check_output(out)
  return(out$outfile)
}

#' Record Script Selection
#'
#' @return A filename of the output video
#' @param output_ext Extension of the output filename
#' @importFrom rstudioapi getSourceEditorContext sendToConsole
#' @importFrom clipr dr_clipr clipr_available write_clip
#' @export
record_script_walk = function(output_ext = getOption("record.ext", "gif")) {
  res = rstudioapi::getSourceEditorContext()
  L = rstudioapi::executeCommand("consoleClear")
  tfile = tempfile(fileext = paste0(".", output_ext))
  out = start_screen_record(outfile = tfile, audio = FALSE,
                            verbose = FALSE)
  on.exit({
    end_screen_record(out$pid)
  })
  out$output_ext = output_ext
  Sys.sleep(4)
  rstudioapi::setCursorPosition(c(0, 0), id = res$id)
  res = rstudioapi::getSourceEditorContext()
  # while (!end_of_document(res)) {
    res = rstudioapi::getSourceEditorContext()
    rstudioapi::executeCommand("executeCode")
    # rstudioapi::executeCommand("executeCurrentParagraph")
    Sys.sleep(4)
  # }
  # Sys.sleep(4)

  result = end_screen_record(out$pid)
  check_output(out)
  return(out$outfile)
}

end_of_document = function(res) {
  res = rstudioapi::getSourceEditorContext()
  doc_length = length(res$contents)
  ranges = lapply(res$selection, function(x) x$range)
  rows = sapply(ranges, function(x) max(x$start["row"], x$end["row"]))
  max(rows) >= (doc_length - 1)
}


check_output = function(out) {
  if (clipr::clipr_available()) {
    clipr::write_clip(out$outfile)
    # message(paste("file is located at", out$outfile))
    # message("Video file is copied to the clipboard.")
  } else if (interactive()) {
    clipr::dr_clipr()
    message(
      "Unable to put result on the clipboard. How to get it:\n",
      "Path to `outfile`:\n",
      "  * ", out$outfile
    )
  }

  stopifnot(file.exists(out$outfile))

  if (out$output_ext %in% "gif") {
    img = try({
      magick::image_read(out$outfile)
    }, silent = TRUE)
    if (!inherits(img, "try-error")) {
      print(img)
    }
  }
  return(invisible(NULL))
}
