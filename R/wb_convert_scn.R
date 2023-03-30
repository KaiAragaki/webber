#' Convert a .scn file to another format
#'
#' The BioRad ChemiDoc produces .scn files that can be converted to .tif(f)
#' files using bfconvert.
#'
#' This function requires [bftools](https://bio-formats.readthedocs.io/en/stable/users/comlinetools/index.html) to be installed
#'
#' @param file Character. Path to the file to be converted.
#' @param dest_name Character. File name and path to save to. If not supplied,
#'   will be same name and location as input, but with a .tif extension.
#' @param overwrite Logical. Should the file be overwritten if it already
#'   exists?
#'
#' @return Path to the saved file
#' @export
#'
#' @examples
#' \dontrun{
#' wb_convert_scn("path/to/file.scn", dest_name = "~/new/path.tif")
#' }
#'
wb_convert_scn <- function(file, dest_name = NA, overwrite = FALSE) {
  check_if_bfconvert_exists()

  if (is.na(dest_name)) {
    dest_name <- paste0(fs::path_ext_remove(file), ".tif")
  }

  cmd <- "bfconvert '{file}' '{dest_name}'"

  if (overwrite) {
    cmd <- paste(cmd,  "-overwrite")
  }

  system(glue::glue(cmd))

  dest_name
}
