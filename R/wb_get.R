#' Download a Western blot by path
#'
#' Downloads a Western blot image and returns its path. If it already exists in
#' the specified location, no downloading will occur unless `overwrite = TRUE`.
#' Will return the local path to the file.
#'
#' This is a thin wrapper around `bladdr::get_gbci_file`, and has hard-coded
#' paths - so it's more a quality-of-life function.
#'
#' @param path Character. The path to the image/directory, sans `wb_path`.
#' @param dest_dir Character. The path to save the image/directory. If it
#'   doesn't have an extension, it's assumed you mean a folder, and that folder
#'   already exists.
#' @param overwrite Logical. Should files be overwritten if they already exist?
#' @param user Character. Whose folder this file should be pulled from.
#' @param wb_path Character. Path to the top level directory that contains all
#'   western blots.
#' @param quiet Logical. Should this function not message upon finding existing
#'   files?
#'
#' @return Character. The local path to the file or directory.
#' @export
#'
#' @examples
#' \dontrun{
#' wb_get(
#'   "2022-06-22/h3.tif",
#'   tempdir(),
#'   user = "aragaki-kai",
#'   wb_path = "Raw Data/licor"
#' )
#' }
wb_get <- function(path,
                   dest_dir,
                   overwrite = FALSE,
                   user = Sys.getenv("WEBBER_USR"),
                   wb_path = Sys.getenv("WEBBER_WBPATH"),
                   quiet = FALSE) {
  check_if_user_is_empty(user)
  check_if_wb_path_is_empty(wb_path)

  if (fs::path_ext(dest_dir) == "") {
    if (fs::path_ext(path) != "") {
      dest_path <- fs::path(dest_dir, fs::path_file(path))
    } else {
      dest_path <- dest_dir
    }
  } else {
    dest_path <- fs::path(dest_dir, path)
  }

  if (file.exists(dest_path) & !overwrite) {
    if (!quiet) {
      cli::cli_inform("{dest_path} exists and overwrite = FALSE, returning local path to file")
    }
    return(dest_path)
  }

  no_file <- stringr::str_remove(dest_path, "[^/]*$")
  fs::dir_create(no_file)

  e <- try(
    bladdr::get_gbci(
      fs::path(wb_path, user, path),
      dest = dest_path,
      overwrite = overwrite),
    silent = TRUE
  )

  e_class <- attr(e, "condition") |> attr("class")

  if (length(e_class) > 0) {
    if (e_class[1] == "http_404") {
      cli::cli_abort(c(e[1], i = "`wb_list(path)` can help find available blots within a given directory"))
    }
    cli::cli_abort(e)
  }
  dest_path
}

