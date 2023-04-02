#' Upload a Western Blot
#'
#' This is a thin wrapper around the `upload_file` function from
#' `Microsoft365R`, it just has defaults and safeguards tailored to uploading
#' Western Blots. You could theoretically upload anything this way. It WILL
#' overwrite without prompting.
#'
#' The uploaded file location is produced via `fs::path(wb_edit_path, user,
#' dest_path)`
#'
#' @param path Character. Path to the local file.
#' @param dest_path Character. Path to upload the file. See details.
#' @param wb_edit_path Character. Path where Western blot edits are kept.
#' @param user Character. User directory name.
#' @param drive An optional `ms_drive` object. If none supplied, will make one
#'   automatically.
#'
#' @return Character. Path to the file on Sharepoint.
#' @export
#'
#' @examples
#' \dontrun{
#' wb <- wb_get(
#' "2022-06-22/h3.tif",
#' tempdir(),
#' user = "aragaki-kai",
#' wb_path = "Raw Data/licor"
#' )
#'
#' wb |>
#' }
wb_put <- function(path,
                   dest_path,
                   wb_edit_path = Sys.getenv("WEBBER_WBEDITPATH"),
                   user = Sys.getenv("WEBBER_USR"),
                   drive = NULL) {
  check_if_wb_edit_path_is_empty(wb_edit_path)
  check_if_user_is_empty(user)
  check_if_wb_path_is_wb_edit_path(wb_edit_path = wb_edit_path)

  if (is.null(drive)) {
    drive <- bladdr::get_gbci_drive_connection()
  }

  dest_path <- fs::path(wb_edit_path, user, dest_path)

  drive$upload_file(path, dest_path)

  dest_path
}
