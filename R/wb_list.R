#' List files in a directory
#'
#' @param path Character. The path to the image, sans `wb_path`
#' @param recursive Logical. Should folders of folders be recursively indexed into?
#' @param user Whose folder should this file be pulled from?
#' @param wb_path Character. Path to the top level directory that contains all western blots.
#'
#' @return a `data.frame` with the path to the item on SharePoint
#' @export
#'
#' @examples
#' \dontrun{
#' wb_list(
#'   "2022-02-04_mcconkey_fgfr3",
#'   recursive = TRUE,
#'   user = "aragaki-kai",
#'   wb_path = "Raw Data/ChemiDoc"
#' )
#' wb_list(
#'   "2022-02-04_mcconkey_fgfr3",
#'   recursive = FALSE,
#'   user = "aragaki-kai",
#'   wb_path = "Raw Data/ChemiDoc"
#' )
#' }
wb_list <- function(path = "", recursive = FALSE,
                    user = Sys.getenv("WEBBER_USR"),
                    wb_path = Sys.getenv("WEBBER_WBPATH")) {
  check_if_arg_is_empty(arg = user, envar_name = "WEBBER_USR")
  check_if_arg_is_empty(arg = wb_path, envar_name = "WEBBER_WBPATH")
  bladdr::list_gbci_dir(
    fs::path(wb_path, user, path),
    recursive = recursive
  )
}
