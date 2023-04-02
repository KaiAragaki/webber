# REFACTOR: 3 times means it's function time

# Seems like some kind of eval substitute magic needs to happen here to get arg
# names

check_if_user_is_empty <- function(user) {
  if (user == "") {
    rlang::abort(
      c("Argument `user` is blank",
        i = "This may be because the environmental variable `WEBBER_USR` isn't set, and the `user` argument wasn't supplied."
      )
    )
  }
}

check_if_wb_path_is_empty <- function(wb_path) {
  if (wb_path == "") {
    rlang::abort(
      c("Argument `wb_path` is blank",
        i = "This may be because the environmental variable `WEBBER_WBPATH` isn't set, and the `wb_path` argument wasn't supplied."
      )
check_if_wb_edit_path_is_empty <- function(wb_edit_path) {
  if (wb_edit_path == "") {
    rlang::abort(
      c("Argument `wb_edit_path` is blank",
        i = "This may be because the environmental variable `WEBBER_WBEDITPATH` isn't set, and the `wb_edit_path` argument wasn't supplied.")
    )
  }
}

check_if_wb_path_is_wb_edit_path <- function(wb_path = NULL, wb_edit_path = NULL) {
  if (is.null(wb_path)) {
    wb_path <- Sys.getenv("WEBBER_WBPATH")
  }

  if (is.null(wb_edit_path)) {
    wb_edit_path <- Sys.getenv("WEBBER_WBEDITPATH")
  }

  # If a wb env isn't set, it'll be ""
  # However, this function isn't about checking to see if something exists
  # The user might not even be supplying one of these args to the parent
  # function.
  if ((wb_path != "" & wb_edit_path != "") & (wb_path == wb_edit_path)) {
    if (wb_path == wb_edit_path) {
      cli::cli_abort(
        c("wb_path and wb_edit_path (or their environmental variables) are the same!",
          x = "To protect you from yourself (and overwriting raw data), this is prohibited.")
      )
    }
  }

}

check_if_bfconvert_exists <- function() {
  exists <- system2(
    "command", c("-v", "bfconvert"),
    stdout = FALSE,
    stderr = FALSE
  )
  if (!exists) {
    cli::cli_abort(
      c(
        "Could not find bfconvert",
        i = "bfconvert doesn't appear to be installed on this machine",
        i = "It might not be in your PATH",
        i = "You can download it (and all of bftools) here:",
        i = "https://bio-formats.readthedocs.io/en/stable/users/comlinetools/index.html"
      )
    )
  }
}

check_if_edit_path_is_wb_path <- function() {

