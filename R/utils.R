check_if_arg_is_empty <- function(arg, envar_name) {
  if (arg == "") {
    name <- deparse(substitute(arg))
    cli::cli_abort(
           c("Argument {.code {name}} is blank.",
             i = "Is envvar {.envvar {envar_name}} set?")
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
  if ((wb_path != "" && wb_edit_path != "") && (wb_path == wb_edit_path)) {
    if (wb_path == wb_edit_path) {
      cli::cli_abort(
        c("wb_path and wb_edit_path (or their envvars) are the same!",
          x = "To protect you from overwriting raw data, this is prohibited.")
      )
    }
  }

}

check_if_bfconvert_exists <- function() {
  exists <- suppressWarnings(
    system2(
      "command", c("-v", "bfconvert"),
      stdout = FALSE,
      stderr = FALSE
    )
  )
  # Returns 0 if it exists

  if (exists != 0) {
    cli::cli_abort(
      c(
        "Could not find bfconvert",
        i = "bfconvert doesn't appear to be installed on this machine",
        i = "It might not be in your PATH",
        i = "You can download it (and all of bftools) here:",
        i = "https://bio-formats.readthedocs.io/en/stable/users/comlinetools/index.html" #nolint
      )
    )
  }
}

check_if_edit_path_is_wb_path <- function() {

}
