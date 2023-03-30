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
    )
  }
}