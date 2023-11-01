#' First-time setup helper with webber
#'
#' @return Returns nothing
#' @export
wb_setup <- function() {
  cli::cli_inform("Let's get you set up with webber!")
  cli::cli_inform("Paste the following into the file that was just opened:")
  cli::cli_verbatim(
    "Sys.setenv(WEBBER_USR = \"XXXXXXXX\",
           WEBBER_WBPATH = \"Raw Data/licor\",
           WEBBER_WBEDITPATH = \"Projects and Manuscripts\")"
  )
  cli::cli_inform("Change the XXXXXXXX to be your folder name as it appears on Sharepoint. For me, it's 'aragaki-kai'.")
  cli::cli_inform("Once you've done that, save the file and restart R!")
  cli::cli_inform("You'll also want to make sure you're set up with Sharepoint.")
  cli::cli_inform("See here for more info:")
  cli::cli_text("{.url https://kai.quarto.pub/bok/analysis_setup.html#getting-r-to-talk-to-sharepoint}")
  usethis::edit_r_profile()
}
