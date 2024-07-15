# source("renv/activate.R")
# options(vsc.rstudioapi = TRUE)

# guarantees that httpgd is on, and R plots on vs code are plot in the same window
if (interactive()) {
  suppressMessages(require(httpgd))
  hgd <- httpgd::hgd()
  options(device = function(...) httpgd::hgd_browse(hgd))
}

if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}

# turn of the underlines from lintr for spell check
options(languageserver.diagnostics = FALSE)