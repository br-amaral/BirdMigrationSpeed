# source("renv/activate.R")
# options(vsc.rstudioapi = TRUE)

# guarantees that httpgd is on, and R plots on vs code are plot in the same window
if (interactive()) {
  suppressMessages(require(httpgd))
  hgd <- httpgd::hgd()
  options(device = function(...) httpgd::hgd_browse(hgd))
}