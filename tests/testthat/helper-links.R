# Creating a symbolic link on Windows needs a privilege that ordinary accounts
# (and the CRAN check machines) do not hold; file.symlink() then returns FALSE
# with a warning. A directory junction needs no privilege, and fs::is_link()
# reports it as a link, so tests that put a link in place of a folder fall back
# to a junction there. Each capability is probed once per test run.
link_capability <- new.env(parent = emptyenv())

# TRUE when this account can create symbolic links.
symlinks_available <- function() {
  if (is.null(link_capability$symlinks)) {
    probe <- tempfile("link-probe-")
    dir.create(probe)
    on.exit(unlink(probe, recursive = TRUE), add = TRUE)
    target <- file.path(probe, "target")
    link <- file.path(probe, "link")
    writeLines("probe", target)
    link_capability$symlinks <-
      isTRUE(suppressWarnings(file.symlink(target, link))) &&
      isTRUE(unname(fs::is_link(link)))
  }
  link_capability$symlinks
}

skip_if_no_symlinks <- function() {
  testthat::skip_if_not(symlinks_available(),
                        "Symbolic links cannot be created on this system")
}

# Put a link to the existing folder `target` at `link`: a symbolic link where
# the account may create one, otherwise a directory junction on Windows.
link_folder <- function(target, link) {
  if (symlinks_available()) return(isTRUE(file.symlink(target, link)))
  if (.Platform$OS.type != "windows") return(FALSE)
  # Sys.junction() is defined on Windows only.
  junction <- get("Sys.junction", envir = baseenv(), mode = "function")
  isTRUE(junction(target, link)) && isTRUE(unname(fs::is_link(link)))
}

skip_if_no_folder_links <- function() {
  if (is.null(link_capability$folders)) {
    probe <- tempfile("link-probe-")
    target <- file.path(probe, "target")
    link <- file.path(probe, "link")
    dir.create(target, recursive = TRUE)
    on.exit(unlink(probe, recursive = TRUE), add = TRUE)
    linked <- suppressWarnings(link_folder(target, link))
    link_capability$folders <- isTRUE(linked)
    if (isTRUE(unname(fs::is_link(link)))) fs::link_delete(link)
  }
  testthat::skip_if_not(
    link_capability$folders,
    "Neither symbolic links nor junctions can be created on this system"
  )
}
