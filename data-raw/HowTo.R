# first time setup
usethis::create_package("path/to/MyPackage")   # create package scaffold
usethis::use_git()                              # initialise git repo
usethis::use_github()                           # create & link GitHub repo

# daily development cycle
devtools::load_all()      # load all R/ functions into session for testing
devtools::document()      # rebuild man/ from roxygen comments
devtools::check()         # full R CMD CHECK — run before pushing
devtools::test()          # run tests/testthat/

Ctrl + Shift + F10 restart R STudio

# publish update to GitHub
# (just regular git: commit + push — no special devtools function needed)
