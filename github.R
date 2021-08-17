
usethis::use_git()


usethis::use_github()


usethis::create_github_token()


gitcreds::gitcreds_set()


gitcreds::gitcreds_list()


gitcreds::gitcreds_delete()


gh_token_help()


install.packages("oskeyring")
install.packages("git2r")

library(oskeyring)

usethis::edit_r_environ()

usethis::use_git_config(
  user.name = "andreubrito", 
  user.email = "andre.ubrito@gmail.com")


Sys.getenv("GITHUB_PAT")

