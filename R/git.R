library(usethis)

# Uma vez na vida
use_git_config(
  user.name = "RosanaAlves",
  user.email = "rosanaalvesmolina7@gmail.com"
)

# Uma vez na vida
git_default_branch_configure()

# Uma vez por projeto
use_git()

# Uma vez na vida
create_github_token()

# Uma vez na vida
gitcreds::gitcreds_set()

# Uma ver por projeto
use_github()
#usethis::edit_r_environ() editar
