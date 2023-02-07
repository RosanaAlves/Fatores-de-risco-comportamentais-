
# Criar projeto (lembrar de usar caminho absoluto!)
#usethis::create_project("/home/rosana/Documentos/Curso-R/Ciencia de dados II/TAREFAS/Tarefas aula1/Licao de casa/Maisroubos")

### O R VAI ABRIR UMA NOVA SESSÃO

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
