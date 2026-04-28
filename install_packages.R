# Instaluje wymagane pakiety (pomija te już zainstalowane)
req <- c(
  'readxl','ggplot2','dplyr','readr','stringr','rmarkdown','yaml',
  'glmnet','ranger','fastshap'
)
inst <- req[!sapply(req, requireNamespace, quietly = TRUE)]
if(length(inst)) {
  install.packages(inst, repos='https://cloud.r-project.org')
} else {
  message('Wszystkie wymagane pakiety już są.')
}
