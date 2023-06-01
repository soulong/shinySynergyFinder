# shinySynergyFinder

## run from local
runApp(app, host='0.0.0.0', port=5000L)

## run from remote
runGitHub('shinySynergyFinder', 'soulong', host='0.0.0.0', port=5000L)


## dependency
if(!require(synergyfinder)) BiocManager::install('synergyfinder')
library(tidyverse)
library(patchwork)
