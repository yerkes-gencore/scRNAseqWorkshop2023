
cran_packages <- c(
  'harmony',
  'tidyverse',
  'Seurat',
  'here',
  'ggforce',
  'hdf5r'
)

bioc_packages <- c(
  "SingleR",
  "celldex"
)

github_packages <- c(
)

for (package in cran_packages) {
  tryCatch(
    {
      if (!require(package, quietly = TRUE, character.only = TRUE)) {
        install.packages(package, character.only = TRUE, clean = TRUE)
        message(paste0(package, ' successfully installed'))
      } else {
        message(paste0(package, ' already installed'))
      }
    },
    error = function(e) {
      message(paste0('Error installing package ', package, '\nOriginal message:'))
      message(e)
    }#,
    # warning=function(w){
    #   message(paste0('Warning installing package ', lib, '\nOriginal message:'))
    #   message(w)
    # }
  )
}

for (package in bioc_packages) {
  tryCatch(
    {
      if (!require(package, quietly = TRUE, character.only = TRUE)) {
        BiocManager::install(package)
        message(paste0(package, ' successfully installed'))
      } else {
        message(paste0(package, ' already installed'))
      }
    },
    error = function(e){
      message(paste0('Error installing package ', package, '\nOriginal message:'))
      message(e)
    }#,
    # warning=function(w){
    #   message(paste0('Warning installing package ', lib, '\nOriginal message:'))
    #   message(w)
    # }
  )
}

for (package in github_packages) {
  tryCatch(
    {
      if (!require(package, quietly = TRUE, character.only = TRUE)) {
        devtools::install_github(paste0('github::', package))
        message(paste0(package, ' successfully installed'))
      } else {
        message(paste0(package, ' already installed'))
      }
    },
    error = function(e){
      message(paste0('Error installing package ', package, '\nOriginal message:'))
      message(e)
    }#,
    # warning=function(w){
    #   message(paste0('Warning installing package ', lib, '\nOriginal message:'))
    #   message(w)
    # }
  )
}
