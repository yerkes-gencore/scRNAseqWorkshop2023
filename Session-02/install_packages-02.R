
cran_packages <- c(
  'BiocManager',
  'here',
  'tidyverse',
  'Seurat',
  'clustree'
)

bioc_packages <- c(
  'scDblFinder'
)

github_packages <- c(
)

for (package in cran_packages){
  tryCatch(
    {
      if (!require(package, quietly = FALSE, character.only = TRUE)){
        install.packages(package, character.only = TRUE, clean = TRUE)
      }
    },
    error=function(e){
      message(paste0('Error installing package ', package, '\nOriginal message:'))
      message(e)
    }#,
    # warning=function(w){
    #   message(paste0('Warning installing package ', lib, '\nOriginal message:'))
    #   message(w)
    # }
  )
}

for (package in bioc_packages){
  tryCatch(
    {
      if (!require(package, quietly = FALSE, character.only = TRUE)){
        BiocManager::install(package)
      }
    },
    error=function(e){
      message(paste0('Error installing package ', package, '\nOriginal message:'))
      message(e)
    }#,
    # warning=function(w){
    #   message(paste0('Warning installing package ', lib, '\nOriginal message:'))
    #   message(w)
    # }
  )
}

for (package in github_packages){
  tryCatch(
    {
      if (!require(package, quietly = FALSE, character.only = TRUE)){
        devtools::install_github(paste0('github::', package))
      }
    },
    error=function(e){
      message(paste0('Error installing package ', package, '\nOriginal message:'))
      message(e)
    }#,
    # warning=function(w){
    #   message(paste0('Warning installing package ', lib, '\nOriginal message:'))
    #   message(w)
    # }
  )
}
