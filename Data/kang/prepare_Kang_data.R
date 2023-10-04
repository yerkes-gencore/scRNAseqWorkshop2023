library(DropletUtils)
library(Seurat)
library(here)
library(scater)
library(Seurat)

## This is only kept for record keeping purposes for how data was generated
## You don't need to run this

kang.sce <- muscData::Kang18_8vs8()
kang.sce <- scater::logNormCounts(kang.sce)
kang <- Seurat::as.Seurat(kang.sce)
kang$stim <- as.character(kang$stim)

for (individual in unique(kang$ind)) {
  for (cond in c('ctrl', 'stim')){
    kang.sub <- subset(kang, subset = ind == individual & stim == cond)
    DropletUtils::write10xCounts(kang.sub@assays$originalexp@counts,
                                 path = here::here(paste0("Data/kang/", 's', individual, '_', cond, '.h5')),
                                 type='HDF5', version="3", overwrite = TRUE)
  }
}

## Testing all subsets ----

for (individual in unique(kang$ind)) {
  for (cond in c('ctrl', 'stim')){
    name <- paste0(individual, '_', cond)
    obj <- Read10X_h5(here::here(paste0('Data/kang/s', name,'.h5')))
    obj <- CreateSeuratObject(obj)
    rmarkdown::render(here::here('Session-02/02-QC_processing.Rmd'),
                      output_dir = here('Session-02/test_outputs'),
                      output_file = paste0(name, '.html'))
  }
}
