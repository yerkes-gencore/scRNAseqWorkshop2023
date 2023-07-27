## Helper functions for 02-processing.Rmd
## generate_capture_QC_cutoffs ----
generate_capture_QC_cutoffs <- function(capture,
                                        nmads = 4,
                                        metrics = c('nCount_RNA',
                                                    'nFeature_RNA',
                                                    'percent.rb',
                                                    'percent.mt',
                                                    'log10GenesPerUMI')){
  ## Generates upper and lower threshold based on MAD calculation
  cutoffs <- list()
  for (metric in metrics) {
    thresholds <- .generate_outlier_thresholds(capture, metric, nmads)
    cutoffs[[paste0(metric, '.max')]] <- thresholds[['upper']]
    cutoffs[[paste0(metric, '.min')]] <- thresholds[['lower']]
  }
  return(cutoffs)
}


.generate_outlier_thresholds <- function(capture,
                                         metric,
                                         nmads,
                                         min.value = 0){
  ## helper function for generate_capture_QC_cutoffs
  data <- capture@meta.data[[metric]]
  data.mad <- stats::mad(data)
  data.med <- stats::median(data)
  upper <- data.med + nmads * data.mad
  lower <- data.med - nmads * data.mad
  if (lower < min.value) { lower <- min.value }
  return(list(upper = upper, lower = lower))
}

## filterLowGenes ----
filterLowGenes <- function(obj,
                           min.cells,
                           assay = 'RNA',
                           calculate_only = TRUE){
  removed_genes <- (Matrix::rowSums(obj[[assay]]@counts > 0) < min.cells)
  message(paste0(obj@project.name, ': ', sum(removed_genes),
                 ' genes found in fewer than ', min.cells, ' cells'))
  if (!calculate_only){
    message('Removing low count genes. New object:')
    obj[[assay]] <- SeuratObject::CreateAssayObject(obj[[assay]]@counts,
                                                    min.cells = min.cells)
    show(obj)
  }
  return(obj)
}

## run_scDblFinder ----
run_scDblFinder <- function(obj){
  ## scDblFinder expects data in SCE format
  obj.sce <- Seurat::as.SingleCellExperiment(obj)
  obj.sce <- scDblFinder::scDblFinder(obj.sce, nfeatures = 2500)
  ## Writing the relevant results back to our Seurat object
  obj$scDblFinder.class <- obj.sce$scDblFinder.class
  obj$scDblFinder.score <- obj.sce$scDblFinder.score
  obj
}
