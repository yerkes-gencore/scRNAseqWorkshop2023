#' Violin plot of gene(s) expression
#'
#' Similar to `Seurat::VlnPlot` but more customizable. Pass a seurat object and
#'  genes to plot. Optionally subset the data by a meta.data variable defined
#'  with `subset_var`, selecting level(s) `subset`. Useful to plot expression
#'  in one subset of your data without having to make separate objects.
#'  Optionally group the data
#'  by meta.data variable `grouping_var`, selecting levels `groups`. Optionally
#'  filter zeros to focus on cells expressing gene.
#'
#'  Requires `ggforce` to be installed
#'
#' @param obj Seurat object
#' @param genes Genes to plot
#' @param subset_var  Optional. Column of `obj@meta.data` to subset on. Default
#'  is `seurat_clusters` so you could `subset` on a specific cluster.
#' @param subset Levels of `subset_var` to subset data to
#' @param grouping_var Optional. Column of `obj@meta.data` to group data by.
#' @param groups Levels of `grouping_var` to include in the plot. Also used
#'  to specify order of levels.
#' @param filter_zeros Remove 0s from plot: default `TRUE`.
#' @param assay Assay to pull expression data from, default `RNA`
#'
#' @returns A ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' ## Plots only the 'Intermediate' cells (as labeled in 'coarse_labels' column)
#' ## Groups results by 'Pre' and 'Post', as labeled in the 'stage' column'
#'   gcoreVlnPlot(myeloid_obj,
#'     genes = c('ISG15', 'ISG20', 'CD14', 'LDHA', 'IFITM1'),
#'     subset = 'Intermediate', subset_var = 'coarse_labels',
#'     grouping_var = 'stage', groups = c('Pre', 'Post'))
#' }
gcoreVlnPlot <- function(obj,
                         genes,
                         assay = 'RNA',
                         subset = NULL,
                         subset_var = 'seurat_clusters',
                         grouping_var,
                         groups,
                         filter_zeros = TRUE){
  if (!is.null(subset)){
    obj <- obj[,obj@meta.data[[subset_var]] %in% subset]
  }
  mat_to_plot <- reshape2::melt(as.matrix(obj@assays[[assay]]@data)[genes,])
  mat_to_plot <- merge(mat_to_plot,
                       obj@meta.data %>%
                         as.data.frame() %>%
                         dplyr::select(.data[[grouping_var]]),
                       by.x = 'Var2', by.y = 0)
  mat_to_plot[[grouping_var]] <- factor(mat_to_plot[[grouping_var]],
                                        levels = groups)
  mat_to_plot <- mat_to_plot %>%
    filter(.data[[grouping_var]] %in% groups)
  
  if (filter_zeros) {mat_to_plot <- mat_to_plot %>% filter(.data[['value']] != 0)}
  
  ggplot2::ggplot(mat_to_plot,
                  aes(x = .data[[grouping_var]],
                      y = .data[['value']],
                      color = .data[[grouping_var]])) +
    # geom_jitter() +
    ggplot2::geom_violin(draw_quantiles = 0.5) +
    ggforce::geom_sina(size = 0.01, alpha = 0.2) +
    ggplot2::facet_wrap(~Var1, scale = 'free_y') +
    ggplot2::theme_bw() +
    ggplot2::labs(caption = paste0(
      if (!is.null(subset)) {
        paste0('Showing expression of subset ', subset,
               ' from variable ', subset_var,'\n')},
      if (filter_zeros) {'Only showing cells with non-zero expression'}),
      y = 'Expression')
}

#' Generate volcano plot from DESeq2 results object
#'
#' Generate volcano plot from DESeq2 results object. Labels can be custom set,
#' or specified as the top N genes by adjusted p value.
#'
#' @param result DESeqResults object
#' @param labels If integer, the top N genes (by adjusted p value) to label.
#'  If a character vector, labels those genes regardless of values.
#'
#' @import EnhancedVolcano
#' @inheritParams EnhancedVolcano::EnhancedVolcano
#'
#' @inheritDotParams EnhancedVolcano::EnhancedVolcano
#'
#' @inherit EnhancedVolcano::EnhancedVolcano return
#'
#' @export
generateVolcanoPlot <- function(result,
                                labels = 20,
                                FCcutoff = log2(1.3),
                                pCutoff = 0.05,
                                title = NULL,
                                caption = NULL,
                                subtitle = NULL,
                                ...) {
  volData <- result[!is.na(result$p_val_adj), ]
  if (is.numeric(labels)) {
    volData <- volData[order(volData$p_val_adj), ]
    labels <- rownames(volData[1:labels, ])
  }
  volplot <- EnhancedVolcano(volData,
                             x = "avg_log2FC",
                             y = "p_val_adj",
                             lab = rownames(volData),
                             selectLab = labels,
                             drawConnectors = TRUE,
                             colConnectors = "lightgrey",
                             pCutoff = pCutoff,
                             FCcutoff = FCcutoff,
                             title = title,
                             caption = caption,
                             subtitle = subtitle,
                             ...
  )
  return(volplot)
}

#' Plot reference mapping calls and scores facetted by cluster
#'
#' Creates violin plots of cell annotation calls and confidence scores, facetted
#'  by query object clusters.
#'
#'  Metadata is pulled from columns in the Seurat
#'  object as specified by the user, so it should be agnostic to SingleR, Azimuth,
#'  or other reference mapping functions that provide labels and scores. You
#'  can optionally specify a minimum proportion threshold to plot, which will
#'  remove data for cell-types appearing in less than X percent of a cluster.
#'  Order of annotation levels appearing on the X axis can be specified with
#'  `facet_order`.
#'
#' @param obj A seurat object with columns for cell-type annotations and annotation scores
#' @param label_column Column to pull annotations labels
#' @param label_score_column Column to pull annotation scores
#' @param clusters_column Column to pull cell clusters
#' @param min_proportion  Minimum proportion of a cluster an annotation must
#'  meet to be plotted
#' @param ncol  Number of columns for facetting
#' @param n_font_size Font size of cluster size labels
#' @param facet_order Order of annotation levels to show on X axis
#'
#' @returns A ggplot object
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' map_prediction_facetplot(
#' combined.obj,
#' label_column = 'predicted.cell_type.lymphTS',
#' label_score_column = 'predicted.cell_type.score.lymphTS',
#' min_freq = 0.05
#' )
#' }
plotRefMapScoresFacet <- function(obj,
                                  label_column,
                                  label_score_column,
                                  clusters_column = 'seurat_clusters',
                                  #mapping_score_column,
                                  min_proportion = 0,
                                  ncol = 3,
                                  n_font_size = 2.5,
                                  facet_order = NULL){
  
  # Calculate proportion of calls in each cluster
  sample_sizes <- obj@meta.data %>%
    dplyr::select({{ clusters_column }}, {{ label_column }}, {{ label_score_column }}) %>%
    dplyr::group_by(dplyr::across(c({{ clusters_column }},{{ label_column }}))) %>%
    dplyr::summarise(n = n(), .groups = 'drop_last') %>%
    dplyr::mutate('freq' = .data[[ 'n' ]] / sum(.data[[ 'n' ]])) %>%
    dplyr::filter(.data[[ 'freq' ]] > min_proportion)
  
  data <- obj@meta.data %>%
    dplyr::select({{ clusters_column }},
                  {{ label_column }},
                  {{ label_score_column }})
  
  # Old, for median mapping scores, specific to Azimuth
  # med_map_scores <- data %>%
  #   dplyr::select(seurat_clusters, {{ mapping_score_column }}) %>%
  #   group_by(seurat_clusters) %>%
  #   summarise(med = median(.data[[mapping_score_column]])) %>%
  #   mutate(label = paste0('Cluster ', seurat_clusters, '\nMedian mapping score: ', round(med, 3)))
  # medmapscores <- as.character(med_map_scores$label)
  # names(medmapscores) <- as.character(med_map_scores$seurat_clusters)
  
  plot_data <- merge(sample_sizes, data,
                     by = c(clusters_column, label_column),
                     all.x = TRUE)
  
  ## Relevel annotation levels if provided
  plot_data[[clusters_column]] <-
    factor(plot_data[[clusters_column]],
           levels = if (is.null(facet_order)){
             sort(unique(plot_data[[clusters_column]]), decreasing = FALSE)
           } else {
             facet_order})
  
  ggplot2::ggplot(plot_data, aes(x = .data[[ label_column ]],
                                 y = .data[[ label_score_column ]],
                                 color = .data[[ label_column ]])) +
    ggplot2::geom_violin(draw_quantiles = c(0.5)) +
    ggplot2::geom_jitter(size=0.2, alpha=0.35) +
    ggplot2::facet_wrap(. ~ factor(.data[[ clusters_column ]] %>% paste("Cluster",.),
                                   levels = gtools::mixedsort(unique(.data[[ clusters_column ]] %>% paste("Cluster",.)))),
                        ncol = ncol,
                        # labeller = ggplot2::labeller(.data[[ clusters_column ]] = medmapscores)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::geom_text(aes(label=n, y = -0.1), angle = 45, size = n_font_size, show.legend = FALSE) +
    ggplot2::ylim(-0.15,NA) +
    ggplot2::labs(x='Predicted cell type',
                  color = 'Predicted cell type',
                  y = 'Prediction score',
                  title='Reference based predictions by cluster',
                  caption = paste0(
                    "Lines in violin plots indicate the median.",
                    "\nNumbers below violins indicate the number of cells in that cluster with that label.",
                    # '\nMedian mapping score is a different metric than the prediction score mapped on the Y axis',
                    if (min_proportion > 0) {
                      paste0("\nCalls for less than ",
                             min_proportion*100,
                             "% of a cluster population are omitted for clarity.")
                    } else NULL )) +
    ggplot2::guides(color = guide_legend(override.aes=list(shape = 15, size = 5, alpha = 1, linetype = 0))) +
    ggplot2::coord_cartesian(clip = "off")
}
