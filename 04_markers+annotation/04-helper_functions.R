plotClusterAnnotTile <- function(obj.seurat, labels, assay = "RNA", plot_proportions = TRUE) {
  plot_data <- obj.seurat@meta.data %>%
    group_by(.data[["seurat_clusters"]], .data[[labels]]) %>%
    summarize(n = n()) %>%
    mutate(proportion = n/sum(n)) %>%
    ungroup() %>%
    group_by(.data[[labels]]) %>%
    mutate(colSum = sum(n)) %>%
    ungroup() %>%
    dplyr::filter(.data[["colSum"]] > 10)

  ggplot(data = plot_data,
         aes(x = .data[["seurat_clusters"]],
             y = .data[[labels]],
             fill = if (plot_proportions) {proportion} else {log10(n + 10)})) +
    geom_tile() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_blank(),
          legend.position = "top",
          panel.background = element_rect(fill = "black")) +
    labs(fill = (if (plot_proportions) {'Proportion of cells in cluster'} else {'log10(n+10)'}))
}

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
    ggplot2::labs(x=label_column,
                  color = 'Predicted cell type',
                  y = label_score_column,
                  title='Reference based predictions by cluster',
                  caption = paste0(
                    "Lines in violin plots indicate the median.",
                    "\nNumbers below violins indicate the number of cells in that cluster with that label.",
                    if (min_proportion > 0) {
                      paste0("\nCalls for less than ",
                             min_proportion*100,
                             "% of a cluster population are omitted for clarity.")
                    } else NULL )) +
    ggplot2::guides(color = guide_legend(override.aes=list(shape = 15, size = 5, alpha = 1, linetype = 0))) +
    ggplot2::coord_cartesian(clip = "off")
}
