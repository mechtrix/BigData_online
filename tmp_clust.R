plt_sil <- function(dataset, n) {
  out <- fviz_silhouette(
    pam(
      dataset,
      n
    ),
    label = TRUE,
    print.summary = FALSE
  ) +
    theme_bw(
      base_size = 15
    ) +
    scale_fill_brewer(
      palette = "Set1"
    )
  return(out)
}

plt_sil(USArrests, 4)


kclusts <-
  dplyr::tibble(n_clusts = 1:12) %>%
  dplyr::mutate(
    kclust = purrr::map(
      n_clusts,
      ~ kmeans(USArrests, centers = .x, iter.max = 10, nstart = 5)
    ),
    augmented = purrr::map(kclust, broom::augment, USArrests),
    tidied = purrr::map(kclust, broom::tidy),
    glanced = purrr::map(kclust, broom::glance)
  ) %>%
  dplyr::select(-kclust)

point_assignments <- kclusts %>%
  dplyr::select(n_clusts, augmented) %>%
  tidyr::unnest(augmented)

cluster_info <- kclusts %>%
  dplyr::select(n_clusts, tidied) %>%
  tidyr::unnest(tidied)

model_stats <- kclusts %>%
  dplyr::select(n_clusts, glanced) %>%
  tidyr::unnest(glanced)


ggplot2::ggplot(
  data = model_stats,
  aes(
    n_clusts,
    tot.withinss
  )
) +
  ggplot2::geom_line(
    linewidth = 2
  ) +
  ggplot2::scale_x_continuous(
    limits = c(1, 12),
    breaks = seq(1, 12, 1)
  ) +
  ggplot2::ggtitle("Total within sum of squares, by # clusters") +
  theme_bw(base_size = 15)
