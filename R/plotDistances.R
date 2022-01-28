#' Boxplot of Beta-diversity Distances
#'
#' Takes a `phyloseq` object with samples grouped in the sample data, calculates
#' all pairwise beta-diversity metrics, and plots the distances in facets. Now
#' it returns a list where the df can be accessed with $df, and the plot can be
#' accessed with $plot.
#'
#' @param p A phyloseq object
#' @param m Distance metric
#' @param s Column name with sample IDs
#' @param d Grouping for comparisons
#'
#' @export

plot_distances = function(p = GlobalPatterns, m = "wunifrac", s = "X.SampleID", d = "SampleType") {

  require("phyloseq")
  require("tidyverse")
  require("dplyr")

  # calc distances
  wu.m = phyloseq::distance(p, m) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column("Var1") %>%
    pivot_longer(cols = c(everything(), -Var1), names_to = "Var2", values_to = "value") %>%
    filter(as.character(Var1) != as.character(Var2))

  # get sample data (S4 error OK and expected)
  sd = data.frame(sample_data(p)) %>%
    select(s, d) %>%
    mutate_if(is.factor, as.character)

  # combined distances with sample data
  colnames(sd) = c("Var1", "Type1")
  wu.sd = left_join(wu.m, sd, by = "Var1")

  colnames(sd) = c("Var2", "Type2")
  wu.sd = left_join(wu.sd, sd, by = "Var2")

  # plot
  plot_object = ggplot(wu.sd, aes(x = Type2, y = value)) +
    theme_bw() +
    geom_point() +
    geom_boxplot(aes(color = ifelse(Type1 == Type2, "red", "black"))) +
    scale_color_identity() +
    facet_wrap(~ Type1, scales = "free_x") +
    theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(title = paste0("Distance Metric = ", m), y = m, x = d)

  # rename dataframe back to original names
  s1 = paste(s, "1", sep = "_")
  s2 = paste(s, "2", sep = "_")
  d1 = paste(d, "1", sep = "_")
  d2 = paste(d, "2", sep = "_")

  df = wu.sd %>%
    dplyr::rename(!!s1 := "Var1") %>%
    dplyr::rename(!!s2 := "Var2") %>%
    dplyr::rename(!!d1 := "Type1") %>%
    dplyr::rename(!!d2 := "Type2")


  l = list("plot" = plot_object, "df" = df)

  return(l)
}


#' plotDistances() (deprecated)
#'
#' @export


plotDistances = function(p = GlobalPatterns, m = "wunifrac", s = "X.SampleID", d = "SampleType", plot = TRUE) {
  .Deprecated("plot_distances()")
}
