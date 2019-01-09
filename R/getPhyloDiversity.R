#' Phylogenetic Diversity Measures
#'
#' @param p A phyloseq object
#' @param runs Number of bootstraps to run
#' @param null.model The null.model for the SES calculations
#' @param minOccurrence Species must be found at least this many times in at least once sample to be kept

#'
#' @export

getPhyloDiversity = function(p, runs = 999, null.model = "frequency", minOccurrence = 1) {
  require("picante")
  p = filter_taxa(p, function(x) sum(x >= minOccurrence) >= 1, TRUE)

  # make initial objects
  tree = phy_tree(p)

  # root tree if necessary
  if (!is.rooted(tree)) {
    require("phytools")
    tree = midpoint.root(tree)
  }

  tree.co <- cophenetic(tree)
  counts = as.data.frame(t(otu_table(p)))
  counts <- counts[, tree$tip.label] # make sure counts is in the same order as phy.dist

  # PD
  PD = pd(counts, tree, include.root = TRUE)
  phyloDiversity = PD %>% select(PD)

  # MPD
  MPD.unweighted = mpd(counts, tree.co, abundance.weighted = FALSE)
  MPD.weighted   = mpd(counts, tree.co, abundance.weighted = TRUE)
  phyloDiversity$MPD.u = MPD.unweighted
  phyloDiversity$MPD.w = MPD.weighted

  # MNTD
  MNTD.unweighted = mntd(counts, tree.co, abundance.weighted = FALSE)
  MNTD.weighted   = mntd(counts, tree.co, abundance.weighted = TRUE)
  phyloDiversity$MNTD.u = MNTD.unweighted
  phyloDiversity$MNTD.w = MNTD.weighted

  # SES.MPD
  SES.MPD.unweighted = ses.mpd(counts, tree.co, null.model = null.model, abundance.weighted = FALSE, runs = runs) %>%
    select(mpd.obs.z, mpd.obs.p) %>%
    rename(ses.MPD.u.z = mpd.obs.z, ses.MPD.u.p = mpd.obs.p)
  SES.MPD.weighted   = ses.mpd(counts, tree.co, null.model = null.model, abundance.weighted = TRUE,  runs = runs) %>%
    select(mpd.obs.z, mpd.obs.p) %>%
    rename(ses.MPD.w.z = mpd.obs.z, ses.MPD.w.p = mpd.obs.p)

  phyloDiversity = merge(phyloDiversity, SES.MPD.unweighted, by = 'row.names')
  rownames(phyloDiversity) = phyloDiversity$Row.names
  phyloDiversity$Row.names = NULL
  phyloDiversity = merge(phyloDiversity, SES.MPD.weighted, by = 'row.names')
  rownames(phyloDiversity) = phyloDiversity$Row.names
  phyloDiversity$Row.names = NULL

  # SES.MNTD
  SES.MNTD.unweighted = ses.mntd(counts, tree.co, null.model = null.model, abundance.weighted = FALSE, runs = runs) %>%
    select(mntd.obs.z, mntd.obs.p) %>%
    rename(ses.MNTD.u.z = mntd.obs.z, ses.MNTD.u.p = mntd.obs.p)
  SES.MNTD.weighted   = ses.mntd(counts, tree.co, null.model = null.model, abundance.weighted = TRUE,  runs = runs) %>%
    select(mntd.obs.z, mntd.obs.p) %>%
    rename(ses.MNTD.w.z = mntd.obs.z, ses.MNTD.w.p = mntd.obs.p)

  phyloDiversity = merge(phyloDiversity, SES.MNTD.unweighted, by = 'row.names')
  rownames(phyloDiversity) = phyloDiversity$Row.names
  phyloDiversity$Row.names = NULL
  phyloDiversity = merge(phyloDiversity, SES.MNTD.weighted, by = 'row.names')
  rownames(phyloDiversity) = phyloDiversity$Row.names
  phyloDiversity$Row.names = NULL

  sampleData = data.frame(sample_data(p))
  phyloDiversity = merge(sampleData, phyloDiversity, by = 'row.names')
  rownames(phyloDiversity) = phyloDiversity$Row.names
  phyloDiversity$Row.names = NULL

  return(phyloDiversity)
}
