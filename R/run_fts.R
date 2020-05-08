#' run_fts
#'
#' @export

run_fts = function(ms_object, taxa_level, fts_class, fts_id, fts_time, B = 99, C = 0.3, log = TRUE, norm = TRUE) {

  require("metagenomeSeq")

  classes = sort(unique(fData(ms_object)[, taxa_level]))

  intervals_df = data.frame()
  abundance_df = data.frame()
  fit_df = data.frame()

  j = 0

  for (i in classes) {
    j = j + 1
    print(paste(j, i))

    fts_results = fitTimeSeries(obj=ms_object,
                                feature=i,
                                class=fts_class,
                                id=fts_id,
                                time=fts_time,
                                lvl=taxa_level,
                                C=C,
                                B=B,
                                log = log,
                                norm = norm)


    #### intervals
    fit = fts_results$fit$fit
    se = fts_results$fit$se

    confInt95 = 1.96
    minValue = min(fit - (confInt95 * se)) - 0.5
    maxValue = max(fit + (confInt95 * se)) + 0.5

    if (length(fts_results$timeIntervals) > 1) {
      dat = fts_results$data

      sig = as.data.frame(fts_results$timeIntervals) %>%
        arrange(desc(Area)) %>%
        mutate(I = paste0("Interval",row_number())) %>%
        mutate(T = map2(`Interval start`, `Interval end`, ~ seq(from = .x, to = .y)),
               D = ifelse(Area >= 0, levels(dat$class)[2], levels(dat$class)[1])) %>%
        select(I, T, D, p.value, Area) %>%
        unnest(cols = T)

      df = data.frame("fit" = fts_results$fit$fit, "se" = fts_results$fit$se, "T" = fts_results$fit$timePoints) %>%
        left_join(sig, by = "T") %>%
        mutate(I = ifelse(is.na(I), "NONE", I)) %>%
        mutate(F = i)

      intervals_df = rbind(intervals_df, df)
      #### abundances

      mod = gss::ssanova(abundance ~ time * class, data = dat)
      include = c("1", "class", "time:class")

      timePoints = seq(min(dat$time), max(dat$time), by = 1)
      group0 = data.frame(time = timePoints, class = levels(dat$class)[1])
      group1 = data.frame(time = timePoints, class = levels(dat$class)[2])

      pred0 = predict(mod, newdata = group0, include = include, se = TRUE)
      pred1 = predict(mod, newdata = group1, include = include, se = TRUE)

      df.group0 = data.frame("T" = timePoints, "GROUP" = group0$class, "FIT" = pred0$fit, "SE" = pred0$se)
      df.group1 = data.frame("T" = timePoints, "GROUP" = group1$class, "FIT" = pred1$fit, "SE" = pred1$se)

      df_a = data.frame()
      df_a = rbind(df.group0, df.group1) %>%
        mutate(F = i)

      dat$F = i
      abundance_df = rbind(abundance_df, dat)
      fit_df = rbind(fit_df, df_a)

    } else {
      # df = data.frame("fit" = fts_results$fit$fit, "se" = fts_results$fit$se, "T" = fts_results$fit$timePoints, "I" = "NONE", "D" = NA) %>%
      #   mutate(F = i)

    }

  }

  l = list("i" = intervals_df, "a" = abundance_df, "a_fit" = fit_df)
  return(l)
}
