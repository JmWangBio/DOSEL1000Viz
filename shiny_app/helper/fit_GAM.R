
fit_GAM <- function(dat, selected_time) {
  # log-transform dose
  dat.m <- dat %>%
    mutate(logConc = log(pert_dose + pseudo_conc),
           pert_time = as.factor(pert_time))
  
  # fit GAM
  if (length(unique(dat.m$pert_time)) > 1) {
    gam.fit <- tryCatch({
      gam(abundance ~ pert_time + s(logConc, k = 4,
                                    by = pert_time),
          data = dat.m)
    }, error = function(e) {
      e
    })
  } else {
    gam.fit <- tryCatch({
      gam(abundance ~ s(logConc, k = 4,
                        by = pert_time),
          data = dat.m)
    }, error = function(e) {
      e
    })         
  }
  # if no error with model fitting
  if (class(gam.fit)[1] == "gam") {
    d <- gam.fit$model
    x.seq <- seq(min(d$logConc), max(d$logConc), length = 1001)
    pred <- data.frame(logConc = x.seq, fit = predict(
      gam.fit, newdata = data.frame(logConc = x.seq, pert_time = selected_time)
    ))
    return(pred)
  }
  return(NULL)
}
