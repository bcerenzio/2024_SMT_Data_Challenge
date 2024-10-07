#### copied function from stack overflow & plotly documetnation: 
#https://plotly.com/r/cumulative-animations/
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], time_in_play_s = lvls[[x]])
  })
  bind_rows(dats)
}