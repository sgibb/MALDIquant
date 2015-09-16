## deprecated since MALDIquant 1.11.12
plotImsSlice <- function(x, range=c(0, Inf),
                         sub=paste0("m/z: ", range[1L], "-", range[2L], ""),
                         removeEmptyRows=TRUE,
                         removeEmptyCols=TRUE,
                         colRamp=colorRamp(c("black", "blue", "green",
                                             "yellow", "red")),
                         interpolate=FALSE, ...) {
  .deprecatedFunction("1.11.12", new="plotMsiSlice")
}
