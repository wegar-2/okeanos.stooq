bIsScalarOfClass <- function(objIn, cClassName) {
  return(methods::is(object = objIn, class2 = cClassName) & length(objIn) == 1L)
}
