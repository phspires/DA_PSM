#outer_loading
outer.loadings=function(crossload,outermodel){

  w=create.w.matrix(outermodel)
  pesos=w

  pesos=crossload*w

  return(pesos)
}
