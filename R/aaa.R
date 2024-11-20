.onAttach<-function(lib, pkg){
  if(interactive())
  {
    packageStartupMessage(
      "A successor to `move` has been developed and is available on cran (`move2`, https://bartk.gitlab.io/move2/). This brings speed improvements and is based on `sf`. Feedback (including missing functionality) on this new package is welcome, for new projects it might be worth considering starting directly with `move2`."
    )
  }
  invisible()
}
