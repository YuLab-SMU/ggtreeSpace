test_that("make_hm_data supports adding external and internal trait data", {
  library(ggtree)
  library(ggplot2)
  library(phytools)
  library(dplyr)

  set.seed(123)

  tr <- rtree(15)
  trd <- ggtree::fortify(tr)
  td <- fastBM(tr, nsim = 2, bounds = c(0, Inf))

  tdex <- data.frame(
    z = fastBM(tr, nsim = 1, bounds = c(0, Inf)),
    node = 1:15
  )
  trd2 <- left_join(trd, tdex)

  data1 <- ggtreeSpace:::make_hm_data(trd, tdex, 0.01)

  data2 <- ggtreeSpace:::make_hm_data(trd2, "z", 0.01)


  expect_identical(data2, data1)
})
