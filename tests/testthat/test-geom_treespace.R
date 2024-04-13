test_that("make_ts_data works with tip data", {
    set.seed(123)
    tr <- ape::rtree(10)
    td <- phytools::fastBM(tr, nsim = 2)

    ancd <- as.data.frame(rbind(td, apply(td, 2, fastAnc, tree = as.phylo(tr))))

    ntrd <- ggtreeSpace::make_ts_data(tr, td)
    expect_equivalent(data.frame(V1 = ntrd$x, V2 = ntrd$y), ancd)
})





test_that("make_ts_data works with node data", {
    nwk <- "(((((((OldCheston)CherryHinton)Newnham,CastleEnd,
  NewChesterton)NewMarketRoad)HistoricCenter)SturtonTown)RomseyTown)NewTown;"
    tr <- ape::read.tree(text = nwk)

    dt <- data.frame(
        xc = c(0.32, -0.05, 0.03, 0.07, 0.27, 0.15, 0.02, 0.2, -0.12, 0.17),
        yc = c(
            52.44, 52.37, 52.42, 52.22, 52.27, 52.27, 52.3, 52.33, 52.17,
            52.13
        )
    )
    ntrd <- ggtreeSpace::make_ts_data(tr, dt)


    expect_equal(data.frame(xc = ntrd$x, yc = ntrd$y), dt)
})
