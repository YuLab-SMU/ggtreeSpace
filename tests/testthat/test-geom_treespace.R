context('geom_treespace')

set.seed(123)
tr <- rtree(10)
td <- fastBM(tr, nsim = 2)


test_that('geom_treespace works with tip data', {
  p <- ggplot() + 
    geom_treespace(tr, td)
  
  expect_true(ggplot2::is.ggplot(p))
})


nwk <- "(((((((OldCheston)CherryHinton)Newnham,CastleEnd,NewChesterton)NewMarketRoad)HistoricCenter)SturtonTown)RomseyTown)NewTown;"
tr <- ape::read.tree(text = nwk)

dt <- data.frame(xc = c(0.32, -0.05, 0.03, 0.07, 0.27, 0.15, 0.02, 0.2, -0.12, 0.17),
                 yc = c(52.44, 52.37, 52.42, 52.22, 52.27, 52.27, 52.3, 52.33, 52.17, 52.13))


test_that('geom_treespace works with node data', {
  p <- ggplot() +
        geom_treespace(tr, dt, mapping = aes(x = x,y = y), size = 1.1, 
                       arrow = arrow(length=unit(0.1, "inches")), lineend = "round", linejoin = "round", 
                       linemitre = 1000)
  
  expect_true(ggplot2::is.ggplot(p))
})
