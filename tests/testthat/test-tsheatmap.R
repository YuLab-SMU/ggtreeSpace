context("Add a layer of heat map with trait data")

set.seed(123)

tr <- rtree(15)
td <- fastBM(tr, nsim = 2, bounds = c(0, Inf))
tdex <- data.frame(z = fastBM(tr, nsim = 1, bounds = c(0, Inf)),
                   node = 1:15)

test_that('geom_tsheatmap supports adding external trait data', {
  p <- ggtreespace(tr, td) +
    geom_tsheatmap(trait = tdex, alpha = 0.7 ,resolution = 0.01, bin = 24)
  
  expect_true(ggplot2::is.ggplot(p))
})

test_that('geom_tsheatmap supports adding internal trait data', {
  p <- ggtreespace(tr, td)
  
  p %<+% tdex +
    geom_tippoint() +
    geom_tsheatmap(trait = "z", alpha = 0.7 ,resolution = 0.01, bin = 24) +
    scale_fill_manual(values = col,
                      guide = guide_colorsteps(show.limits = T)
    ) +
    theme_treespace2() +
    theme(legend.key.height = unit(1, "null"),
          legend.justification.top = "right")
  
  expect_true(ggplot2::is.ggplot(p))
})
