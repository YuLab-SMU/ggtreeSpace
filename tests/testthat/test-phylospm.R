
test_that('set.params can set parameters properly', {
  tr.params = list(size = 3, 
                 panel.grid = FALSE)

  default.tr.params <- list(size = 1, 
                          colors = c("red", 
                                     'orange', 
                                     'green', 
                                     'cyan', 
                                     'blue'),
                          panel.grid = TRUE)

  expect_mapequal(ggtreeSpace:::set.params(tr.params, default.tr.params), 
               list(size = 3, 
                    colors = c("red", 
                               'orange', 
                               'green', 
                               'cyan', 
                               'blue'),
                    panel.grid = FALSE))
})

