# Tests para las funciones Index

test_that("CentDegree", {
  expect_equal(as.vector(CentDegree(impgrid_01)$Entrada),c(5,2,7,5,3,4,2,8,5,7))

  expect_equal(as.vector(CentDegree(impgrid_01)$Salida),c(5,5,7,4,5,3,3,5,5,6))

  expect_equal(as.vector(CentDegree(impgrid_01, method = "weight")$Entrada),
               c(2.330,0.667,3.667,3.667,2,2,1,4.330,1.667,4.667),
               tolerance = 0.001)

  expect_equal(as.vector(CentDegree(impgrid_01, method = "weight")$Salida),
               c(4,1.667,4.667,2,2,1.333,1.333,2.667,2.667,3.667),
               tolerance = 0.001)

  expect_equal(as.vector(CentDegree(impgrid_01, method = "wnorm")$Entrada),
               c(2.330,0.667,3.667,3.667,2,2,1,4.330,1.667,4.667)/9,
               tolerance = 0.001)

  expect_equal(as.vector(CentDegree(impgrid_01, method = "wnorm")$Salida),
               c(4,1.667,4.667,2,2,1.333,1.333,2.667,2.667,3.667)/9,
               tolerance = 0.001)

  expect_equal(as.vector(CentDegree(impgrid_01, method = "norm")$Entrada),
               c(5,2,7,5,3,4,2,8,5,7)/9)

  expect_equal(as.vector(CentDegree(impgrid_01, method = "norm")$Salida),
               c(5,5,7,4,5,3,3,5,5,6)/9)

  expect_equal(as.vector(CentDegree(impgrid_01, method = "ego")$Entrada),
               c(5,2,7,5,3,4,2,8,5,7)/(9*10))

  expect_equal(as.vector(CentDegree(impgrid_01, method = "ego")$Salida),
               c(5,5,7,4,5,3,3,5,5,6)/(9*10))

})

