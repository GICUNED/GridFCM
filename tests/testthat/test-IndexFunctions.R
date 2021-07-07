# Tests para las funciones Index
matrix <- cbind(c(0,0,0,1),c(1,0,0,0),c(0,1,0,0),c(0,0,1,0))

test_that("CentDegree", {                                                       # Test de resultados para la función CentDegree con todos los métodos
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

test_that("DistMatrix",{                                                        # Test de resultados para la función DistMatrix con todos los modos
  expect_equal(DistMatrix(matrix),rbind(c(0,1,2,3),c(3,0,1,2),c(2,3,0,1),
                                        c(1,2,3,0)))
  expect_equal(DistMatrix(matrix, mode = "in"),rbind(c(0,3,2,1),c(1,0,3,2),
                                                     c(2,1,0,3),c(3,2,1,0)))
  expect_equal(DistMatrix(matrix, mode = "all"),rbind(c(0,1,2,1),c(1,0,1,2),
                                                      c(2,1,0,1),c(1,2,1,0)))
})

test_that("CentClose",{
  expect_equal(CentClose(matrix,norm = TRUE),c(3/6,3/6,3/6,3/6), tolerance = 0.001)
  expect_equal(CentClose(matrix,norm = FALSE),c(0.16666,0.16666,0.16666,0.16666), tolerance = 0.001)
})

test_that("CentBetw",{
  expect_equal(CentBetw(matrix,norm = TRUE),c(3/6,3/6,3/6,3/6), tolerance = 0.001)
  expect_equal(CentBetw(matrix,norm = FALSE),c(3,3,3,3), tolerance = 0.001)
})
