# Define your test
test_that("Commodity Factor is running correctly.", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  
  expect_equal(
    commodity_factor_surprise(
      segment_classification = data.frame(matrix(c(
      "Exploração refino e distribuição", "Petroquímicos", "Exploração de imóveis",
      "Exploração refino e distribuição", "Petroquímicos", "Exploração de imóveis",
      "Exploração refino e distribuição", "Petroquímicos", "Exploração de imóveis"), nrow = 3, ncol = 3)),
      segments_with_positive_sensibility_to_surprise = segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise = segments_with_negative_sensibility_to_surprise,
      surprise_matrix = matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))),
    matrix(c(1,-20,0,5,10,0,3,5,0), nrow = 3, ncol = 3)
  )
})

# Define your test
test_that("Commodity Factor is running correctly.", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  expect_equal(
    commodity_factor_surprise(data.frame(matrix(c(
      "Açúcar e álcool", "Agricultura", "Siderurgia",
      "Açúcar e álcool", "Agricultura", "Siderurgia",
      "Açúcar e álcool", "Agricultura", "Siderurgia"), nrow = 3, ncol = 3)),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))),
    matrix(c(-2,0,0,0,0,-3,55,1,1), nrow = 3, ncol = 3)
  )
  
})

# Define your test
test_that("Commodity Factor is running correctly.", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  
  
  expect_equal(
    commodity_factor_surprise(data.frame(matrix(c(
      "Bancos", "Alimentos", "Material rodoviário",
      "Bancos", "Alimentos", "Material rodoviário",
      "Bancos", "Alimentos", "Material rodoviário"), nrow = 3, ncol = 3)),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))),
    matrix(c(0,0,0,0,0,3,0,1,-1), nrow = 3, ncol = 3)
  )
}
)

# Define your test
test_that("Commodity Factor is running correctly.", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  expect_equal(
    commodity_factor_surprise(data.frame(matrix(c(
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), nrow = 4, ncol = 3)),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))),
    matrix(c(-1,-20,-21,0,0,10,1,-3,9,5,15,1), nrow = 4, ncol = 3)
  )
}
)

# Define your test
test_that("Commodity Factor is running correctly - Matrix.", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  expect_equal(
    commodity_factor_surprise(matrix(c(
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), nrow = 4, ncol = 3),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))),
    matrix(c(-1,-20,-21,0,0,10,1,-3,9,5,15,1), nrow = 4, ncol = 3)
  )
}
)

# Define your test
test_that("Commodity Factor is running correctly - Matrix.", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  expect_equal(
    commodity_factor_surprise(matrix(c(
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), nrow = 4, ncol = 3),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      data.frame(matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes"))))),
    matrix(c(-1,-20,-21,0,0,10,1,-3,9,5,15,1), nrow = 4, ncol = 3)
  )
}
)

# Define your test
test_that("Commodity Factor is running correctly with NAs.", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  expect_equal(
    commodity_factor_surprise(matrix(c(
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
      "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), nrow = 4, ncol = 3),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      matrix(rbind(
        Petróleo = c(1,NA,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,NA,5),
        Açúcar = c(-2,0,55),
        Químicos = c(NA,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(NA,0,NA))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))),
    matrix(c(NA,NA,-21,0,0,10,1,-3,NA,5,15,1), nrow = 4, ncol = 3)
  )
}
)

# Define your test
test_that("Commodity Factor throws an error when dimensions differ", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  expect_error(
    commodity_factor_surprise(
      matrix(c(
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), 
        nrow = 4, ncol = 3),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      
      matrix(rbind(
        Petróleo = c(1,5,3,4),
        Grãos = c(0,0,1,-1),
        Metais = c(0,-3,1,NA),
        Carne = c(2,-1,5,6),
        Açúcar = c(-2,0,55,-1),
        Químicos = c(-20,10,5,2),
        Madeira = c(-21,1,15,4),
        Fertilizantes = c(-1,0,9,15))
        , nrow = 8, ncol = 4, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))
      
    ),
    "Number of columns between segment_classification and surprise_matrix should match."
  )
  
})

# Define your test
test_that("Commodity Factor throws an error when dimensions of lists differ", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira")
  
  expect_error(
    commodity_factor_surprise(
      matrix(c(
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), 
        nrow = 4, ncol = 3),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      
      matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))
      
    ),
    "There should be matching number of elements between segments_with_positive_sensibility_to_surprise, 
         segments_with_negative_sensibility_to_surprise and surprise_matrix"
  )
  
})

# Define your test
test_that("Commodity Factor throws an error when classes are not lists", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  segments_with_negative_sensibility_to_surprise <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  
  expect_error(
    commodity_factor_surprise(
      matrix(c(
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), 
        nrow = 4, ncol = 3),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      
      matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))
      
    ),
    "segments_with_positive_sensibility_to_surprise and segments_with_negative_sensibility_to_surprise should be lists."
  )
  
})


# Define your test
test_that("Commodity Factor throws an error when rownames do not match", {
  segments_with_positive_sensibility_to_surprise <- list()
  segments_with_positive_sensibility_to_surprise[[1]] <- c("Exploração refino e distribuição")
  segments_with_positive_sensibility_to_surprise[[2]] <- c("Agricultura", "Alimentos")
  segments_with_positive_sensibility_to_surprise[[3]] <- c("Minerais metálicos", "Siderurgia")
  segments_with_positive_sensibility_to_surprise[[4]] <- c("Carnes e derivados")
  segments_with_positive_sensibility_to_surprise[[5]] <- c("Açúcar e álcool")
  segments_with_positive_sensibility_to_surprise[[6]] <- c("Químicos diversos", "Petroquímicos")
  segments_with_positive_sensibility_to_surprise[[7]] <- c("Papel e celulose", "Madeira")
  segments_with_positive_sensibility_to_surprise[[8]] <- c("Fertilizantes e defensivos")
  names(segments_with_positive_sensibility_to_surprise) <- c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  segments_with_negative_sensibility_to_surprise <- list()
  segments_with_negative_sensibility_to_surprise[[1]] <- c("Transporte aéreo", "Transporte ferroviário", "Transporte hidroviário",
                                                           "Aluguel de carros", "Exploração de rodovias")
  segments_with_negative_sensibility_to_surprise[[2]] <- c("Cervejaria e refrigerantes")
  segments_with_negative_sensibility_to_surprise[[3]] <- c("Material de transporte", "Material aeronáutico e de defesa",
                                                           "Material rodoviário", "Material ferroviário", "Máq. e equip. industriais",
                                                           "Motores compressores e outros", "Incorporações", "Máq. e equip. construção e agrícolas",
                                                           "Armas e munições", "Automóveis e motocicletas")
  segments_with_negative_sensibility_to_surprise[[4]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[5]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[6]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[7]] <- c("None")
  segments_with_negative_sensibility_to_surprise[[8]] <- c("None")
  names(segments_with_negative_sensibility_to_surprise) <- c("Ronaldo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos", "Madeira", "Fertilizantes")
  
  
  expect_error(
    commodity_factor_surprise(
      matrix(c(
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos",
        "Fertilizantes e defensivos", "Químicos diversos", "Papel e celulose", "Minerais metálicos"), 
        nrow = 4, ncol = 3),
      segments_with_positive_sensibility_to_surprise,
      segments_with_negative_sensibility_to_surprise,
      
      matrix(rbind(
        Petróleo = c(1,5,3),
        Grãos = c(0,0,1),
        Metais = c(0,-3,1),
        Carne = c(2,-1,5),
        Açúcar = c(-2,0,55),
        Químicos = c(-20,10,5),
        Madeira = c(-21,1,15),
        Fertilizantes = c(-1,0,9))
        , nrow = 8, ncol = 3, dimnames = list(c("Petróleo", "Grãos", "Metais", "Carne", "Açúcar", "Químicos",
                                                "Madeira", "Fertilizantes")))
      
    ),
    "surprise_matrix rownames should match names of segments lists"
  )
  
})


