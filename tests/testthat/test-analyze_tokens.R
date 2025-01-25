test_that("Analyze tokens function", {
  expected_analyze_tokens <- data.frame(word = c("chuck", "wood", "woodchuck"),
                                        n = c(2, 2, 2))
  expect_equal(dim(analyze_tokens(in_text = c("How much wood would a woodchuck 
  chuck if a woodchuck could chuck wood?"))),
               dim(expected_analyze_tokens))
})
