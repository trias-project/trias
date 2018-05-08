context("test_output_spread_with_duplicates")

test0 <- data.frame(
  col1 = c(1, 1, 1),
  col2 = c("H", "H", "H"),
  key = c("A", "B", "C"),
  value = c("R", "S", "T"),
  stringsAsFactors = FALSE
)

test1 <- data.frame(
  col1 = c(1, 1, 1, 1),
  col2 = c("H", "H", "H", "H"),
  key = c("A", "B", "C", "C"),
  value = c("R", "S", "T", "X"),
  stringsAsFactors = FALSE
)

test2 <- data.frame(
  col1 = c(1,1,1,1),
  col2  = c("H", "H", "H", "H"),
  key = c("A", "B", "C", "C"),
  value = c(2, 3, 1, 8),
  stringsAsFactors = FALSE
)

test3 <- data.frame(
  col1 = c(1, 1, 1, 2),	
  key = c("A", "C", "C", "A"),
  value = c("R", "T", "X", "R"),
  stringsAsFactors = FALSE
)

test4 <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
test4 <- tidyr::gather(test4, stock, price, -time)

test5 <- test4
# two different price values for same time - stock combination
test5[2, 1] <- as.Date('2009-01-01')  

test6 <- data.frame(row = rep(c(1, 51), each = 3),
                    var = c("Sepal.Length", "Species", "Species_num"),
                    value = c(5.1, "setosa", 1, 7.0, "versicolor", 2))

testthat::test_that("no duplicates present", {
  expect_equal(
    spread_with_duplicates(test0, key, value), spread(test0, key, value))
  expect_equal(
    spread_with_duplicates(test0, 3, 4), spread(test0, key, value))
  expect_equal(
    spread_with_duplicates(test0, -2, -1), spread(test0, key, value))
  expect_equal(
    spread(test4, key = stock, value = price),
    spread_with_duplicates(test4, key = stock, value = price))
  expect_equal(
    spread(test4, key = time, value = price),
    spread_with_duplicates(test4, key = time, value = price))
  
})

testthat::test_that("keep duplicates", {
  expect_equal(spread_with_duplicates(test1, key, value) %>% nrow(), 2)
  expect_equal(spread_with_duplicates(test1, 3, 4) %>% nrow(), 2)
  expect_equal(spread_with_duplicates(test1, -2, -1) %>% nrow(), 2)
  expect_equal(spread_with_duplicates(test2, key, value) %>% nrow(), 2)
  expect_equal(spread_with_duplicates(test2, 3, 4) %>% nrow(), 2)
  expect_equal(spread_with_duplicates(test2, -2, -1) %>% nrow(), 2)
  expect_equal(
    test1 %>% 
      spread_with_duplicates(key, value) %>% 
      pull(C), 
    test1 %>% filter(key == "C") %>% pull(value))
  expect_equal(
    test2 %>% 
      spread_with_duplicates(key, value) %>% 
      pull(C), 
    test2 %>% filter(key == "C") %>% pull(value))
})

testthat::test_that("key and value columns quoted", {
  expect_equal(
    spread_with_duplicates(test1, "key", "value"),
    spread_with_duplicates(test1, key, value)
  )
  expect_equal(
    spread_with_duplicates(test1, "key", value),
    spread_with_duplicates(test1, key, "value")
  )
})

testthat::test_that("handle NAs", {
  expect_true(
    "No_idea" %in%
      (test3 %>% spread_with_duplicates(key, value, fill = "No_idea") %>% 
         pull(C)))
})

testthat::test_that("apply aggregate function", {
  expect_equal(
    test1 %>%
      spread_with_duplicates(key, value, aggfunc = paste, collapse = "-") %>%
      pull(C), 
    test1 %>% filter(key == "C") %>% pull(value) %>% paste(collapse = "-"))
  expect_equal(
    test1 %>%
      spread_with_duplicates(key, value, aggfunc = str_c, collapse = "-") %>%
      pull(C), 
    test1 %>% filter(key == "C") %>% pull(value) %>% str_c(collapse = "-"))
  expect_equal(
    test2 %>%
      spread_with_duplicates(key, value, aggfunc = max) %>% 
      pull(C),
    test2 %>% filter(key == "C") %>% summarize(max = max(value)) %>% pull())
  expect_equal(
    test2 %>% 
      spread_with_duplicates(key, value, aggfunc = mean) %>% 
      pull(C),
    test2 %>% filter(key == "C") %>% summarize(mean = mean(value)) %>% pull())
  expect_equal(
    test2 %>% spread_with_duplicates(key, value, aggfunc = length) %>% pull(C),
    test2 %>% filter(key == "C") %>% nrow())
  expect_equal(
    test5 %>% 
      spread_with_duplicates(stock, price, aggfunc = mean) %>% 
      filter(time == as.Date("2009-01-01")) %>% 
      select("X"), 
    test5 %>% 
      filter(stock == "X") %>% 
      filter(time == as.Date("2009-01-01")) %>%
      summarize(X = mean(price)))
})

testthat::test_that("test convert equal TRUE", {
  expect_equal(test6 %>% 
                 spread_with_duplicates(var, value, convert = TRUE),
               test6 %>% 
                 spread(var, value, convert = TRUE))
})
