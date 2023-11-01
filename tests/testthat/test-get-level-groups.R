# Tests the get_level_groups helper function called by top_levels()

shorts <- factor(c("a", "b", "c", "d", "e", "f"), levels = rev(letters[1:6]))
longs <- factor(c("aaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbbb", "cccccccccccccccccccc", "dddddddddddddddd", NA, "hhhhhhhhhhhhhhhh", "bbbbbbbbbbbbbbbbb"), levels = c("dddddddddddddddd", "aaaaaaaaaaaaaaaa", "cccccccccccccccccccc", "bbbbbbbbbbbbbbbbb", "hhhhhhhhhhhhhhhh"))

short1 <- get_level_groups(shorts, 1, max(as.numeric(shorts), na.rm = TRUE))
short2 <- get_level_groups(shorts, 2, max(as.numeric(shorts), na.rm = TRUE))
short3 <- get_level_groups(shorts, 3, max(as.numeric(shorts), na.rm = TRUE))

test_that("names are grouped properly and groups are ordered correctly", {
  expect_equal(short1, list(top = "f", mid = "e, d, c, b", bot = "a"))
  expect_equal(short2, list(top = "f, e", mid = c("d, c"), bot = "b, a"))
  expect_equal(short3, list(top = "f, e, d", mid = NA, bot = "c, b, a"))
})

long1 <- get_level_groups(longs, 1, max(as.numeric(longs), na.rm = TRUE))
long2 <- get_level_groups(longs, 2, max(as.numeric(longs), na.rm = TRUE))

test_that("truncation works correctly", {
  expect_equal(long1, list(top = "dddddddddddddddd", mid = "<<< Middle Group (3 categories) >>>", bot = "hhhhhhhhhhhhhhhh"))
  expect_equal(long2, list(top = "dddddddddddddddd, aaaaaaaaa...", mid = "cccccccccccccccccccc", bot = "bbbbbbbbbbbbbbbbb, hhhhhhhh..."))
  expect_equal(nchar(long2$top), 30)
  expect_equal(nchar(long2$bot), 30)
})
