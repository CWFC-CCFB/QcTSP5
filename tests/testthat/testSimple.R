#'
#' A series of simple tests
#'

restoreQcTSP5Data()

test_that("Testing nb rows in plots", {expect_equal(nrow(QcTSP5Data$plots), 40246)})
test_that("Testing nb rows in sites", {expect_equal(nrow(QcTSP5Data$sites), 40151)})
test_that("Testing nb rows in photoInterpretedStands", {expect_equal(nrow(QcTSP5Data$photoInterpretedStands), 40246)})
test_that("Testing nb rows in trees", {expect_equal(nrow(QcTSP5Data$trees),  1876579)})
test_that("Testing nb rows in studyTrees", {expect_equal(nrow(QcTSP5Data$studyTrees), 183499)})
test_that("Testing nb rows in saplings", {expect_equal(nrow(QcTSP5Data$saplings), 166718)})

plots <- c("0300600101", "0300600202",  "0301700302")
selectedTrees <- extractArtemisFormatForMetaModelling(QcTSP5Data, plots)
selectedTrees <- selectedTrees[which(selectedTrees$TREEDHPCM >= 9),]
test_that("Testing nb rows in selectedTrees", {expect_equal(nrow(selectedTrees), 114)})
test_that("Testing nb rows in selectedTrees", {expect_equal(nrow(selectedTrees[which(selectedTrees$TREESTATUS %in% c(10,12)),]), 99)})

plots <- QcTSP5Data$plots
unique(plots$TYPE_PE)



stratumPlots <- data.frame("stratum"=c("BOJ","BOJ","SAB"),"plots"=c("0300600101", "0300600202",  "0301700302"))
selectedTrees <- extractNaturaFormatForMetaModelling(QcTSP5Data, stratumPlots)
test_that("Testing nb rows in selectedTrees", {expect_equal(nrow(selectedTrees[[1]]), 114)})
test_that("Testing nb rows in selectedTrees", {expect_equal(nrow(selectedTrees[[1]][which(selectedTrees[[1]]$TREESTATUS %in% c(10,12)),]), 99)})


selectedTrees <- extractNaturaFormatForMetaModelling(QcTSP5Data, stratumPlots)
test_that("Testing nb rows in selectedTrees", {expect_equal(nrow(selectedTrees[[2]]), 8)})
