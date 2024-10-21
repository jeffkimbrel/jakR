test_that("gives correct info", {
  expect_equal(length(illumina_codes("VH01105.139.AAFNL3GM5.1")), 4)

  expect_equal(illumina_codes("VH01105.139.AAFNL3GM5.1")$machine, "NextSeq 2000")
  expect_equal(illumina_codes("VH01105.139.AAFNL3GM5.1")$machine_code, "VH01105")

  expect_equal(illumina_codes("VH01105.139.AAFNL3GM5.1")$flowcell, "NextSeq_1000/2000")
  expect_equal(illumina_codes("VH01105.139.AAFNL3GM5.1")$flowcell_code, "AAFNL3GM5")
})

test_that("unknown data returns unknown", {
  expect_equal(illumina_codes("123.456.78.9")$machine, "Unknown Illumina Machine")
  expect_equal(illumina_codes("123.456.78.9")$flowcell, "Unknown Flowcell Type")
})

test_that("giving non-string errors", {
  expect_error(illumina_codes(mtcars), "run_id must be a string")
})
