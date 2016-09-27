context ("read.spc")
old.spc <- paste0 ("spc/", c ('CONTOUR.SPC', 'DEMO 3D.SPC', 'LC DIODE ARRAY.SPC'))
other.spc <- setdiff (Sys.glob ("spc/*.[sS][pP][cC]"), old.spc)

test_that ("old file format -> error", {
  for (f in old.spc)
    expect_error (read.spc (f))
})

test_that("SPC SDK example files", {
  
  checksums <-  c (`spc/BARBITUATES.SPC` = 'ea0df161b701569878a6334e24272c89',
                   `spc/barbsvd.spc` = 'ea0df161b701569878a6334e24272c89',
                   `spc/BENZENE.SPC` = '62230d381d856dba5fd0d47b4f23ceac',
                   `spc/DRUG SAMPLE_PEAKS.SPC` = 'cc72aa701d26dcefa38bb2f0d8e94b37',
                   `spc/DRUG SAMPLE.SPC` = 'b3f7146e14438c182c14c392a9a559c6',
                   `spc/FID.SPC` = '84728e15c4729dd6c9bb42fa38a19987',
                   `spc/HCL.SPC` = 'a522a71e85de7a0fadd5e7b8e11a5f2f',
                   `spc/HOLMIUM.SPC` = '14840f0dbb0a5a6a43668f22996df6c4',
                   `spc/IG_BKGND.SPC` = 'fbf8f6f8651c777028c62ce0875bb039',
                   `spc/IG_MULTI.SPC` = 'e47ebbbe892fd2a593f5d9159a8aac2d',
                   `spc/IG_SAMP.SPC` = '18a98ff8fc33dab2f966d61d235b4a95',
                   `spc/KKSAM.SPC` = 'b9da38601a6779814cc30aa5e166b579',
                   `spc/POLYR.SPC` = '5e56d2e3655818f7624be79fea0786e5',
                   `spc/POLYS.SPC` = '6109a96011cbf1037aba7b742114e6fa',
                   `spc/SINGLE POLYMER FILM.SPC` = '6d10890c00766709abe045fe7e4d0cb2',
                   `spc/SPECTRUM WITH BAD BASELINE.SPC` = '2017e75947a39bdac23886d08eb8a64a',
                   `spc/TOLUENE.SPC` = '6da20abc17ca8a22ee2ec44270fb763b',
                   `spc/TriVista-linear.spc` = '03c05e5df5450e6ea57ad9488c7cd6e7',
                   `spc/TriVista-normal.spc` = '9a058749cbbc660592a89ec870157ca5',
                   `spc/TUMIX.SPC` = '5d7d1e199d1d9563b785b8252690dd80',
                   `spc/TWO POLYMER FILMS.SPC` = '6109a96011cbf1037aba7b742114e6fa',
                   `spc/Witec-timeseries.spc` = '8310efddaae41cdf9bb3804696faa31d',
                   `spc/XYTRACE.SPC` = '85e1bb738671f9abb4c8952ac0bd3d55')
    
  for (f in other.spc) {
    spc <- read.spc (f)
    expect_equivalent (digest (spc), checksums [f])
  }
})

test_that("LabRam spc files", {
  expect_equal (digest (read.spc("spc.LabRam/LabRam-1.spc")), "1e94aa0059bb680b2b725706e012fd5c")
  expect_equal (digest (read.spc("spc.LabRam/LabRam-2.spc")), "f34707e5be36205a3d66985f29c829c7")
})

