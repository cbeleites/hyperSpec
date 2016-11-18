context ("read.spc")
old.spc <- paste0 ("spc/", c ('CONTOUR.SPC', 'DEMO 3D.SPC', 'LC DIODE ARRAY.SPC'))
other.spc <- setdiff (Sys.glob ("spc/*.[sS][pP][cC]"), old.spc)

test_that ("old file format -> error", {
  for (f in old.spc)
    expect_error (read.spc (f))
})

test_that("SPC SDK example files", {
  
  checksums <-  c (`spc/BARBITUATES.SPC` = 'b60431f933320c12791a5b5478483770',
                   `spc/barbsvd.spc` = 'd1233e4e64f77b208efa9826ff6e7886',
                   `spc/BENZENE.SPC` = '65edfda407c047a26d758cac9d14c3e7',
                   `spc/DRUG SAMPLE_PEAKS.SPC` = 'b4010a0146de3bb33fea69591bed6052',
                   `spc/DRUG SAMPLE.SPC` = 'b7e7598ccf27ff91602312fe796a3195',
                   `spc/FID.SPC` = 'a57a88eed49a8b3b1ca01b429d5e06e7',
                   `spc/HCL.SPC` = 'b15861b9558d3e15343a0de84d541d33',
                   `spc/HOLMIUM.SPC` = 'd8371d9e0fd62984fc0cf46f12dc2e81',
                   `spc/IG_BKGND.SPC` = '445ec3770b8e6f0070024b059fd81f4c',
                   `spc/IG_MULTI.SPC` = 'cb032af1a3db205a82e39a669dd6b5bd',
                   `spc/IG_SAMP.SPC` = 'e83e3de820c8a1430f42f1f4ea1c075f',
                   `spc/KKSAM.SPC` = '64ca86bd563bc71f4616fd59321aa96b',
                   `spc/POLYR.SPC` = '9805a8a2608fe05012bc9ec55ffcf70a',
                   `spc/POLYS.SPC` = 'c26b60aefae86e6a492266c35ba3e900',
                   `spc/SINGLE POLYMER FILM.SPC` = 'd3691203156a5cb6cde8d458f98310b8',
                   `spc/SPECTRUM WITH BAD BASELINE.SPC` = 'cb2cd63223fdd174824eec08cd36907c',
                   `spc/time.spc` = '6ed422c24d57912e03a924ebd62eef47',
                   `spc/TOLUENE.SPC` = '6fc420528c9f723d296e948f019229aa',
                   `spc/TriVista-linear.spc` = '8512c24cb1bf673725ad3cc4dd0d17da',
                   `spc/TriVista-normal.spc` = '61b7fb5eebb3251edcfd4201de4d9e14',
                   `spc/TUMIX.SPC` = '6a6974b0c4545a980f18b8ecbcd78d46',
                   `spc/TWO POLYMER FILMS.SPC` = '34d9c5c1f98da4906b2de85cc25376bb',
                   `spc/Witec-timeseries.spc` = '5e9bdf41fd1599016fefc0431e19be07',
                   `spc/XYTRACE.SPC` = '29dfa274f0282e6577ef3977aa2864aa')

  for (f in other.spc) {
    spc <- read.spc (f)
#    cat (sprintf ("                   `%s` = '%s',\n", f, digest (spc)))
    expect_equivalent (digest (spc), checksums [f])
  }
})

test_that("LabRam spc files", {
  expect_equal (digest (read.spc("spc.LabRam/LabRam-1.spc")), "502481f3310f21fb98683dfcfd920543")
  expect_equal (digest (read.spc("spc.LabRam/LabRam-2.spc")), "4f37d25a94cfb0fa4ea56aa83f318e0a")
})

