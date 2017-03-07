context ("test-read.jdx")

files <- c (Sys.glob ("jcamp-dx/*.DX"), Sys.glob ("jcamp-dx/*.dx"),
            Sys.glob ("jcamp-dx/*.jdx"), Sys.glob ("jcamp-dx/*.JCM"),
            Sys.glob ("jcamp-dx/PE-IR/*.DX"),
            "jcamp-dx/GMD_20111121_MDN35_ALK_JCAMP.txt" # MPI Golm
)

## these files need special parameters:
files <- setdiff (files, c ("jcamp-dx/shimadzu.jdx", "jcamp-dx/virgilio.jdx"))

test_that ("JCAMP-DX examples that need particular parameter sets",{
  expect_equivalent (digest (read.jdx ("jcamp-dx/shimadzu.jdx", encoding = "latin1", keys.hdr2data=TRUE)),
                     "4b5567753e773ad3ddeaec5a64e25ee0")
  expect_equivalent (digest (read.jdx ("jcamp-dx/virgilio.jdx", ytol = 1e-9)),
                     "ecc9f4b5c70fe7162abe89ccbd9f7692")
})

unsupported <- c ("jcamp-dx/BRUKER2.JCM",
                  "jcamp-dx/BRUKER1.JCM",
                  "jcamp-dx/TESTSPEC.DX",
                  "jcamp-dx/TEST32.DX",
                  "jcamp-dx/SPECFILE.DX",
                  "jcamp-dx/ISAS_MS2.DX",
                  "jcamp-dx/ISAS_MS3.DX", # NTUPLES
                  "jcamp-dx/BRUKSQZ.DX",
                  "jcamp-dx/BRUKDIF.DX",
                  "jcamp-dx/BRUKNTUP.DX", # NTUPLES
                  "jcamp-dx/ISAS_CDX.DX", # PEAK ASSIGNMENTS= (XYMA)
                  "jcamp-dx/TESTFID.DX", # NTUPLES
                  "jcamp-dx/TESTNTUP.DX" # NTUPLES
)

checksums <- c (`jcamp-dx/AMA1.DX` = '296b8b12cd14479473968875e71217f3',
                `jcamp-dx/AMA2.DX` = '9ba9917980cb359249cf5bbea03549d8',
                `jcamp-dx/AMA3.DX` = '7c59426f2851053dcbdd7093eaffbe5b',
                `jcamp-dx/br_154_1.DX` = '138bfe5567960ab04521ff59913580c3',
                `jcamp-dx/BRUKAFFN.DX` = 'b10f0bae990e327f41801d7086c6c13a',
                `jcamp-dx/BRUKPAC.DX` = 'a90f6f3ffdce74143ca7867d8fe145ff',
                `jcamp-dx/ISAS_MS1.DX` = 'ca7f296963b4869bc45228b1d8f622e8',
                `jcamp-dx/LABCALC.DX` = 'f9a545da5b531e98667e97dd5ba4735c',
                `jcamp-dx/PE1800.DX` = 'b3ac10af20186e79138e47cdef065334',
                `jcamp-dx/testjose.dx` = '1a3937e8956197b9a819591975990c86',
                `jcamp-dx/sign-rustam.jdx` = '8137a1cb21dbd73e1b4dc9edceb0e72e',
                `jcamp-dx/PE-IR/br_1.DX` = 'b01f56c7a7b9a369537027ac8d596269',
                `jcamp-dx/PE-IR/br_2.DX` = 'f70a1d5fb7332e2e0a71bec07fc18e93',
                `jcamp-dx/PE-IR/br_3.DX` = '0d6df6d392cd38c478e81ecbaea967b1',
                `jcamp-dx/PE-IR/br_4.DX` = '1dd311f8615ae5c28059a719f33a309d',
                `jcamp-dx/PE-IR/br_5.DX` = '087f7303302bec97348fa3964ce1c581',
                `jcamp-dx/PE-IR/fort_1.DX` = '17874f098744d9d903fade83c1576349',
                `jcamp-dx/PE-IR/fort_2.DX` = 'd00aaeb25e5a2df7f0cb80d8b659e74c',
                `jcamp-dx/PE-IR/fort_3.DX` = '086c2f146feebb7fce8da56955f3cc02',
                `jcamp-dx/PE-IR/fort_4.DX` = 'e8107a76808d3cd07ce088c41c3f76e7',
                `jcamp-dx/PE-IR/fort_5.DX` = '7f673586a52458d1d324cd7c3bb645ed',
                `jcamp-dx/PE-IR/lp_1.DX` = '910bd428cc31c6292a165cf0b322b92d',
                `jcamp-dx/PE-IR/lp_2.DX` = '68f120ba1b2dcd57798efaa75b282884',
                `jcamp-dx/PE-IR/lp_3.DX` = '9d005ebdb0e3063764a230c25bf944cd',
                `jcamp-dx/PE-IR/lp_4.DX` = '8ff957598616afbddf99c79304d916cb',
                `jcamp-dx/PE-IR/lp_5.DX` = '688969a513254095ef551dd6b323be61',
                `jcamp-dx/GMD_20111121_MDN35_ALK_JCAMP.txt` = '3c0970681191257e583e74a7382c66eb',
                `jcamp-dx/IR_S_1.DX` =  "73978fd8fd2c3f2975c4d0cfab67d542")


test_that("JCAMP-DX example files", {
  for (f in files [! files %in% unsupported]) {
    spc <- read.jdx (f, ytol = 1e-6)
    # cat (sprintf ("`%s` = '%s',\n", f, digest (spc)))
    expect_equivalent (digest (spc), checksums [f], label = f)
  }
})
