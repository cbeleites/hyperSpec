context ("read.ENVI")

#old <- hy.getOption("file.remove.emptyspc")
#on.exit(hy.setOptions(file.remove.emptyspc = old))

expect_message(read.ENVI ("ENVI/example2.img"), ".read.ENVI.header: Guessing header file name ENVI/example2.hdr")
expect_message(read.ENVI ("ENVI/example2.img"), ".read.ENVI.bin: 'byte order' not given => Guessing 'little'")
#expect_equal(digest (read.ENVI ("ENVI/example2.img")), "36e5f42a8c313153433cf656db4ff4b6")

#hy.setOptions(file.remove.emptyspc = old)
