all: barbiturates.rda paracetamol.rda fileio.pdf

clean:
	rm -f *-fig-*.pdf *.aux *.bbl *.blg *.out *.log *-pkg.bib *.toc *.idx *.ilg *.ind *.rda
	rm -f *~ .*~ \#*\#

.SECONDARY: barbiturates.rda paracetamol.rda
.PRECIOUS: barbiturates.rda paracetamol.rda

# raw files -----------------------------------------------------------------------------------------
asc.Andor = asc.Andor/ASCII-Andor-Solis.asc

ENVI =    ENVI/example2.hdr ENVI/example2.img ENVI/toy.bil ENVI/toy.bil.aux.xml ENVI/toy.hdr

jaz =     jaz/oo_destination.spc jaz/source.jaz

jcamp-dx = jcamp-dx/2934luana.JDX jcamp-dx/2934luana-nolinkblock.JDX \
          jcamp-dx/AMA1.DX jcamp-dx/AMA2.DX jcamp-dx/AMA3.DX \
          jcamp-dx/br_154_1.DX \
          jcamp-dx/BRUKAFFN.DX jcamp-dx/BRUKDIF.DX jcamp-dx/BRUKER1.JCM jcamp-dx/BRUKER2.JCM \
               jcamp-dx/BRUKNTUP.DX jcamp-dx/BRUKPAC.DX jcamp-dx/BRUKSQZ.DX \
          jcamp-dx/DX-DIR.TXT \
          jcamp-dx/GMD_20111121_MDN35_ALK_JCAMP.txt \
          jcamp-dx/ISAS_CDX.DX jcamp-dx/ISAS_MS1.DX jcamp-dx/ISAS_MS2.DX jcamp-dx/ISAS_MS3.DX \
          jcamp-dx/LABCALC.DX \
          jcamp-dx/PE1800.DX \
          jcamp-dx/PE-IR/br_1.DX jcamp-dx/PE-IR/br_2.DX jcamp-dx/PE-IR/br_3.DX jcamp-dx/PE-IR/br_4.DX \
               jcamp-dx/PE-IR/br_5.DX \
          jcamp-dx/PE-IR/fort_1.DX jcamp-dx/PE-IR/fort_2.DX jcamp-dx/PE-IR/fort_3.DX  \
               jcamp-dx/PE-IR/fort_4.DX jcamp-dx/PE-IR/fort_5.DX \
          jcamp-dx/PE-IR/lp_1.DX jcamp-dx/PE-IR/lp_2.DX jcamp-dx/PE-IR/lp_3.DX jcamp-dx/PE-IR/lp_4.DX \
               jcamp-dx/PE-IR/lp_5.DX \
          jcamp-dx/shimadzu.jdx \
          jcamp-dx/SPECFILE.DX \
          jcamp-dx/TEST32.DX \
          jcamp-dx/TESTFID.DX \
          jcamp-dx/testjose.dx \
          jcamp-dx/TESTNTUP.DX \
          jcamp-dx/TESTSPEC.DX \
          jcamp-dx/virgilio.jdx \
          jcamp-dx/IR_S_1.DX

mat.cytospec = mat.cytospec/cytospec.mat

mat.Witec = mat.Witec/LineX.mat mat.Witec/LineY.mat mat.Witec/Map.mat mat.Witec/time-series.mat  \
           mat.Witec/Z-Stack.mat

NIST      = NIST/mercurytable2.htm

pz2       = pz2/handyspec_soilplant.PZ2 pz2/read.txt.pz2.R

spc       = spc/BARBITUATES.SPC spc/barbsvd.spc spc/BENZENE.SPC spc/CONTOUR.SPC spc/DEMO\ 3D.SPC \
          spc/DRUG\ SAMPLE_PEAKS.SPC spc/DRUG\ SAMPLE.SPC spc/FID.SPC spc/HCL.SPC spc/HOLMIUM.SPC \
          spc/IG_BKGND.SPC spc/IG_MULTI.SPC spc/IG_SAMP.SPC spc/KKSAM.SPC spc/LC\ DIODE\ ARRAY.SPC \
          spc/POLYR.SPC spc/POLYS.SPC spc/SINGLE\ POLYMER\ FILM.SPC \
          spc/SPECTRUM\ WITH\ BAD\ BASELINE.SPC spc/time.spc spc/TOLUENE.SPC spc/TriVista-linear.spc \
          spc/TriVista-normal.spc spc/TUMIX.SPC spc/TWO\ POLYMER\ FILMS.SPC spc/Witec-timeseries.spc \
          spc/XYTRACE.SPC

spc.Kaisermap = spc.Kaisermap/ebroAVII.spc spc.Kaisermap/ebroAVIJ.spc spc.Kaisermap/ebroAVIK.spc \
          spc.Kaisermap/ebroAVIL.spc spc.Kaisermap/ebroAVIM.spc spc.Kaisermap/ebroAVIN.spc \
          spc.Kaisermap/ebroAVIO.spc spc.Kaisermap/ebroAVIP.spc spc.Kaisermap/ebroAVIQ.spc \
          spc.Kaisermap/ebroAVIR.spc spc.Kaisermap/ebroAVIS.spc spc.Kaisermap/ebroAVIT.spc \
          spc.Kaisermap/ebroAVIU.spc spc.Kaisermap/ebroAVIV.spc spc.Kaisermap/ebroAVIW.spc \
          spc.Kaisermap/ebroAVIX.spc spc.Kaisermap/ebroAVIY.spc spc.Kaisermap/ebroAVIZ.spc \
          spc.Kaisermap/ebroAVMY.spc spc.Kaisermap/ebroAVMZ.spc spc.Kaisermap/ebroAVN0.spc \
          spc.Kaisermap/ebroAVN1.spc spc.Kaisermap/ebroAVN2.spc spc.Kaisermap/ebroAVN3.spc \
          spc.Kaisermap/ebroAVN4.spc spc.Kaisermap/ebroAVN5.spc spc.Kaisermap/ebroAVN6.spc \
          spc.Kaisermap/ebroAVN7.spc spc.Kaisermap/ebroAVN8.spc spc.Kaisermap/ebroAVN9.spc \
          spc.Kaisermap/ebroAVNA.spc spc.Kaisermap/ebroAVNB.spc spc.Kaisermap/ebroAVNC.spc \
          spc.Kaisermap/ebroAVND.spc spc.Kaisermap/ebroAVNE.spc spc.Kaisermap/ebroAVNF.spc \
          spc.Kaisermap/ebroAVNG.spc spc.Kaisermap/ebroAVNH.spc spc.Kaisermap/ebroAVNI.spc \
          spc.Kaisermap/ebroAVNJ.spc spc.Kaisermap/ebroAVNK.spc spc.Kaisermap/ebroAVNL.spc \
          spc.Kaisermap/ebroAVNM.spc spc.Kaisermap/ebroAVNN.spc spc.Kaisermap/ebroAVNO.spc \
          spc.Kaisermap/ebroAVNP.spc spc.Kaisermap/ebroAVNQ.spc spc.Kaisermap/ebroAVNR.spc \
          spc.Kaisermap/ebroAVNS.spc spc.Kaisermap/ebroAVNT.spc spc.Kaisermap/ebroAVNU.spc \
          spc.Kaisermap/ebroAVNV.spc spc.Kaisermap/ebroAVNW.spc spc.Kaisermap/ebroAVNX.spc \
          spc.Kaisermap/ebroAVRW.spc spc.Kaisermap/ebroAVRX.spc spc.Kaisermap/ebroAVRY.spc \
          spc.Kaisermap/ebroAVRZ.spc spc.Kaisermap/ebroAVS0.spc spc.Kaisermap/ebroAVS1.spc \
          spc.Kaisermap/ebroAVS2.spc spc.Kaisermap/ebroAVS3.spc spc.Kaisermap/ebroAVS4.spc \
          spc.Kaisermap/ebroAVS5.spc spc.Kaisermap/ebroAVS6.spc spc.Kaisermap/ebroAVS7.spc \
          spc.Kaisermap/ebroAVS8.spc spc.Kaisermap/ebroAVS9.spc spc.Kaisermap/ebroAVSA.spc \
          spc.Kaisermap/ebroAVSB.spc spc.Kaisermap/ebroAVSC.spc spc.Kaisermap/ebroAVSD.spc \
          spc.Kaisermap/ebroAVSE.spc spc.Kaisermap/ebroAVSF.spc spc.Kaisermap/ebroAVSG.spc \
          spc.Kaisermap/ebroAVSH.spc spc.Kaisermap/ebroAVSI.spc spc.Kaisermap/ebroAVSJ.spc \
          spc.Kaisermap/ebroAVSK.spc spc.Kaisermap/ebroAVSL.spc spc.Kaisermap/ebroAVSM.spc \
          spc.Kaisermap/ebroAVSN.spc spc.Kaisermap/ebroAVSO.spc spc.Kaisermap/ebroAVSP.spc \
          spc.Kaisermap/ebroAVSQ.spc spc.Kaisermap/ebroAVSR.spc spc.Kaisermap/ebroAVST.spc \
          spc.Kaisermap/ebroAVSU.spc spc.Kaisermap/ebroAVSV.spc spc.Kaisermap/ebroAVSW.spc \
          spc.Kaisermap/ebroAVWU.spc spc.Kaisermap/ebroAVWV.spc spc.Kaisermap/ebroAVWX.spc \
          spc.Kaisermap/ebroAVWY.spc spc.Kaisermap/ebroAVWZ.spc spc.Kaisermap/ebroAVX0.spc \
          spc.Kaisermap/ebroAVX1.spc spc.Kaisermap/ebroAVX2.spc spc.Kaisermap/ebroAVX3.spc \
          spc.Kaisermap/ebroAVX4.spc spc.Kaisermap/ebroAVX5.spc spc.Kaisermap/ebroAVX6.spc \
          spc.Kaisermap/ebroAVX7.spc spc.Kaisermap/ebroAVX8.spc spc.Kaisermap/ebroAVX9.spc \
          spc.Kaisermap/ebroAVXA.spc spc.Kaisermap/ebroAVXB.spc spc.Kaisermap/ebroAVXC.spc

spc.LabRam = spc.LabRam/LabRam-1.spc spc.LabRam/LabRam-2.spc

spc.Shimadzu = spc.Shimadzu/F80A20-1.SPC

spc.Witec = spc.Witec/P_A32_006_Spec.Data\ 1.spc spc.Witec/P_A32_007_Spec.Data\ 1.spc \
          spc.Witec/Read_Project4_data/Matlab_format/P_A32_006_Spec.Data\ 1.mat \
          spc.Witec/Read_Project4_data/Matlab_format/P_A32_007_Spec.Data\ 1.mat \
          spc.Witec/Read_Project4_data/Size.txt \
          spc.Witec/Read_Project4_data/Table_format/P_A32_006_Spec.Data\ 1.txt \
          spc.Witec/Read_Project4_data/Table_format/P_A32_007_Spec.Data\ 1.txt \
          spc.Witec/Witec-Map.spc \
          spc.Witec/Witec-timeseries.spc

spe =     spe/aspirin.spe  spe/HeNe.SPE spe/noise.spe spe/polystyrene.SPE \
          spe/blut1.SPE spe/blut2.SPE spe/kuvette.SPE \
          spe/blut1.spc spe/blut2.spc spe/kuvette.spc

spe.gasmet = spe.gasmet/06dvan_BS_cdst_Diesel_run3_281011_00001.SPE

sp.PerkinElmer = sp.PerkinElmer/A1701001.SP sp.PerkinElmer/A1702001.SP sp.PerkinElmer/A1703001.SP \
          sp.PerkinElmer/A1704001.SP sp.PerkinElmer/A1705001.SP sp.PerkinElmer/A1706001.SP \
          sp.PerkinElmer/A1707001.SP sp.PerkinElmer/A1708001.SP sp.PerkinElmer/A1709001.SP \
          sp.PerkinElmer/A1710001.SP sp.PerkinElmer/A1711001.SP sp.PerkinElmer/A1712001.SP \
          sp.PerkinElmer/A1713001.SP sp.PerkinElmer/A1714001.SP sp.PerkinElmer/A1715001.SP \
          sp.PerkinElmer/A1716001.SP sp.PerkinElmer/A1717001.SP sp.PerkinElmer/A1718001.SP \
          sp.PerkinElmer/A1719001.SP sp.PerkinElmer/A1720001.SP sp.PerkinElmer/A1721001.SP \
          sp.PerkinElmer/A1722001.SP sp.PerkinElmer/A1723001.SP sp.PerkinElmer/A1724001.SP \
          sp.PerkinElmer/A1725001.SP sp.PerkinElmer/A1726001.SP sp.PerkinElmer/A1727001.SP \
          sp.PerkinElmer/A1728001.SP sp.PerkinElmer/A1729001.SP sp.PerkinElmer/A1730001.SP \
          sp.PerkinElmer/A1731001.SP sp.PerkinElmer/A1732001.SP sp.PerkinElmer/A1733001.SP \
          sp.PerkinElmer/A1734001.SP sp.PerkinElmer/A1735001.SP sp.PerkinElmer/A1736001.SP \
          sp.PerkinElmer/A1737001.SP sp.PerkinElmer/A1738001.SP sp.PerkinElmer/A1739001.SP \
          sp.PerkinElmer/A1740001.SP sp.PerkinElmer/A1741001.SP sp.PerkinElmer/A1742001.SP \
          sp.PerkinElmer/A1743001.SP sp.PerkinElmer/A1744001.SP sp.PerkinElmer/A1745001.SP \
          sp.PerkinElmer/A1746001.SP sp.PerkinElmer/A1747001.SP sp.PerkinElmer/A1748001.SP \
          sp.PerkinElmer/A1749001.SP sp.PerkinElmer/A1750001.SP sp.PerkinElmer/A1751001.SP \
          sp.PerkinElmer/A1752001.SP sp.PerkinElmer/A1753001.SP sp.PerkinElmer/A7401001.SP \
          sp.PerkinElmer/A7402001.SP sp.PerkinElmer/A7403001.SP sp.PerkinElmer/A7404001.SP \
          sp.PerkinElmer/A7405001.SP sp.PerkinElmer/A7406001.SP sp.PerkinElmer/A7407001.SP \
          sp.PerkinElmer/A7408001.SP sp.PerkinElmer/A7409001.SP sp.PerkinElmer/A7410001.SP \
          sp.PerkinElmer/A7411001.SP sp.PerkinElmer/A7412001.SP sp.PerkinElmer/A7413001.SP \
          sp.PerkinElmer/A7414001.SP sp.PerkinElmer/A7415001.SP sp.PerkinElmer/A7416001.SP \
          sp.PerkinElmer/A7417001.SP sp.PerkinElmer/A7418001.SP sp.PerkinElmer/A7419001.SP \
          sp.PerkinElmer/A7420001.SP sp.PerkinElmer/A7421001.SP sp.PerkinElmer/A7422001.SP \
          sp.PerkinElmer/A7423001.SP sp.PerkinElmer/A7424001.SP sp.PerkinElmer/A7425001.SP \
          sp.PerkinElmer/A7426001.SP sp.PerkinElmer/A7427001.SP sp.PerkinElmer/A7428001.SP \
          sp.PerkinElmer/A7429001.SP sp.PerkinElmer/A7430001.SP sp.PerkinElmer/A7431001.SP \
          sp.PerkinElmer/A7432001.SP sp.PerkinElmer/A7433001.SP sp.PerkinElmer/A7434001.SP \
          sp.PerkinElmer/A7435001.SP sp.PerkinElmer/A7436001.SP sp.PerkinElmer/A7437001.SP \
          sp.PerkinElmer/A7438001.SP sp.PerkinElmer/A7439001.SP sp.PerkinElmer/A7440001.SP \
          sp.PerkinElmer/A7441001.SP sp.PerkinElmer/A7442001.SP sp.PerkinElmer/A7443001.SP \
          sp.PerkinElmer/A7444001.SP sp.PerkinElmer/A7445001.SP sp.PerkinElmer/A7446001.SP \
          sp.PerkinElmer/A7447001.SP sp.PerkinElmer/A7448001.SP sp.PerkinElmer/A7449001.SP \
          sp.PerkinElmer/A7450001.SP sp.PerkinElmer/A7451001.SP sp.PerkinElmer/A7452001.SP \
          sp.PerkinElmer/A7453001.SP sp.PerkinElmer/WT501001.SP sp.PerkinElmer/WT502001.SP \
          sp.PerkinElmer/WT503001.SP sp.PerkinElmer/WT504001.SP sp.PerkinElmer/WT505001.SP \
          sp.PerkinElmer/WT506001.SP sp.PerkinElmer/WT507001.SP sp.PerkinElmer/WT508001.SP \
          sp.PerkinElmer/WT509001.SP sp.PerkinElmer/WT510001.SP sp.PerkinElmer/WT511001.SP \
          sp.PerkinElmer/WT512001.SP sp.PerkinElmer/WT513001.SP sp.PerkinElmer/WT514001.SP \
          sp.PerkinElmer/WT515001.SP sp.PerkinElmer/WT516001.SP sp.PerkinElmer/WT517001.SP \
          sp.PerkinElmer/WT518001.SP sp.PerkinElmer/WT519001.SP sp.PerkinElmer/WT520001.SP \
          sp.PerkinElmer/WT521001.SP sp.PerkinElmer/WT522001.SP sp.PerkinElmer/WT523001.SP \
          sp.PerkinElmer/WT524001.SP sp.PerkinElmer/WT525001.SP sp.PerkinElmer/WT526001.SP \
          sp.PerkinElmer/WT527001.SP sp.PerkinElmer/WT528001.SP sp.PerkinElmer/WT529001.SP \
          sp.PerkinElmer/WT530001.SP sp.PerkinElmer/WT531001.SP sp.PerkinElmer/WT532001.SP \
          sp.PerkinElmer/WT533001.SP sp.PerkinElmer/WT534001.SP sp.PerkinElmer/WT535001.SP \
          sp.PerkinElmer/WT536001.SP sp.PerkinElmer/WT537001.SP sp.PerkinElmer/WT538001.SP \
          sp.PerkinElmer/WT539001.SP sp.PerkinElmer/WT540001.SP sp.PerkinElmer/WT541001.SP \
          sp.PerkinElmer/WT542001.SP sp.PerkinElmer/WT543001.SP sp.PerkinElmer/WT544001.SP \
          sp.PerkinElmer/WT545001.SP sp.PerkinElmer/WT546001.SP sp.PerkinElmer/WT547001.SP \
          sp.PerkinElmer/WT548001.SP sp.PerkinElmer/WT549001.SP sp.PerkinElmer/WT550001.SP \
          sp.PerkinElmer/WT551001.SP sp.PerkinElmer/WT552001.SP sp.PerkinElmer/WT553001.SP \
          sp.PerkinElmer/WT601001.SP sp.PerkinElmer/WT602001.SP sp.PerkinElmer/WT603001.SP \
          sp.PerkinElmer/WT604001.SP sp.PerkinElmer/WT605001.SP sp.PerkinElmer/WT606001.SP \
          sp.PerkinElmer/WT607001.SP sp.PerkinElmer/WT608001.SP sp.PerkinElmer/WT609001.SP \
          sp.PerkinElmer/WT610001.SP sp.PerkinElmer/WT611001.SP sp.PerkinElmer/WT612001.SP \
          sp.PerkinElmer/WT613001.SP sp.PerkinElmer/WT614001.SP sp.PerkinElmer/WT615001.SP \
          sp.PerkinElmer/WT616001.SP sp.PerkinElmer/WT617001.SP sp.PerkinElmer/WT618001.SP \
          sp.PerkinElmer/WT619001.SP sp.PerkinElmer/WT620001.SP sp.PerkinElmer/WT621001.SP \
          sp.PerkinElmer/WT622001.SP sp.PerkinElmer/WT623001.SP sp.PerkinElmer/WT624001.SP \
          sp.PerkinElmer/WT625001.SP sp.PerkinElmer/WT626001.SP sp.PerkinElmer/WT627001.SP \
          sp.PerkinElmer/WT628001.SP sp.PerkinElmer/WT629001.SP sp.PerkinElmer/WT630001.SP \
          sp.PerkinElmer/WT631001.SP sp.PerkinElmer/WT632001.SP sp.PerkinElmer/WT633001.SP \
          sp.PerkinElmer/WT634001.SP sp.PerkinElmer/WT635001.SP sp.PerkinElmer/WT636001.SP \
          sp.PerkinElmer/WT637001.SP sp.PerkinElmer/WT638001.SP sp.PerkinElmer/WT639001.SP \
          sp.PerkinElmer/WT640001.SP sp.PerkinElmer/WT641001.SP sp.PerkinElmer/WT642001.SP \
          sp.PerkinElmer/WT643001.SP sp.PerkinElmer/WT644001.SP sp.PerkinElmer/WT645001.SP \
          sp.PerkinElmer/WT646001.SP sp.PerkinElmer/WT647001.SP sp.PerkinElmer/WT648001.SP \
          sp.PerkinElmer/WT649001.SP sp.PerkinElmer/WT650001.SP sp.PerkinElmer/WT651001.SP \
          sp.PerkinElmer/WT652001.SP sp.PerkinElmer/WT653001.SP sp.PerkinElmer/WT701001.SP \
          sp.PerkinElmer/WT702001.SP sp.PerkinElmer/WT703001.SP sp.PerkinElmer/WT704001.SP \
          sp.PerkinElmer/WT705001.SP sp.PerkinElmer/WT706001.SP sp.PerkinElmer/WT707001.SP \
          sp.PerkinElmer/WT708001.SP sp.PerkinElmer/WT709001.SP sp.PerkinElmer/WT710001.SP \
          sp.PerkinElmer/WT711001.SP sp.PerkinElmer/WT712001.SP sp.PerkinElmer/WT713001.SP \
          sp.PerkinElmer/WT714001.SP sp.PerkinElmer/WT715001.SP sp.PerkinElmer/WT716001.SP \
          sp.PerkinElmer/WT717001.SP sp.PerkinElmer/WT718001.SP sp.PerkinElmer/WT719001.SP \
          sp.PerkinElmer/WT720001.SP sp.PerkinElmer/WT721001.SP sp.PerkinElmer/WT722001.SP \
          sp.PerkinElmer/WT723001.SP sp.PerkinElmer/WT724001.SP sp.PerkinElmer/WT725001.SP \
          sp.PerkinElmer/WT726001.SP sp.PerkinElmer/WT727001.SP sp.PerkinElmer/WT728001.SP \
          sp.PerkinElmer/WT729001.SP sp.PerkinElmer/WT730001.SP sp.PerkinElmer/WT731001.SP \
          sp.PerkinElmer/WT732001.SP sp.PerkinElmer/WT733001.SP sp.PerkinElmer/WT734001.SP \
          sp.PerkinElmer/WT735001.SP sp.PerkinElmer/WT736001.SP sp.PerkinElmer/WT737001.SP \
          sp.PerkinElmer/WT738001.SP sp.PerkinElmer/WT739001.SP sp.PerkinElmer/WT740001.SP \
          sp.PerkinElmer/WT741001.SP sp.PerkinElmer/WT742001.SP sp.PerkinElmer/WT743001.SP \
          sp.PerkinElmer/WT744001.SP sp.PerkinElmer/WT745001.SP sp.PerkinElmer/WT746001.SP \
          sp.PerkinElmer/WT747001.SP sp.PerkinElmer/WT748001.SP sp.PerkinElmer/WT749001.SP \
          sp.PerkinElmer/WT750001.SP sp.PerkinElmer/WT751001.SP sp.PerkinElmer/WT752001.SP \
          sp.PerkinElmer/WT753001.SP

TriVista = TriVista/aspirin.tvf

txt.HoribaJobinYvon = txt.HoribaJobinYvon/map.txt txt.HoribaJobinYvon/ts.txt

txt.Kaiser = txt.Kaiser/test-lo-4.txt

txt.PerkinElmer = txt.PerkinElmer/flu1.txt txt.PerkinElmer/flu2.txt txt.PerkinElmer/flu3.txt \
          txt.PerkinElmer/flu4.txt txt.PerkinElmer/flu5.txt txt.PerkinElmer/flu6.txt

txt.Renishaw = txt.Renishaw/chondro.bz2 txt.Renishaw/chondro.gz txt.Renishaw/chondro.lzma \
          txt.Renishaw/chondro.txt txt.Renishaw/chondro.xz txt.Renishaw/chondro.zip \
          txt.Renishaw/laser.txt.gz txt.Renishaw/paracetamol.txt

txt.t = txt.t/Triazine\ 5_31.txt

txt.Witec = txt.Witec/Witec-Map_full.txt \
          txt.Witec/Witec-Map\ (Header).txt txt.Witec/Witec-Map\ (X-Axis).txt txt.Witec/Witec-Map\ (Y-Axis).txt \
          txt.Witec/Witec-Map_label.txt txt.Witec/Witec-Map_no.txt txt.Witec/Witec-Map_unit.txt \
          txt.Witec/Witec-Map-x.dat txt.Witec/Witec-Map-y.dat \
          txt.Witec/Witec-timeseries_full.txt txt.Witec/Witec-timeseries\ (Header).txt \
          txt.Witec/Witec-timeseries\ (X-Axis).txt txt.Witec/Witec-timeseries\ (Y-Axis).txt \
          txt.Witec/Witec-timeseries_label.txt txt.Witec/Witec-timeseries_no.txt  \
          txt.Witec/Witec-timeseries_unit.txt \
          txt.Witec/Witec-timeseries-x.dat txt.Witec/Witec-timeseries-y.dat \
          txt.Witec/nofilename\ (Header).txt txt.Witec/nofilename\ (X-Axis).txt txt.Witec/nofilename\ (Y-Axis).txt

uxd = uxd/DIV1208200(1).UXD uxd/DIV1208300(1).UXD uxd/DIV1208400(1).UXD uxd/DIV1208500(1).UXD  \
          uxd/DIV1208600(1).UXD uxd/DIV1208700(1).UXD uxd/DIV1208800(1).UXD uxd/DIV1208900(1).UXD \
          uxd/DIV1209000(1).UXD uxd/DIV1214300(1).UXD uxd/DIV1214400(1).UXD uxd/DIV1214500(1).UXD \
          uxd/DIV1214600(1).UXD uxd/DMI09057A0(1).UXD uxd/DMI09111A0(1).UXD uxd/DMI12047A0(1).UXD \
          uxd/DMI12051A0(1).UXD uxd/RPE0327400(1).UXD uxd/RPE0327401(1).UXD uxd/VCA0106602(1).UXD \
          uxd/VCA0106603(1).UXD uxd/VCA0106604(1).UXD uxd/VCA0106701(1).UXD uxd/VCA0106702(1).UXD \
          uxd/VCA0106703(1).UXD

WIP.Witec = WIP.Witec/LineScans.WIP WIP.Witec/Si-data.WIP

RAWFILES = $(asc.Andor) \
           $(ENVI) \
           $(jaz) \
           $(jcamp-dx) \
           $(mat.cytospec) \
           $(mat.Witec) \
           $(NIST) \
           $(pz2) \
           $(spc) \
           $(spc.Kaisermap) \
           $(spc.LabRam) \
           $(spc.Shimadzu) \
           $(spc.Witec) \
           $(spe) \
           $(spe.gasmet) \
           $(sp.PerkinElmer) \
           $(TriVista) \
           $(txt.HoribaJobinYvon) \
           $(txt.Kaiser) \
           $(txt.PerkinElmer) \
           $(txt.Renishaw) \
           $(txt.t) \
           $(txt.Witec) \
           $(uxd) \
           $(wdf.Renishaw) \
           $(WIP.Witec)

fileio.pdf: fileio.Rnw vignettes.defs | $(RAWFILES)
	R CMD Sweave fileio.Rnw --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

$(RAWFILES):
	$(error $@ does not exist. Try running git lfs sync.)

barbiturates.rda: fileio.pdf
	touch barbiturates.rda

paracetamol.rda: fileio.pdf
	touch paracetamol.rda
