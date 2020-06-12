% README.TXT to accompany INCA 1.41
%
% Author : Claus A. Andersson
% E-mail : claus@andersson.dk

A. DESCRIPTION
INCA is a Matlab 5.2 compatible GUI-based program that reads in files from the Perkin Elmer LS50B spectrofluorometers and spectra from the TIDAS instruments as converted by the TIDAS-to-MAT file conversion utility. If the datastructure is defined to be 'three-way', the array can be used DIRECTLY in the 'N-WAY TOOLBOX' with all necesary helper arrays defined automatically. Features are: Resizeable window sizes, features for selecting emission axis density and wider ranges on first and second order removal of Rayleigh peaks. 


B. INSTALLATION
1. Remove any old version of INCA as this will disrupt the function of the new INCA program. Type 'help inca' to see if you have another version installed.
2. Unzip the self-extracting zipped archieve (inca_141.exe) to any desired place (likely to be under the MATLAB 'toolbox' directory). The file will unzip into 8 M-files.
3. Update your MATLAB path, i.e., write 'pathtool' on the prompt or click on the path icon in the top of the command window. Remember to save the new path.
4. Note that two helper-files are included; 'nshift.m' and 'ncollapse.m'.


C. HANDS-ON EXAMPLE
1. Check that INCA is properly installed by typing 'help inca' in the MATLAB command prompt.

2. If not already done, download the INCA testdata set from 'http://www.models.kvl.dk/source/inca'. Extract the SP-files to a subdirectory under the 'INCA' directory, e.g., '..\inca\testdata'.

3. In the MATLAb command window, move to the folder holding the data.

4. Write '[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca('*.sp');' in the command window. This will show all the SP-files in the current directory. 

5. We wish to import the three landscapes which start by 'Wt' in their names. Select them with the mouse. (We could have done the same in a simpler manner by typing '[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca('wt.sp'); in the prompt in step 4.

6. Since we have more than one landscape we have to choose 'Unfolded three-way structure with Rayleigh signals set to NaN' in the dialog-box below.

7. If you know the data well you can use the other controls to adjust the conversion to your particular measurement conditions.

8. Press the 'OK' button to import the files.

9. Write 'DimX' to see the dimension of the unfolded structure in 'X'. It should be [3 311 53], designating 3 landscapes of 311 emission wavelenghts and 53 excitation wavelengths.

10. Type the other variables, in particular 'Axis1' and 'Axis2' which are the actual wavelength axes. Also note that 'DataInfo' holds ALL the parameters you specified - svaed them to be able to reconstruct your import. If you write 'DataInfo.FileNames' you can get all the filenames that have been imported.


D. SUPPORT
There is no DIRECT support. However, e-mail all details of the problems that might occur. Mail them to 'claus@andersson.dk' and I will get back a.s.a.p. 


E. COPYRIGHT
INCA version 1.41 is free. The programmer cannot be held responsible for any direct or indirect effects that may be inflicted by using this code. If the user is not able to evaluate the correctness of the output of this program or incapable of correctly evaluating the obtained data, he/she should not use the code. The code must not be compiled, modified or in any way changed without prior notice to the author. If any files in the collection are distributed, this file ('readme.txt') MUST accompany any such files.
