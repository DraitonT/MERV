                    Sample Input and Output Files


                     PROGRAM TEST REFERENCE CASE

inputref.txt    - commented test input file for reference case
listref.txt     - list output file for reference case

Comments at the end of the input file provide brief descriptions of
the input parameters. Most of these parameters have default values,
as specified in this input file.  Only parameters that differ from
their default values actually need to be included in the input file.
This input/list set can be used to verify proper installation and
operation of Venus-GRAM 2004.  After a successful run of Venus-GRAM with
input file inputref.txt, the LIST.txt file produced can be compared with
the listref.txt file provided.  On UNIX machines this is done with
the command

diff LIST.txt listref.txt

(in UNIX, a null response indicates no differences between the two files).

In PC-DOS, this comparison is done with command

fc LIST.txt listref.txt

Note that UNIX output includes leading zeroes in front of decimal points
(e.g. 0.12 or -0.34), while in DOS format, the leading zeroes are dropped
(e.g.  .12 or  -.34).  A text editor can be used to insert (or remove)
these leading zeroes, as necessary for purposes of doing the file
comparison.  Because of machine-dependent round-off differences, a few
numbers may differ (typically no more than one number in the last
significant digit displayed) between the LIST.txt file produced on the
user's machine and the listref.txt reference file provided.


