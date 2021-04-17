Replication instructions for Charnysh, Volha and Evgeny Finked. 2017. "The Death Camp Eldorado: Political and Economic Effects of Mass Violence.” APSR (forthcoming).

The replication data and code provided here can be used to replicate all the tables and figures in the main text and the supplementary online appendix. 

Contents of the replication archive is as follows:

(1) The "replication.R" script contains replication code for all tables in the paper and online appendix. 

(2) “MainDataset.RData” contains two objects, “crd” for communities before 1999 and “pol” for communities after 1999. All variables used in tables and figures in the article and most variables used in the Online Appendix are in this file.

(3) Additional files used for some tables in the appendix are included separately because they are for different units of analysis: 

(a) “election1928” contains results of the 1928 election for communities within pre-WWII administrative borders 

(b) “Migration88” contains results of the 1988 national census for communities within 70 km of Treblinka

(b) “census1946” contains results of the 1946 national census census for communities within 70 km of Treblinka for administrative divisions from 1945-46.

(d)“distMatTreblinkaMGm1990s” and “distMatTreblinkaM” contain shortest distances (“as crow flies”) from the geometric center of each community to all other communities in “MainDataset.RData” used for Figure A6 in the appendix

(e) “textStemmed.RData” contains a corpus of documents needed for text analysis in Figure A1 and Figure A2 in the appendix

To replicate the analysis, download the scripts and data structures into a local folder. Replication was tested with R version 3.3.3. The header of the R script checks for missing R packages and should install them if needed.
