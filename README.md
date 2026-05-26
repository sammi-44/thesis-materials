# Contents of thesis-materials GitHub 

*Note: All files are named based on row numbers, following the scheme r#1, r#2, etc. based on Table?5, originally in Section 3.2 of the thesis.*

## Monte Carlo R Simulations
This folder includes the files of R-code scripts for each row number. The important parts of the script are detailed in Section 3.4 of the thesis. The script for row #4 is given in full in Appendix 1; the rest of the scripts follow this general template.

## Praat Files
### *This folder includes all of the inputs and outputs of the Praat Program*
This folder contains input files, including the input OT grammars *r#_ot.txt*, input pair distributions *r#_pair_distribution*, learned grammar *r#_ot_learned*, tables showing a snapshot of disharmonies and tableaus *r#_table*, and finally output checks *r#_learned_out*.\
\
Direct examples of these files can be found in section 4.2.2 of the theses. These examples are for row #2, particularly, but each file follows the same general outline as what is seen. *r#2_ot.txt* matches with Figure?1, *r#2_pair_distribution.txt* matches with Figure?2, *r#2_table.png* matches with Table?9 and Tableau?1. The learned grammar downloaded from Praat corresponds to *r#2_ot_learned*, then there is another file *r#2_ot_learned_out* that is an output check of this learned grammar.\
\
The remainder of the visualizations of the "table" files, or the visualizations of the learned OT grammar, which includes ranking values and disharmonies along with relevant tableaus, is present in Appendix 6.

## Stochastic Representations
This folder includes code scripts that are relevant for understanding the stochastic analysis. The *r#_plot* files are just images of the plots that show the normal distribution of each constraint ranking based on the mean (ranking value) given by Praat. The *r_plot_code* files are simply the scripts that were used to generate these images. And finally, the *r#_word_phigher* files are scripts used to calculate how often certain constraints win out against each other. The visualizations of these files can be seen in Section 4.3 and in Appendix 7.
