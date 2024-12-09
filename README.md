## daGOAT model 

README


1. SYSTEM REQUIREMENTS

Operating system that has been tested on： Microsoft Windows 10

Software version that has been tested on： R version 4.3.1 (2023-06-16)

Required R libraries: readxl ==1.4.3 e1071 == 1.7-14


2. INSTALLATION GUIDE

Not applicable.


3. INSTRUCTIONS

Mock patient data consist of "adult-stationary.csv" (peritransplant features) and 
"adult-dynamic.csv" (dynamic co-variates) in the "test" folder.  Format requirements
for these two .csv files are detailed in "format-of-peritransplant-features.xlsx" and
"format-of-dynamic-covariates.xlsx".

Make sure "repository.R", "preset", and "test" are in the "src/" folder.

Modify line 3 of "repository.R" to set the root path to "src/".

Run "repository.R".

daGOAT-computed risk-stratification from +17 to +23 days posttransplant will be 
stored in the list "res.3" and can be accessed using "res.3$risk_level[17 : 23]".  
Using the provided mock patient data, it will be "Low-risk" from +17 to +20 days, "Intermediate-risk" from +21 to +22 days and "High-risk" at +23 day .


4. POSTSCRIPT

Total expected run time: No more than a couple of seconds.
