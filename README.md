# Omics Data Paper Generator

This repository contains all code used to create the Omics data paper R shiny app: https://mdmtrv.shinyapps.io/Omics_data_paper/

The Omics Data Paper Generator app emulates the functionality of the automatic workflow for streamlined import of ENA metadata into manuscript integrated inside Pensoft's ARPHA Writing Tool.

## Requirements for running the application locally
- R 3.0.1+
- RStudio Desktop 1.3.1093

## Instructions for running the application locally
1. Clone this repository with git clone https://github.com/pensoft/omics-data-paper-shinyapp.git.
2. Open app.R with RStudio and click 'Run app' to run the shiny application in a separate window or your browser.  
3. In the User Interface of the app, enter a Study or Project ID from the European Nucleotide Archive (ENA) into the text field. You can also try running the app with the identifier we have preloaded.
4. Click the "Convert" button and the process of metadata retrieval and import will begin.
5. Once the metadata has been imported into the omics data paper manuscript template, you can download the results as HTML or as JATS XML by clicking the relevant buttons.
6. In addition, you can browse or download a MIxS checklist of BioSamples data, if such data exists for the records you have imported. The checklist is imported separately as a Supplementary file in CSV format. You can browse it as a data frame object in the end of the webpage or you can download it by clicking the "Download Supplementary Material" button.

## Reproducibility
We have included a directory called *testing* which contains a text file with input data (en example Sample ID) as well as 3 other files which are generated as output when the application is run with the given Sample ID. You can compare the files you obtain when running the application with this ID to the ones we have provided. 

## Comments and suggestions
If you have any comments or suggestions about the R Shiny app or the workflow, you can contact us at semanticpublishing@pensoft.net

