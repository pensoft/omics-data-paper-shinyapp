# Omics Data Paper Generator

This repository contains all code used to create the Omics data paper R shiny app: https://mdmtrv.shinyapps.io/Omics_data_paper/

The Omics Data Paper Generator app emulates the functionality of the automatic workflow for streamlined import of ENA metadata into manuscript integrated inside Pensoft's ARPHA Writing Tool.

## Instructions
1. Enter a Study or Project ID from the European Nucleotide Archive (ENA) into the text field.
2. Click the "Convert" button and the process of metadata retrieval and import will begin.
3. Once the metadata has been imported into the omics data paper manuscript template, you can download the results as HTML or as JATS XML by clicking the relevant buttons.
4. In addition, you can browse or download a MIxS checklist of BioSamples data, if such data exists for the records you have imported. The checklist is imported separately as a Supplementary file in CSV format. You can browse it as a data frame object in the end of the webpage or you can download it by clicking the "Download Supplementary Material" button.

## Reproducibility
We have included a directory called *testing* which contains a text file with input data (en example Sample ID) as well as 3 other files which are generated as output when the application is run with the given Sample ID. You can compare the files you obtain when running the application with this ID to the ones we have provided. 

## Comments and suggestions
If you have any comments or suggestions about the R Shiny app or the workflow, you can contact us at semanticpublishing@pensoft.net

