# qualitative-analysis
Qualitative analysis summary of results &amp; hypothesis testing. This file audits all qualitative and quantitative data to present inconsistencies. The quantitative data is taken as ground truth for the conditions assignment.

## Files
* qual-analysis.r: Script completes the following tasks:
1. Audits information to present inconsistencies in the data.
2. Runs Chi-squared tests to determine whether teaching conditions are related to teams mention of accessibility
3. Runs a Fisher Test to identify whether there is a relationship between the conditions and teams' choice of end users.
3. Counts the number of words in the PDF files (for description of qualitative coding).

## Requirements
* R with dplyr, pdftools, stringr, and tm installed.

The following files are not included to protect participants' privacy.
* ISTE-SWEN-All-Data.csv: All quantitative data
* projects-with-id.csv: Second tab in the qualitative coding spreadsheet.
* codes-with-id.csv: First tab in the qualitative coding spreadsheet.
* Output: Directory where the script will write any information to, if needed.

## Notes:
There are still a few minor inconsistencies in the quantitative data (typos in professors' names) that have not been resolved with the other qualitative data. This does not effect analysis because they are teams that did not have any mentions in their projects. However, it would be good to make these more consistent in the future.
