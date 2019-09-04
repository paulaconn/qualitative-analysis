#
# Audits information from the qualitative and quantitative data (2) to present any
# inconsistencies. The quantitative data is taken as the ground truth for conditions.
#
# By Paula 
#

library(dplyr)
library(pdftools)
library(stringr)
library(tm)

checkData <- function(project_data, qual_data){
  # Checks if the project teams are consistent in the two qualitative data sources.
  #
  teams <- unique(qual_data$ID.1)
  missing_projects <- setdiff(teams, projects_df$ID)
  extra_projects <- setdiff(projects_df$ID, teams)
  
  if(length(missing_projects)>0 && missing_projects[1] != ""){
    cat("-----------------------------
   Missing in projects summary spreadsheet. Please Review: \n\n")
    print(missing_projects)
  }
  if(length(extra_projects)>0){
    cat("-----------------------------
        Extra projects in summary spreadsheet. Please compare to qualitative data. \n\n")
    print(extra_projects)
  }
}

checkTargetTotals <- function(project_data, qual_data){
  # Checks target market information based on two qualitative data sources
  # If there are inconsistencies, will print to CSV and on console.
  #
  total_summary <- sum(project_data$Number.of.Target.Markets)
  codes_targetMkt <- qual_data[which(qual_data$Consensus.Tag %in% 
                                       c("Target market includes a group requiring accessibility",
                                         "target market includes a group requiring accessibility")),]
  count_cTgtMkt_raw <- codes_targetMkt %>% 
    dplyr::group_by(ID.1, Final.Population.Subcategory) %>% 
    dplyr::summarise(raw_count = n())
  count_cTgtMkt <- count_cTgtMkt_raw %>% 
    dplyr::group_by(ID.1) %>% 
    dplyr::summarise(count = n())
  total_cTgtMkt <- sum(count_cTgtMkt$count)
  if (total_summary != total_cTgtMkt){
    cat("-------------------------
    Discreptancy in number of target markets:
        \nIn summary: ", total_summary,
        "\nIn qualitative data: ", total_cTgtMkt)
    
    #Check if some teams are missing. Ignore the rows that have 0 target markets. 
    summary_eligible <- project_data[which(project_data$Number.of.Target.Markets>0),]
    extra_summary <- setdiff(summary_eligible$ID, count_cTgtMkt$ID.1)
    cat("\n\nThere are ", length(extra_summary), "extra items in the summary spreadsheet. Please resolve if necessary: \n")
    print(extra_summary)
    #print(data.frame(table(summary_eligible$ID)))  #uncomment to see if duplicates
    colnames(count_cTgtMkt)[1] <- "ID"
    count_cTgtMkt <- merge(x = count_cTgtMkt, y = summary_eligible, by = "ID", all = TRUE)
    write.csv(count_cTgtMkt, "Output/qualitativeTargetMarket.csv")
    write.csv(count_cTgtMkt_raw, "Output/qualitativeTargetMarket_raw.csv")
  }
  else { 
    print('All qualitative data is consistent.')
  }
}

mergeQuantQualConditions <- function(filename, project_data){
  # Merges the conditions of the quantitative and qualitative data and checks
  # for inconsistencies.
  quantData <- read.csv(filename, sep = ",")
  quantData <- within(quantData,  ID <- paste(quantData$SEMESTER, quantData$YEAR,
                                              quantData$CLASS, quantData$INSTRUCTOR,
                                              quantData$GROUP.NAME, sep=""))
  quantData <- dplyr::filter(quantData, TERM.OF.COLLECTION != "SENIOR") 
  quantData <- dplyr::filter(quantData, INSTRUCTOR != "") %>% select(ID, Final.Condition) %>%
    group_by(ID, Final.Condition) %>% summarise(count = n())
  
  merged_conditions_df <- merge(x = project_data, y = quantData, by = "ID", all = TRUE)
  
  # Ignore teams that did not submit a survey, the conditions will be 'NA'
  merged_conditions_df <- dplyr::filter(merged_conditions_df, !is.na(Final.Condition))
  
  # Identify inconsistencies
  merged_conditions_df$Conditions.Match <- ifelse(
      as.character(merged_conditions_df$Team.Condition) == 
      as.character(merged_conditions_df$Final.Condition), "Y", "N")
  merged_conditions_df <- merged_conditions_df[-which(
    merged_conditions_df$Conditions.Match == "Y"),]
  ifelse(nrow(merged_conditions_df) > 0,
    print('ERROR: Quantitative data conditions inconsistent please review condition-comparison.csv'),
    print('All quantitative data consistent'))
  write.csv(merged_conditions_df, "Output/condition-comparison.csv")
}

openPDF <- function (folderName){
  filenames <- list.files(folderName, pattern = "*.pdf", full.names = TRUE)
  files <- suppressWarnings(lapply(filenames, pdf_text))
  return(files)
}

wordCount <- function(files, totWords){
  # Counts number of words in PDF files. Two classes from Fall 2018 are not included
  # because they had the baseline condition. Folder has 365 items
  for (txt in files){
    txt <- removePunctuation(txt)
    txt <- str_replace(gsub("\\s+", " ", str_trim(txt)), "\n", " ")
    totWords <- totWords + sum(lengths(strsplit(txt, " ")))
  }
  return(totWords)
}

###########################
# MAIN EXECUTION
###########################

projects_df <- read.csv("projects-with-id.csv", sep = ",")
codes_df <- read.csv("codes-with-id.csv", sep = ",")
projects_df_csq <- projects_df

#Remove rows containing senior or UNT
codes_df <- codes_df[which(!codes_df$CLASS %in% c("UNT","SENIORS")),]

#Ignore if they did not mention accessibility
cat("Total teams (all conditions): ", nrow(projects_df), "\n")
projects_df <- projects_df[which(projects_df$Mentioned.Accessibility. == "Y"),]
cat("Total teams mentioned accessibility: ", nrow(projects_df), "\n")

#Check summary data against raw codes and quantitative data.
checkData(projects_df, codes_df)
checkTargetTotals(projects_df, codes_df)
mergeQuantQualConditions("ISTE-SWEN-All-Data.csv", projects_df_csq)

#Run Chi-Squared Test
projects_df_csq <- projects_df_csq[! projects_df_csq$Team.Condition %in% 
                                     c("**REVIEW**","Baseline"), ]
project_freq_df <- table(droplevels(projects_df_csq$Team.Condition), 
                         projects_df_csq$Mentioned.Accessibility)
projects_df_tg <- projects_df_csq[projects_df_csq$Mentioned.Accessibility == "Y", ]
project_tg_df <- table(droplevels(projects_df_tg$Team.Condition), 
                       projects_df_tg$Chose.Target.Market)
print("Mentioned Accessibility:")
print(project_freq_df)
print(chisq.test(project_freq_df))
print("Chose target market:")
print(project_tg_df)
print(fisher.test(project_tg_df))

# Count number of words and documents
pdfFiles <- openPDF("F17-S19")
print(paste0("Total words (pdf): ", wordCount(pdfFiles,0)))