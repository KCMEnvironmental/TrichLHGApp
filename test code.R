## TEST SCRIPT

## UPLOAD DATA FILE: 

# summarize sample IDs based on tab names
sampleID_list <- readxl::excel_sheets("Protocol 2022-441-001 report - draft #1.xlsx") 
sampleID_list <- SampleID_list[SampleID_list %in% c("Otolith Analysis","Sample Set Information", "COC", "Sample Notes") == FALSE]

# dummy place holder for for()loop variable
tab <- "Female 3"

# read in data, skip omit 2 and 3, renames length column 
data <- read_excel("Protocol 2022-441-001 report - draft #1.xlsx", sheet = tab)[-c(1:2),] %>% 
  rename("Length (µm)" = "Parameter") %>% 
  mutate(`Length (µm)` = as.numeric(`Length (µm)`))

# dummy place holder for summarize option 
sumnum <- 10

# summarize data by number of rows 
data_sum <- data %>% rowid_to_column("RowID") %>% 
  mutate(ints = cut(RowID, breaks = seq(min(`Length (µm)`), nrow(data), sumnum), include.lowest = TRUE)) %>% 
  group_by(ints) %>% 
  summarise(across(2:last_col(), median, .names = "{.col}_median"))

# summarize data by length interval
data_sum <- data %>% 
  mutate(ints = cut(`Length (µm)`, breaks = seq(min(`Length (µm)`), nrow(data), sumnum), include.lowest = TRUE)) %>% 
  group_by(ints) %>% 
  summarise(across(everything(), median, .names = "{.col}_median"))


file_output <- createWorkbook()

file_list <- list()

for (i in sampleID_list) {   
  
  data <- read_excel("Protocol 2022-441-001 report - draft #1.xlsx", sheet = i)[-c(1:2),] %>% 
    rename("Length (µm)" = "Parameter") %>% 
    mutate(`Length (µm)` = as.numeric(`Length (µm)`)) %>% 
    mutate(ints = cut(`Length (µm)`, 
                      breaks = seq(min(`Length (µm)`), 
                                   nrow(.), 10),
                      include.lowest = TRUE)) %>% 
    group_by(ints) %>% 
    summarise(across(everything(), median))
  
  addWorksheet(file_output, sheetName = i)
  writeData(file_output, sheet = i, x = data)
  
  file_list <- append(file_list, data)
  
}

saveWorkbook(file_output, "addWorksheetExample.xlsx", overwrite = TRUE)

## add each iteration to file 

## set up web app :) 


