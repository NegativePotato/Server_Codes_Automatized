make_excel_files_for_server_codes <- function(sessions.df, batteries.df, nonbatteries.df) {
  
  df_list <- make_session_tab(sessions.df)
  Session_tab.df <- df_list$session_tab.df
  
  Protocol_tab.df <- make_protocol_tab(Session_tab.df)
  
  ConditionMatrix_tab.df <- make_condition_tab(Session_tab.df)
  Conditions_tab.df <- make_conditionmatrix_tab(Session_tab.df)
  
  list_of_elements.df <- df_list$list_of_elements.df
  Batteries_tab.df <- make_batteries_tab(list_of_elements.df, batteries.df)
  
  NonBatteries_tab.df <- make_nonbatteries_tab(list_of_elements.df, nonbatteries.df)
  
  BlessedBatteries_tab.df <- make_blessedbatteries_tab(Session_tab.df)
  
  
  
  
  for (server_code in unique(Session_tab.df$`Server Code`)) {
    
    # Create a new workbook
    wb <- createWorkbook()
    
    # Define a bold style for the header
    header_style <- createStyle(textDecoration = "bold")
    first_col_style <- createStyle(textDecoration = "bold")
    
    # Add sheets and write data
    add_bold_header_sheet(wb = wb, sheet_name = "ConditionMatrix", 
                          data = ConditionMatrix_tab.df %>% filter(`Server Code` == server_code) %>% select(-`Server Code`), 
                          bold = "header")
    add_bold_header_sheet(wb = wb, sheet_name = "Conditions", 
                          data = Conditions_tab.df %>% filter(`Server Code` == server_code) %>% select(-`Server Code`),
                          bold = "header")
    add_bold_header_sheet(wb = wb, sheet_name = "Protocol", 
                          data = Protocol_tab.df %>% filter(`Server Code` == server_code) %>% select(-`Server Code`), 
                          bold = "header")
    Session_tab_curr.df <- Session_tab.df %>% filter(`Server Code` == server_code) %>% select(-`Server Code`)
    names(Session_tab_curr.df)[1] <- ""
    add_bold_header_sheet(wb = wb, sheet_name = "Session", 
                          data = Session_tab_curr.df, 
                          bold = "col1")
    add_bold_header_sheet(wb = wb, sheet_name = "Batteries", 
                          data = Batteries_tab.df %>% filter(`Server Code` == server_code) %>% select(-`Server Code`), 
                          bold = "header")
    add_bold_header_sheet(wb = wb, sheet_name = "NonBatteries", 
                          data = NonBatteries_tab.df %>% filter(`Server Code` == server_code) %>% select(-`Server Code`), 
                          bold = "header")
    add_bold_header_sheet(wb = wb, sheet_name = "BlessedBatteries", 
                          data = BlessedBatteries_tab.df %>% filter(`Server Code` == server_code) %>% select(-`Server Code`), 
                          bold = "header")

    # Save workbook
    saveWorkbook(wb, file.path(here(), "Tables", "Output", sprintf("%s.xlsx", server_code)), overwrite = TRUE)
  }  
}

make_excel_files_for_server_codes_wircs <- function(server_code_info.df, 
                                                    batteries.df, 
                                                    server_code_table.df, 
                                                    progress_bar = NULL) {
  
  if(!is.null(progress_bar)) {
    progress_bar$tick(0)
  }
  
  current_server_code <- server_code_info.df$server_code
  
  # Session Tab
  Session_tab.df <- make_individual_session_tab(server_code_info.df$`Task Orders`, 
                                                server_code_info.df$`Set Orders`, 
                                                server_code_info.df$`Expectation Conditions`, 
                                                server_code_info.df$Voice, 
                                                server_code_info.df$`Traning Conditions`, 
                                                server_code_table.df)
  
  ## Create the tab dataframes
  # Batteries Tab 
  list_of_elements <- server_code_table_long.df %>% 
    filter(session_name %in% names(Session_tab.df)) %>% 
    filter(task_number > 0) %>% 
    select(server_code) %>% 
    unique() %>% 
    .$server_code

  Batteries_tab.df <- batteries.df %>% 
    filter(`Server Code` %in% list_of_elements) %>% 
    select(`Server Code`, isBlessed, Notes) %>% 
    rename(Name = `Server Code`) %>%   
    mutate(`Battery ID` = row_number()) %>% 
    relocate(`Battery ID`) 
  
  # Protocol Tab 
  sessions_for_protocol <- names(Session_tab.df %>% select(-col_headers))
  Protocol_tab.df <- tibble(
    `Proto-id` = c(current_server_code, 
                   rep("", length(sessions_for_protocol)-1)),
    `Session-id` = sessions_for_protocol, 
    `Count`	= 1,
    `Notes` = Session_tab.df %>%
      filter(col_headers == "Notes") %>% 
      select(-col_headers) %>% 
      unlist(use.names = F)
  )
    
  # Condition tab
  Conditions_tab.df <- tibble(
    `Condition ID` = 'Condition1',
    `Protocol ID` = current_server_code, 
    `Require Max Volume [B]` = "TRUE"
  )
  
  # Condition Matrix tab
  n_rows <- 6 # Totally Arbitrary Number
  ConditionMatrix_tab.df <- tibble(
    `Condition Matrix ID` = rep("", n_rows),
    `Parent ID` = rep("", n_rows),
    `Condition ID` = rep("", n_rows),
    `Weight` = rep("", n_rows),
    `Limit` = rep("", n_rows),
    `Meta` = rep("", n_rows)
  )
  
  # Non-Batteries tab
  n_rows <- 6
  NonBatteries_tab.df <- tibble(
    `Non Battery ID` = rep("", n_rows),
    `Name` = rep("", n_rows),
    `Notes` = rep("", n_rows)
  )
  
  # Non-Batteries tab
  n_rows <- 6
  BlessedBatteries_tab.df <- tibble(
    `Blessed Battery ID` = rep("", n_rows),
    `Name` = rep("", n_rows),
    `Notes` = rep("", n_rows)
  )
  
  
  ## Save the Excel file
  # Create a new workbook
  wb <- createWorkbook()
  
  # Define a bold style for the header
  header_style <- createStyle(textDecoration = "bold")
  first_col_style <- createStyle(textDecoration = "bold")
  
  # Add sheets and write data
  add_bold_header_sheet(wb = wb, sheet_name = "ConditionMatrix", 
                        data = ConditionMatrix_tab.df, 
                        bold = "header")
  add_bold_header_sheet(wb = wb, sheet_name = "Conditions", 
                        data = Conditions_tab.df,
                        bold = "header")
  add_bold_header_sheet(wb = wb, sheet_name = "Protocol", 
                        data = Protocol_tab.df, 
                        bold = "header")
  Session_tab_curr.df <- Session_tab.df
  names(Session_tab_curr.df)[1] <- ""
  add_bold_header_sheet(wb = wb, sheet_name = "Session", 
                        data = Session_tab_curr.df, 
                        bold = "col1")
  add_bold_header_sheet(wb = wb, sheet_name = "Batteries", 
                        data = Batteries_tab.df, 
                        bold = "header")
  add_bold_header_sheet(wb = wb, sheet_name = "NonBatteries", 
                        data = NonBatteries_tab.df, 
                        bold = "header")
  add_bold_header_sheet(wb = wb, sheet_name = "BlessedBatteries", 
                        data = BlessedBatteries_tab.df, 
                        bold = "header")
  
  # Save workbook
  saveWorkbook(wb, file.path(here(), "Tables", "Output", sprintf("%s.xlsx", current_server_code)), overwrite = TRUE)
  
  if(!is.null(progress_bar)) {
    progress_bar$tick()
  }
}

<<<<<<< HEAD
=======
make_excel_file_all_conditions_wircs <- function(server_codes.df,
                                                 batteries.df, 
                                                 subbateries.df,
                                                 server_code_table.df, 
                                                 n_per_condition.df, 
                                                 output_file_name) {
  
  # Tabs independant of conditions
  # Session Tab
  Session_tab.df <- server_code_table.df
  
 
  ## Create the tab dataframes
  # Batteries Tab
  subbateries_to_keep.df <- subbateries.df %>% 
    filter(Parent_Battery %in% batteries.df$`Server Code`) %>%
    mutate(Notes = batteries.df %>% filter(`Server Code` %in% Child_Battery) %>% .$Notes) %>% 
    rename(`Server Code` = Child_Battery) %>% 
    mutate(Name = `Server Code`) %>% 
    mutate(`Battery ID` = row_number()) %>% 
    relocate(`Battery ID`) %>%
    relocate(Notes, .after = everything()) %>%
    select(-Parent_Battery) %>% 
    filter(!(`Server Code` %in% batteries.df$`Server Code`))
  
  Batteries_tab.df <- batteries.df %>% 
    select(`Server Code`, isBlessed, Notes) %>% 
    rename(Name = `Server Code`) %>%
    bind_rows(subbateries_to_keep.df) %>% 
    mutate(`Battery ID` = row_number()) %>% 
    relocate(`Battery ID`)
  
  # Non-Batteries tab
  n_rows <- 6
  NonBatteries_tab.df <- tibble(
    `Non Battery ID` = rep("", n_rows),
    `Name` = rep("", n_rows),
    `Notes` = rep("", n_rows)
  )
  
  # Blessed Batteries tab
  n_rows <- 6
  BlessedBatteries_tab.df <- tibble(
    `Blessed Battery ID` = rep("", n_rows),
    `Name` = rep("", n_rows),
    `Notes` = rep("", n_rows)
  )
  
  # Condition Matrix tab
  ConditionMatrix_tab.df <- 
    make_conditionmatrix_tab_wircs(server_codes.df, 
                                   n_per_condition.df) 
  
  # Conditions tab
  Conditions_tab.df <- tibble(
    `Condition ID` = ConditionMatrix_tab.df %>% filter(`Condition ID` != "") %>% .$`Condition ID`,
    `Protocol ID` = ConditionMatrix_tab.df %>% filter(`Condition ID` != "") %>% .$`Condition ID`, 
    `Require Max Volume [B]` = 0
  )
  
  # Get the session list
  Protocol_tab.df <- server_codes.df %>% 
    group_by(server_code) %>% 
    reframe(`Session-id` = get_sessions_names(`Task Orders`, 
                                              `Set Orders`, 
                                              `Expectation Conditions`, 
                                              Voice, 
                                              `Traning Conditions`, 
                                              server_code_table.df),
            Count = 1, 
            Notes = "",
            .keep = T) %>% 
    group_by(server_code) %>% 
    mutate(`Proto-id` = ifelse(row_number() == 1, server_code, "")) %>% 
    relocate(`Proto-id`) %>% 
    ungroup() %>% 
    select(-server_code, -.keep)
  
  ## Save the Excel file
  # Create a new workbook
  wb <- createWorkbook()
  
  # Define a bold style for the header
  header_style <- createStyle(textDecoration = "bold")
  first_col_style <- createStyle(textDecoration = "bold")
  
  # Add sheets and write data
  add_bold_header_sheet(wb = wb, sheet_name = "ConditionMatrix", 
                        data = ConditionMatrix_tab.df, 
                        bold = "header", 
                        bgfill = "matrix_condition_style")
  add_bold_header_sheet(wb = wb, sheet_name = "Conditions", 
                        data = Conditions_tab.df,
                        bold = "header")
  add_bold_header_sheet(wb = wb, sheet_name = "Protocol", 
                        data = Protocol_tab.df, 
                        bold = "header")
  Session_tab_curr.df <- Session_tab.df
  names(Session_tab_curr.df)[1] <- ""
  add_bold_header_sheet(wb = wb, sheet_name = "Session", 
                        data = Session_tab_curr.df, 
                        bold = "col1")
  add_bold_header_sheet(wb = wb, sheet_name = "Batteries", 
                        data = Batteries_tab.df, 
                        bold = "header")
  add_bold_header_sheet(wb = wb, sheet_name = "NonBatteries", 
                        data = NonBatteries_tab.df, 
                        bold = "header")
  add_bold_header_sheet(wb = wb, sheet_name = "BlessedBatteries", 
                        data = BlessedBatteries_tab.df, 
                        bold = "header")
  
  # Save workbook
  saveWorkbook(wb, file.path(here(), "Tables", "Output", output_file_name), 
               overwrite = TRUE)
  
  return(list(ConditionMatrix = ConditionMatrix_tab.df, 
              Conditions = Conditions_tab.df, 
              Protocol = Protocol_tab.df, 
              Session = Session_tab.df, 
              Batteries = Batteries_tab.df, 
              NonBatteries_tab = NonBatteries_tab.df, 
              BlessedBatteries = BlessedBatteries_tab.df))
}
>>>>>>> 737c424 (now includes questionnaires)

add_bold_header_sheet <- function(wb, sheet_name, data, bold = "") {
  bold_header <- createStyle(textDecoration = "bold")  # Bold for header row
  bold_first_col <- createStyle(textDecoration = "bold")  # Bold for first column
  
  addWorksheet(wb, sheet_name)

  # Write data
  writeData(wb, sheet_name, data, withFilter = TRUE)
  
  if(bold %in% c("header", "both")) {
    # Apply bold to header row (row 1, all columns)
    addStyle(wb, sheet_name, bold_header, rows = 1,
             cols = 1:ncol(data), gridExpand = TRUE)  # Apply bold to header row
  } 
  if(bold %in% c("col1", "both")) {
    # Apply bold to first column (all rows in column 1)
    addStyle(wb, sheet_name, bold_first_col, cols = 1,
             rows = 1:nrow(data)+1, gridExpand = TRUE)  # Apply bold to header row
  }
<<<<<<< HEAD
=======
  if(bgfill == "matrix_condition_style") {
    blue_rows = which(data$`Parent ID` == "")
    # Apply blue color to first cells in condition rows 
    addStyle(wb, sheet_name, blue_fill, cols = 1,
             rows = blue_rows+1, gridExpand = TRUE) 
  }
>>>>>>> 737c424 (now includes questionnaires)
}

# Function to save dataframe to Excel with formatting
save_combinations_to_excel <- function(data, file_path, sheet_name, file_name_suffix) {
  
  if (file.exists(file_path)) {
    wb <- loadWorkbook(file_path)
  } else {
    wb <- createWorkbook()
  }
  
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  
  addWorksheet(wb, sheet_name)
  
  # Write data to sheet
  writeData(wb, sheet_name, data, startRow = 1)
  
  # Apply bold style to header
  header_style <- createStyle(textDecoration = "bold", border = "Bottom", borderColour = "black")
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  # Add black border after the last row of each group
  group_columns <- c("Age", "Training Location", "Traning Conditions", "Expectation Conditions")
  # ata <- data %>% arrange(across(all_of(group_columns)))
  last_rows <- data %>% group_by(across(all_of(group_columns))) %>% summarise(last_row = n(), .groups = "drop")
  
  for (row in cumsum(last_rows$last_row)) {
    border_style <- createStyle(border = "Bottom", borderColour = "black")
    addStyle(wb, sheet_name, border_style, rows = row + 1, cols = 1:ncol(data), gridExpand = TRUE)
  }
  
  # Save workbook
  saveWorkbook(wb, gsub(".xlsx", sprintf("%s.xlsx", file_name_suffix), file_path), overwrite = TRUE)
}
