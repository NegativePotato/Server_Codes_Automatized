
make_session_tab <- function(df) {
  list_of_elements.df <- df %>% 
    select(`Server Code`, contains('element')) %>% 
    tidyr::pivot_longer(cols = contains('element'), 
                        names_to = "element_number",
                        names_prefix = "element_",
                        values_to = "element_name"
    ) %>% 
    group_by(`Server Code`) %>% 
    mutate(session_idx = 1+ cumsum(element_name == "<LOCK>"), 
           # element_name = ifelse(element_name == "<LOCK>", 
           #                       sprintf("password%i", session_idx-1), 
           #                       element_name), 
           session_name = sprintf("Session-%03i", session_idx)) %>% 
    filter(element_name != "<LOCK>") %>% 
    select(-session_idx) %>%
    group_by(`Server Code`, session_name) %>%
    mutate(element_number = seq(n())) 
  
  
  # Create additional rows for each group
  # end_screen_names <- c("end_screen_pretest", 
  #                       "end_screen_training1", 
  #                       "end_screen_midtest", 
  #                       "end_screen_training2", 
  #                       "end_screen_posttest")
  notes <- c("Pre-Test", 	
             "Training 1",	
             "Mid-Test",
             "Training 2",	
             "Post-Test")
  # Get unique session names in order and map end_screen_names to them
  session_order <- unique(list_of_elements.df$session_name)
  # session_mapping_screen_names <- setNames(end_screen_names[1:length(session_order)], session_order)
  session_mapping_notes <- setNames(notes[1:length(session_order)], session_order)
  
  # Create additional rows by mapping session_name to the corresponding end_screen_name
  # new_rows_screen_names <- list_of_elements.df %>%
  #   distinct(`Server Code`, session_name) %>%
  #   mutate(element_name = session_mapping_screen_names[session_name],
  #          element_number = 999) %>%
  #   tidyr::drop_na()
  
  new_rows_notes <- list_of_elements.df %>%
    distinct(`Server Code`, session_name) %>%
    mutate(element_name = session_mapping_notes[session_name],
           element_number = 0) %>%
    tidyr::drop_na()
  
  # Bind new rows to the original dataframe
  list_of_elements.df <- list_of_elements.df %>%
    # bind_rows(new_rows_screen_names) %>%
    bind_rows(new_rows_notes) %>%
    arrange(`Server Code`, session_name, element_number) 
  # mutate(element_number = replace(element_number, element_number == 999, NA)) %>%
  # mutate(element_number = ifelse(is.na(element_number), lag(element_number, default = first(na.omit(element_number))) + 1, element_number))  
  
  session_tab.df <- list_of_elements.df %>% 
    tidyr::pivot_wider(names_from = session_name, 
                       values_from = element_name, 
                       values_fill = NA) %>% 
    group_by(`Server Code`) %>% 
    mutate(Session = c("Notes", "Elements", rep("", n()-2))) %>% 
    ungroup() %>% 
    select(-element_number) %>% 
    relocate(Session, .after = `Server Code`)
  
  return(list(session_tab.df = session_tab.df, 
              list_of_elements.df = list_of_elements.df %>% filter(element_number > 0)))
}

# Make the session tab for each server code 
make_individual_session_tab <- function(order, set_order, induction_condition, 
                                        voice, training_task, server_code_table.df) {
  cols <- names(server_code_table.df)
  ee_version <- ifelse(induction_condition %in% c("PP","PN"), "Placebo", "Nocebo")
  al_version <- ifelse(induction_condition %in% c("PP","NP"), "Placebo", "Nocebo")
  sessions <- list()
  
  pattern_pre1 <- paste0("(Pre_Test-D1)(?=.*O", order, ")(?=.*", set_order, ")")
  sessions$pre1 <- grep(pattern_pre1, cols, value = TRUE, perl = TRUE, ignore.case = T)
  
<<<<<<< HEAD
  pattern_pre2 <- paste0("(Pre_Test-D2)(?=.*O", order, ")(?=.*", set_order, ")(?=.*", ee_version, ")(?=.*-", voice, ")")
  sessions_pre2 <- grep(pattern_pre2, cols, value = TRUE, perl = TRUE, ignore.case = T)
  
  pattern_training1 <- paste0(gsub("( |-)", "", capwords(training_task)), ".*D([0-9]|10)$")
  sessions_training1 <- grep(pattern_training1, cols, value = TRUE, perl = TRUE, ignore.case = T)
=======
  if (induction_condition == "N/A") {
    pattern_pre2 <- paste0("(Pre_Test-D2)(?=.*O", order, ")(?=.*", set_order, ")(?=.*", "Neutral", ")(?=.*-", voice, ")")
  } else {
    pattern_pre2 <- paste0("(Pre_Test-D2)(?=.*O", order, ")(?=.*", set_order, ")(?=.*", ee_version, ")(?=.*-", voice, ")")
  }
  sessions$pre2 <- grep(pattern_pre2, cols, value = TRUE, perl = TRUE, ignore.case = T)
  
  if (induction_condition != "N/A") {
    pattern_training1 <- paste0("_", training_condition_to_camel_case(training_task), ".*D([0-9]|10)$")
    sessions$training1 <- grep(pattern_training1, cols, value = TRUE, perl = TRUE, ignore.case = T)
  }
>>>>>>> 737c424 (now includes questionnaires)
  
  pattern_mid <- paste0("(Mid_Test)(?=.*O", order, ")(?=.*", set_order, ")(?=.*", al_version, ")")
  sessions$mid <- grep(pattern_mid, cols, value = TRUE, perl = TRUE, ignore.case = T)
  
<<<<<<< HEAD
  pattern_training2 <- paste0(gsub("( |-)", "", capwords(training_task)), ".*D(([1-9][1-9])|[2-9]0)")
  sessions_training2 <- grep(pattern_training2, cols, value = TRUE, perl = TRUE, ignore.case = T)  
  
=======
  if (induction_condition != "N/A") {
    pattern_training2 <- paste0("_", gsub("( |-)", "", capwords(training_task)), ".*D(([1-9][1-9])|[2-9]0)")
    sessions$training2 <- grep(pattern_training2, cols, value = TRUE, perl = TRUE, ignore.case = T)  
  }

>>>>>>> 737c424 (now includes questionnaires)
  pattern_post <- paste0("(Post_Test)(?=.*O", order, ")(?=.*", set_order, ")")
  sessions$post <- grep(pattern_post, cols, value = TRUE, perl = TRUE, ignore.case = T)
  
  sessions_array <- unlist(sessions)
  
<<<<<<< HEAD
=======
  return(sessions_array)
}

make_individual_session_tab <- function(order, set_order, induction_condition, 
                                        voice, training_task, server_code_table.df) {
  
  sessions <- get_sessions_names(order, set_order, induction_condition, 
                                 voice, training_task, server_code_table.df)
  
>>>>>>> 737c424 (now includes questionnaires)
  output.df <- server_code_table.df %>%
    select(all_of(c(cols[1], sessions)))
  
  return(output.df)
}


make_batteries_tab <- function(list_of_elements.df, batteries.df) {
  batteries_cleaned.df <- batteries.df %>%
    select(`File name`, isBlessed, Notes) %>% 
    rename(Name = `File name`)
  
  batteries_tab.df <- list_of_elements.df %>% 
    group_by(`Server Code`) %>% 
    dplyr::reframe(batteries_cleaned.df %>% 
                     filter(Name %in% element_name)) %>% 
    group_by(`Server Code`) %>% 
    mutate(`Battery ID` = seq(n())) %>% 
    relocate(`Battery ID`, .after = `Server Code`) %>% 
    ungroup()
  
  return(batteries_tab.df)
}

make_nonbatteries_tab <- function(list_of_elements.df, nonbatteries.df) {
  
  nonbatteries_tab.df <- list_of_elements.df %>% 
    group_by(`Server Code`) %>% 
    dplyr::reframe(nonbatteries.df %>% 
                     filter(Name %in% element_name)) %>% 
    group_by(`Server Code`) %>% 
    mutate(`Non Battery ID` = 1000+seq(n())) %>% 
    relocate(`Non Battery ID`, .after = `Server Code`) %>% 
    ungroup()
  
  return(nonbatteries_tab.df)
}

make_protocol_tab <- function(Session_tab.df) {
  
  protocol_tab.df <- Session_tab.df %>% 
    filter(Session == "Notes") %>% 
    select(-Session) %>% 
    tidyr::pivot_longer(cols = contains("Session"), 
                        names_to = "Session-id", 
                        values_to = "Notes") %>% 
    group_by(`Server Code`) %>% 
    mutate(`Proto-id` = c(cur_group()$`Server Code`, rep("", n()-1)),
           Count = 1) %>% 
    relocate(`Proto-id`, .after = `Server Code`) %>% 
    relocate(Count, .before = Notes) %>% 
    ungroup()
  return(protocol_tab.df)
}

make_condition_tab <- function(Session_tab.df) {
  condition_tab <- tibble(
    `Server Code` = unique(Session_tab.df$`Server Code`), 
    `Condition ID` = 'Condition1',
    `Protocol ID` = unique(Session_tab.df$`Server Code`), 
    `Require Max Volume [B]` = "TRUE"
  )
  return(condition_tab)
}

<<<<<<< HEAD
=======
make_conditionmatrix_tab_wircs <- function(server_codes.df, n_per_condition.df) {
  # n_per_condition.df <- n_per_condition.df %>% 
  #   mutate(trainig_task_camel <- training_condition_to_camel_case(`Traning Conditions`))
  n_per_condition.df <- n_per_condition.df %>% 
    mutate(training_task = training_condition_to_camel_case(`Traning Conditions`)) %>% 
    select(-`Traning Conditions`)
  
  condition_matrix.df <- server_codes.df %>% 
    mutate(`Traning Conditions` = training_condition_to_camel_case(`Traning Conditions`)) %>% 
    group_by(`Traning Conditions`) %>% 
    reframe(`Condition Matrix ID` = 
              c(unique(`Traning Conditions`), server_code), 
            `Parent ID` = unique(`Traning Conditions`), 
            `Condition ID` = c("", server_code), 
            `Distribution Weight` = n_per_condition.df %>% 
              filter(training_task == unique(`Traning Conditions`)) %>% 
              .$Weight,
            Capacity = n_per_condition.df %>% 
              filter(training_task == unique(`Traning Conditions`)) %>% 
              .$N_Total) %>% 
    group_by(`Traning Conditions`) %>% 
    mutate(`Condition ID` = ifelse(`Parent ID` == `Condition Matrix ID`, 
                                   "", `Condition ID`),
           `Distribution Weight` = ifelse(`Parent ID` == `Condition Matrix ID`, 
                                   `Distribution Weight`, 1),
           Capacity = ifelse(`Parent ID` == `Condition Matrix ID`, 
                             Capacity, ceiling(Capacity/(n()-1))),
           `Parent ID` = ifelse(`Parent ID` == `Condition Matrix ID`, 
                                "", `Parent ID`), 
           Notes =  rep("", n())) %>% 
    ungroup() %>% 
    select(-`Traning Conditions`)
    
  
  return(condition_matrix.df)
}

>>>>>>> 737c424 (now includes questionnaires)
make_conditionmatrix_tab <- function(Session_tab.df) {
  server_code_array <- rep(unique(Session_tab.df$`Server Code`), each = 5)
  n_rows <- length(server_code_array)
  ConditionMatrix_tab.df <- tibble(
    `Server Code` = server_code_array,
    `Condition Matrix ID` = rep("", n_rows),
    `Parent ID` = rep("", n_rows),
    `Condition ID` = rep("", n_rows),
    `Weight` = rep("", n_rows),
    `Limit` = rep("", n_rows),
    `Meta` = rep("", n_rows)
  )
  return(ConditionMatrix_tab.df)
}

make_blessedbatteries_tab <- function(Session_tab.df) {
  server_code_array <- rep(unique(Session_tab.df$`Server Code`), each = 10)
  n_rows <- length(server_code_array)
  BlessedBatteries_tab.df <- tibble(
    `Server Code` = server_code_array,
    `Blessed Battery ID` = rep("", n_rows),
    Name = rep("", n_rows),
    Notes = rep("", n_rows)
  )
  return(BlessedBatteries_tab.df)
}

make_servercodedetails_tab <- function(conditions.df,  prefix = "w") {
  idx_columns <- conditions.df %>% select(contains("Idx"))
  combinations_idx.df <- expand.grid(purrr::map(idx_columns, unique), stringsAsFactors = FALSE) %>% 
    tidyr::drop_na() %>% 
    mutate(server_code = paste0(prefix, 
                                `Age Idx`, 
                                `Training Location Idx`,
                                `Traning Conditions Idx`, 
                                `Expectation Conditions Idx`, 
                                `Task Orders Idx`, 
                                `Set Orders Idx`, 
                                `Voice Idx`))
  text_columns <- conditions.df %>% select(!contains("Idx"))
  combinations_text.df <- expand.grid(purrr::map(text_columns, unique), stringsAsFactors = FALSE) %>% 
    tidyr::drop_na() %>% 
    mutate(server_code = combinations_idx.df$server_code) %>% 
    relocate(server_code) %>% 
    arrange(desc(Age), 
            desc(`Training Location`), 
            `Traning Conditions`, 
            desc(`Expectation Conditions`), 
            `Task Orders`, 
            `Set Orders`, 
            Voice)
  
  combinations_text_bau.df <- combinations_text.df %>% 
    filter(`Traning Conditions` == "Business-as-Usual") %>% 
    mutate(`Training Location` = "N/A", 
           `Expectation Conditions` = "N/A", Voice = "N/A", 
           server_code = gsub("w(o|y)[ih]b[abcd]([ab])([tuvxyz])[mf]", 
                              "w\\1zbz\\2\\3z", 
                              server_code)) %>% 
    unique()
  
  combinations_text.df <- bind_rows(combinations_text.df %>% 
                                      filter(`Traning Conditions` != "Business-as-Usual"), 
                                    combinations_text_bau.df) %>% 
    arrange(desc(Age), 
            desc(`Training Location`), 
            `Traning Conditions`, 
            desc(`Expectation Conditions`), 
            `Task Orders`, 
            `Set Orders`, 
            Voice) %>% 
    mutate(Age = factor(Age, levels = unique(Age)), 
           `Training Location` = factor(`Training Location`, levels = unique(`Training Location`)), 
           `Traning Conditions` = factor(`Traning Conditions`, levels = unique(`Traning Conditions`)), 
           `Expectation Conditions` = factor(`Expectation Conditions`, levels = unique(`Expectation Conditions`)), 
           `Task Orders` = factor(`Task Orders`, levels = unique(`Task Orders`)), 
           `Set Orders` = factor(`Set Orders`, levels = unique(`Set Orders`)), 
           Voice = factor(Voice, levels = unique(Voice)))
  
  return(combinations_text.df)
}

make_task_order_long <- function(task_order.df) {
  task_order_long.df <- task_order.df %>% 
    tidyr::pivot_longer(cols = !contains("Order"), 
                        names_to = c("Session", "Day"), 
                        names_sep = "\\.", 
                        values_to = "task") %>% 
    mutate(Session = factor(Session, levels = unique(Session))) %>% 
    group_by(Order, Session, Day) %>% 
    dplyr::reframe(task_number = seq(length(strsplit(task, "\n|\r\n")[[1]])), 
                   task = strsplit(task, "\n|\r\n")[[1]])
  return(task_order_long.df)
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " |-"), cap, USE.NAMES = !is.null(names(s)))
}

replicate_dataframe <- function(data, N_Sessions) {
  bind_rows(lapply(1:N_Sessions, function(i) {
    data %>% mutate(Day = i)
  }))
}

make_task_server_code <- function(df, order.df, prefix = 'wircs') {
  tut_str <- ifelse(df$session == "Pre_Test", 
                    "long_tut", "short_tut")
  session_col <- ifelse(df$session == "Mid_Test",
                        capwords(df$task_version),
                        as.character(df$session))
  set_string <- order.df[order.df$`Task Short Name` == df$task & 
                           grepl(pattern = df$set_order, x = order.df$`Task Order IDs`),][[session_col]] 
  # print(df)
  set_str <- case_when(
    set_string == "--" ~ "",
    grepl(pattern = "^[ABCDEXY12]{1,2}$", x = set_string) ~ 
      paste0("set_", tolower(set_string)), 
    TRUE ~ set_string
  )
  server_code_str <- sprintf("%s_%s_%s_%s_%s",
                             prefix, df$task, df$task_version, set_str, tut_str)
  server_code_str <- gsub(pattern = '__', replacement = '_', 
                          server_code_str) # Remove double undrscores
  df$server_code <- server_code_str
  return(df)
}

find_task_server_code <- function(df, order.df, batteries.df, prefix = 'wircs', progress_bar = NULL) {
  # if (df$task == "induction") {
  #   browser()
  # }
  if(!is.null(progress_bar)) {
    progress_bar$tick(0)
  }
  
  tut_str <- ifelse(df$session == "Pre_Test", 
                    "long_tut", "short_tut")
  session_col <- ifelse(df$session == "Mid_Test",
                        capwords(df$task_version),
                        as.character(df$session))
  set_string_raw <- order.df[order.df$`Task Short Name` == df$task & 
                           grepl(pattern = df$set_order, x = order.df$`Task Order IDs`),][[session_col]] 
  # browser()
  set_str <- case_when(
    set_string_raw == "--" ~ "",
    grepl(pattern = "^[ABCDEXY12]{1,2}$", x = set_string_raw) ~ 
      paste0("set_", tolower(set_string_raw)), 
    TRUE ~ set_string_raw
  )
  if(df$task %in% order.df$`Task Short Name`) {
    server_code_str <- batteries.df %>% 
      rowwise() %>% 
      mutate(is_target = all(str_detect(`Server Code`, 
                                        c(prefix, paste0("_", df$task, "_"), df$task_version, set_str, tut_str)), na.rm = T)) %>% 
      filter(is_target) %>% 
      .$`Server Code`
  } else if (grepl("induction", df$task)) {
    server_code_str <- batteries.df %>% 
      rowwise() %>% 
      mutate(is_target = all(str_detect(`Server Code`, 
                                        c(prefix, "video", df$task_version, df$speed, paste0("_", df$voice))), na.rm = T)) %>% 
      filter(is_target) %>% 
      .$`Server Code`
  } else if (grepl("feedback", df$task)) {
    server_code_str <- batteries.df %>% 
      rowwise() %>% 
      mutate(is_target = all(str_detect(`Server Code`, 
                                        c(prefix, df$task, df$task_version)), na.rm = T)) %>% 
      filter(is_target) %>% 
      .$`Server Code`
  } else if (grepl("(break|questionnaire|survey|inter_screen)", df$task)) {
    server_code_str <- batteries.df %>% 
      rowwise() %>% 
      mutate(is_target = all(str_detect(`Server Code`, 
                                        c(prefix, df$task)), na.rm = T)) %>% 
      filter(is_target) %>% 
      .$`Server Code`
  } else {
    server_code_str <- "SERVER_CODE_NOT_FOUND"
  }
  
  # prefix, df$task, df$task_version
  # server_code_str <- grep(pattern = tut_str, x = batteries.df$`Server Code`, value = T)
  
  # print(df)
  
  df$server_code <- server_code_str
  
  if(!is.null(progress_bar)) {
    progress_bar$tick()
  }
  return(df)
}

make_training_server_code <- function(df, params.df, prefix = 'wircs') {
  # if(df$task == "gnb") {browser()}
  day_var <- ifelse(as.character(df$day) %in% 
                      (params.df %>% filter(`Task Short Name` == df$task) %>% .$Day),
                    as.character(df$day),
                    "Other")
  tut_var <- params.df %>% filter(`Task Short Name` == df$task & Day == day_var) %>% .$Tutorial
  tut_str <- ifelse(tut_var == "YES", 
                    "long_tut", "no_tut")
  duration_string <- params.df[params.df$`Task Short Name` == df$task & 
                                 params.df$Day == day_var,][["Duration"]] 
  # browser()
  server_code_str <- sprintf("%s_%s_%s_%s",
                             prefix, df$task, ifelse(is.na(duration_string), "", duration_string), tut_str)
  server_code_str <- gsub(pattern = '__', replacement = '_', 
                          server_code_str) # Remove double undrscores
  df$server_code <- server_code_str
  # print(df)
  return(df)
}

find_training_server_code <- function(df, params.df, batteries.df, prefix = 'wircs') {
  # if(df$task == "gnb") {browser()}
  day_var <- ifelse(as.character(df$day) %in% 
                      (params.df %>% filter(`Task Short Name` == df$task) %>% .$Day),
                    as.character(df$day),
                    "Other")
  tut_var <- params.df %>% filter(`Task Short Name` == df$task & Day == day_var) %>% .$Tutorial
  tut_str <- ifelse(tut_var == "YES", 
                    "long_tut", "no_tut")
  duration_string <- params.df[params.df$`Task Short Name` == df$task & 
                                 params.df$Day == day_var,][["Duration"]] 
  # print(df)
  # browser()
<<<<<<< HEAD
  if (grepl("survey", df$task)) {
=======
  if (grepl("survey|questionnaires", df$task)) {
    # print(df$task)
>>>>>>> 737c424 (now includes questionnaires)
    server_code_str <- batteries.df %>% 
      rowwise() %>% 
      mutate(is_target = all(str_detect(`Server Code`, 
                                        c(prefix, df$task)), na.rm = T)) %>% 
      filter(is_target) %>% 
      .$`Server Code`
  } else {
    server_code_str <- batteries.df %>% 
      rowwise() %>% 
      mutate(is_target = all(str_detect(`Server Code`, 
                                        c(prefix, paste0("_", df$task, "_"), duration_string, tut_str)), na.rm = T)) %>% 
      filter(is_target) %>% 
      .$`Server Code`    
  }
  
  df$server_code <- server_code_str
  # print(df)
  return(df)
}

add_space_before_caps <- function(string) {
  gsub("([A-Z])", " \\1", string) %>% trimws()
}

make_notes <- function(df, session_type, which_session, note_text, day, task_order, task_version, set_order) {
  # print(which_session, day)

  if(session_type == "Training_Session") {
    n_training_sessions <- max(df %>% 
                                 filter(session == which_session) %>% 
                                 .$day)
    task_name <- add_space_before_caps(gsub("^[A-z]*_", "", which_session))
    
    note_str<- sprintf(note_text, day, n_training_sessions, task_name)
  } else if (session_type == "Testing_Session") {
    note_str <- sprintf(note_text, day, task_order, task_version)
  } else {
    note_str <- "No Notes"
  }

  return(note_str)
}

make_training_session_end_screen <- function(df, session_type, which_session, end_screen_text, day) {
  # print(which_session, day)
  if(session_type == "Training_Session") {
    n_training_sessions <- max(df %>% 
                                 filter(session == which_session) %>% 
                                 .$day)
    
    end_screen_str<- sprintf(end_screen_text, day, n_training_sessions) 
  } else if (session_type == "Testing_Session") {
    end_screen_str <- end_screen_text
  } else {
    end_screen_str <- "End of Session"
  }
  return(end_screen_str)
}

get_password <- function(pw.df, session_type, session_number) {
  if(grepl("Training", session_type)) {
    pw_str <- pw.df %>% filter(Session == session_number) %>% .$Training_Passwords_Text
  } else {
    pw_str <- pw.df %>% filter(Session == session_number) %>% .$Testing_Passwords_Text
  } 
  return(pw_str)
}

make_lockout_times <- function(session_type, day, 
                              two_server_codes_per_training_session = T, 
                              lockout_time_between_days_in_hours = 12, 
                              lockout_time_same_day_in_minutes = 5) {
  # Lockout time between two sessions on two separate days
  lockout_time_between_days_in_seconds <- 
    lockout_time_between_days_in_hours*3600
  
  # Lockout time between two sessions on the same day
  lockout_time_same_day_in_seconds <- 
    lockout_time_same_day_in_minutes*60
  
  if(two_server_codes_per_training_session & grepl("Training", session_type) & day %% 2 == 0) {
    # Alternate between the same_day and next_day wait times
    output_time <- as.character(lockout_time_same_day_in_seconds)
  } else {
    # Always long wait 
    output_time <- as.character(lockout_time_between_days_in_seconds)
  }
  
  return(output_time)
}

training_condition_to_camel_case <- function(task_name) {
  return(gsub("( |-)", "", capwords(task_name)))
}