library(dplyr) 
library(here)
library(readxl)
library(progress)
library(openxlsx)
library(stringr)

source(file.path(here(), "Scripts", "Make_Tab_Functions.R"))
source(file.path(here(), "Scripts", "Save_Excel_Files.R"))

# Load the data
<<<<<<< HEAD
excel_file_name <- file.path(here(), "Tables", "Input", "Server_Codes_WIRCS_Input_YA_In-Lab.xlsx")
update_server_code_combinations <- FALSE
=======
excel_file_name <- "Server_Codes_WIRCS_Input_YA_In-Lab_Only-ABA.xlsx"
excel_file_full_path <- file.path(here(), "Tables", "Input", excel_file_name)
update_server_code_combinations <- TRUE
>>>>>>> 737c424 (now includes questionnaires)

if (update_server_code_combinations) {
  conditions.df <- read_xlsx(
    path = excel_file_name, 
    sheet = "Meta Data"
  )
  combinations_text.df <- make_servercodedetails_tab(conditions.df,  prefix = "w")
  save_combinations_to_excel(combinations_text.df %>% 
                               filter(Age == "Younger Adults" & 
                                        `Training Location` %in% c("In-Lab", "N/A")), 
<<<<<<< HEAD
                             excel_file_name, "Server Codes Details", 
                             file_name_suffix = "_YA_In-Lab")  
=======
                             excel_file_full_path, "Server Codes Details", 
                             file_name_suffix = "")  
>>>>>>> 737c424 (now includes questionnaires)
}

server_codes.df <- read_xlsx(
  path = excel_file_name, 
  sheet = "Server Codes Details"
)

subbateries.df <- read_xlsx(
  path = excel_file_full_path, 
  sheet = "Subbatteries"
)

set_orders.df <- read_xlsx(
  path = excel_file_name, 
  sheet = "Set Orders",
  skip = 1
)
  
task_order.df <- read_xlsx(
  path = excel_file_name, 
  sheet = "Task Orders (R-Friendly)"
)
task_meta_lines.df <- task_order.df %>% filter(Order <= 0)
task_order.df <- task_order.df %>% filter(Order > 0) %>% select(-What)
task_order_long.df <- make_task_order_long(task_order.df)

training_params.df <- read_xlsx(
  path = excel_file_name, 
  sheet = "Training Parameters"
)

training_batteries.df <- read_xlsx(
  path = excel_file_name, 
  sheet = "Training Batteries (R-Friendly)"
)
training_meta_lines.df <- training_batteries.df %>% filter(Order <= 0) %>% select(-N_Sessions)
training_batteries.df <- training_batteries.df %>% filter(Order > 0)
N_Training_Sessions <- training_batteries.df$N_Sessions
training_batteries.df <- training_batteries.df %>% 
  select(-N_Sessions, -What)
training_batteries_long.df <- 
  replicate_dataframe(make_task_order_long(training_batteries.df), 
                      N_Training_Sessions) 

passwords.df <- read_xlsx(
  path = excel_file_name, 
  sheet = "Passwords", 
  skip = 3
)

# end_screens.df <- read_xlsx(
#   path = excel_file_name, 
#   sheet = "End Screens"
# )
# 
# notes.df <- read_xlsx(
#   path = excel_file_name, 
#   sheet = "Notes"
# )

batteries.df <- read_xlsx( # list of individual task batteries
  path = excel_file_name, 
  sheet = "Server Codes (Tasks)"
) %>% 
  filter(!is.na(`File name`))

# Make the list of sessions for each server code 
set_order_list <- data.frame(set_order = unique(server_codes.df$`Set Orders`))
names(task_order_long.df) <- tolower(names(task_order_long.df))
mid_test_versions <- c("placebo", "nocebo")
voices <- c("male", "female")
video_mid_test_versions <- rep(mid_test_versions, each = length(voices))
video_voice_versions <- rep(voices, times = length(mid_test_versions))
# speeds <- c("fast", "slow")
speeds <- "fast"

server_code_test_list_1.df <- task_order_long.df %>% 
  rename(task_order = order) %>% 
  cross_join(set_order_list) %>% 
  group_by(across(everything())) %>% 
  mutate(
    task_version = case_when(
      session == "Mid_Test" ~ list(mid_test_versions),
      task == "induction" ~ list(video_mid_test_versions),
      TRUE ~ list("neutral")), 
    voice = case_when(
      task == "induction" ~ list(video_voice_versions),
      TRUE ~ list(NA)), 
    speed = case_when(
      task == "induction" ~ list(speeds),
      TRUE ~ list(NA))) %>% 
  tidyr::unnest(c(task_version, voice, speed))

server_code_list_pb <- progress_bar$new(total = nrow(server_code_test_list_1.df))
server_code_test_list_2.df <- server_code_test_list_1.df %>% 
  group_by_all() %>% 
  # group_map(~make_task_server_code(., set_orders.df, 'wircs'), .keep = T) %>%
  group_map(~find_task_server_code(., set_orders.df, batteries.df, 'wircs', 
                                   progress_bar = server_code_list_pb), .keep = T) %>%
  bind_rows() 

server_code_test_list_3.df <- server_code_test_list_2.df %>%
  mutate(session_name = ifelse(session == "Mid_Test",
                               sprintf("%s-D%s-O%i-%s-%s", session, day, task_order, set_order, capwords(task_version)),
                               sprintf("%s-D%s-O%i-%s", session, day, task_order, set_order)), 
         day = as.numeric(day)) 

server_code_test_list.df <- server_code_test_list_3.df %>%
  group_by(across(everything())) %>%
  mutate(
    task_version = case_when(
      session == "Pre_Test" & day == 2 & task != "induction" ~ list(video_mid_test_versions),
      TRUE ~ list(task_version)), 
    voice = case_when(
      session == "Pre_Test" & day == 2 & task != "induction" ~ list(video_voice_versions),
      TRUE ~ list(voice)), 
    speed = case_when(
      session == "Pre_Test" & day == 2 & task != "induction" ~ list(speeds),
      TRUE ~ list(speed))) %>% 
  tidyr::unnest(c(task_version, voice, speed)) %>% 
  ungroup() %>% 
  mutate(session_name =ifelse(session == "Pre_Test" & day == 2, 
                              paste0(session_name, "-", 
                                     capwords(task_version), "-", 
                                     capwords(voice)), 
                             session_name))

# Training batteries
names(training_batteries_long.df) <- tolower(names(training_batteries_long.df))
server_code_training_list.df <- training_batteries_long.df %>% 
  rename(task_order = order) %>%
  group_by_all() %>%
  group_map(~find_training_server_code(., training_params.df, batteries.df, prefix = 'wircs'), .keep = T) %>%
  bind_rows() %>% 
  mutate(session_name = sprintf("%s-D%s", session, day))

# Merge training and testing datasets
server_code_list.df <- bind_rows(server_code_test_list.df, 
                                 server_code_training_list.df)

# Get session number by type of session
session_number_by_type <- server_code_list.df %>% 
  select(session, day) %>% 
  unique() %>%
  mutate(session_type = ifelse(grepl("Training", session), 
                               "Training_Session", 
                               "Testing_Session")) %>% 
  group_by(session_type) %>% 
  mutate(session_number = 
           ifelse(session_type == "Training_Session", 
                  day, 
                  seq(n()))) %>% 
  ungroup()

# Add the notes, passwords, and end screens
meta_lines.df <- merge(task_meta_lines.df, training_meta_lines.df)
notes_pw_endscreens.df <- server_code_list.df %>% 
  select(session_name, task_order, session, day, set_order, task_version) %>% 
  unique() %>% 
  merge(session_number_by_type, sort = F) %>% 
  rowwise() %>% 
  mutate(notes = 
           make_notes(
             df = server_code_list.df, 
             session_type = session_type,
             note_text = meta_lines.df %>% 
               filter(meta_lines.df$What == "Notes") %>% 
               .[[paste0(session, ".", ifelse(session_type == "Testing_Session", day, 1))]], 
             which_session = session, 
             day = day, 
             task_order = task_order, 
             task_version = task_version, 
             set_order = set_order),
         password = 
           get_password(
             pw.df = passwords.df, 
             session_type = session_type, 
             session_number = session_number), 
         start_screen_texts = 
           make_training_session_end_screen(
             df = server_code_list.df,
             session_type = session_type,
             end_screen_text = meta_lines.df %>% 
               filter(grepl("Start Screen", meta_lines.df$What)) %>% 
               .[[paste0(session, ".", ifelse(session_type == "Testing_Session", day, 1))]],
             which_session = session, 
             day = day),
         end_screen_texts = 
           make_training_session_end_screen(
             df = server_code_list.df,
             session_type = session_type,
             end_screen_text = meta_lines.df %>% 
               filter(grepl("End Screen", meta_lines.df$What)) %>% 
               .[[paste0(session, ".", ifelse(session_type == "Testing_Session", day, 1))]],             which_session = session, 
             day = day), 
         lockout_time = 
           make_lockout_times(session_type = session_type, 
                              day = day, 
                              two_server_codes_per_training_session = T, 
                              lockout_time_between_days_in_hours = 12, 
                              lockout_time_same_day_in_minutes = 5)
  ) %>% 
  tidyr::pivot_longer(cols = c(notes, password, start_screen_texts, end_screen_texts, lockout_time),
                      names_to = "task", 
                      values_to = "server_code") %>% 
  ungroup() %>% 
  mutate(task_number = case_when(task == "notes" ~ -4,
                                 task == "password" ~ -3,
                                 task == "start_screen_texts" ~ -2,
                                 task == "end_screen_texts" ~ -1,
                                 task == "lockout_time" ~ 0,
                                 TRUE ~ 9999)) %>% 
  select(-session_number, -session_type) 

notes_pw_endscreens_count.df <- notes_pw_endscreens.df %>% 
  count(session, day, task_order, set_order, server_code, task_version)

# Merge server codes with notes
server_code_table_long.df <- bind_rows(server_code_list.df, 
                                       notes_pw_endscreens.df)

server_code_table_count.df <- server_code_table_long.df %>% 
  select(session_name, server_code, task_number) %>% 
  count(session_name, server_code, task_number)

# Make the table
session_tab_headers <- c("Notes", "Password", "Custom Session Start Screen",
                         "Custom Session End Screen", "Lockout Time in Seconds", "Elements")
server_code_table.df <- server_code_table_long.df %>% 
  select(session_name, server_code, task_number) %>% 
  tidyr::pivot_wider(names_from = session_name, 
                     values_from = server_code) %>%
  arrange(task_number) %>% 
  rename(col_headers = task_number) %>% 
  mutate(col_headers = c(session_tab_headers, rep("", n() - length(session_tab_headers))))

server_code_list_pb <- progress_bar$new(total = nrow(server_codes.df))
server_codes.df %>% 
  group_by_all() %>% 
  group_map(~ make_excel_files_for_server_codes_wircs(.x, 
                                                      batteries.df, 
                                                      subbateries.df,
                                                      server_code_table.df, 
<<<<<<< HEAD
                                                      server_code_list_pb), 
            .keep = T)
=======
                                                      n_per_condition.df, 
                                                      output_file_name)

server_codes_no_bau.df <- server_codes.df %>% filter(`Traning Conditions` != "Business-as-Usual")
output_file_name_no_bau <- gsub(".xlsx", "_no_bau.xlsx", output_file_name, fixed = T)
excel_file_no_bau.df <- make_excel_file_all_conditions_wircs(server_codes_no_bau.df,
                                                             batteries.df, 
                                                             subbateries.df,
                                                             server_code_table.df, 
                                                             n_per_condition.df, 
                                                             output_file_name_no_bau)
>>>>>>> 737c424 (now includes questionnaires)
