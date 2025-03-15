
Condition codes (server codes unique to each condition) is composed of 8 characters which can be found in the "Meta Data" tab. In a nutshell :
- wircs sutdy indicator : "w" for all codes
- age group : "y" for young adults, "o" for older adults
- training location : "i" for "in-lab", "h" for "at home"
- intervention : "k" for Knowldge Builder, "g" and "n" Gamified and Non-Gamified N-back respectively, "b" for Business-as-usual
- Expectation condition : "a" for Placebo-Placebo, "b" for Placebo-Nocebo, "c" for Nocebo-Placebo, "d" for Nocebo-Nocebo
- Task Orders : "a" for Order 1, "b" for order 2 (see tab "Task Order (R-Friendly)" for the exact orders of tasks in both orders)
- Set Orders : "t", "u", "v", "x", "y", "z" letters for all combinations of orders of sets A, B, and E. For tasks with only two sets A and B, then conditions t, v, and y have set orders ABA, and orders u, x, and z have set orders BAB.  
- Voice : "m" for male voice in the explicit expectation video and "f" for the female voice. 
For the business-as-usual groups, characters related to expectations or interventions were all marked "z". 

Input Excel file tabs
- Server Codes (Tasks) : 
List of all the server codes associated with each task battery. All task batteris follow the follwing format (and as much as possible do NOT deviate from is) : wircs_<task handle>_<placebo_nocebo_version>_<set marker>_<tutorial marker>, where
-- <task handle> is the short name of the task (can be found in the "Training Batteries (R-Friendly)" and "Task Orders (R-Friendly)" tabs)
-- <placebo_nocebo_version> is the version of the task, either "neutral", "placebo", or "nocebo"
-- <set_marker> IF the task has sets, this is the marker for the set this battery corresponds to (format : set_x where x is the set marker for this task, which can be found in the "Set Orders" tab)
-- <tutorial marker> is the marker for the tutorial version of the battery. "long_tut" for full tutorial, "short_tut" for the ttorial without wrong examples.
The traning batteries follow the format wircs_<task handle>_<duration marker>_<tutorial marker>, where "task handle" is same as above, and 
-- <duration marker> is the marker for the duration of the battery in the format "20m" for a 20 minute battery,
-- <tutorial marker> is the marker for the tutorial, either <long_tut> for the full tutorial, or <no_tut> for no tutorial
The video batteries follow the format wircs_video_<placebo_nocebo_version>_<voice marker>_<speed marker>, where
-- <voice marker> is the marker for the gender of the voice in the video, either "male" or "female"
-- <speed marker> is the marker for the speed of the video, either "fast" (for young adults) or "slow" (for older adults)
- Subbatteries : Table mapping child batteries with parent batteries (this is needed to ensure that the child batteries are added to the "Batteries" tab of the output table)
- Meta Data : table with the mapping between seevr code letters and conditions  
- Training Parameters : Parameters for the training tasks. "Task Short Name" is the task handle used in the "Traning Batteries" tab (and is used throughout the R code to match batteries with their parameters). Tutorial is a tag whether the battery has the tutorial or not. The Duration column is a the duration 
- Set Orders : This table shows the sets that should be used at each testing session for each task. When a task does not have a corresponding set, then "--" is used instead. 
- Training Batteries (R-Friendly) : This tab shows the composition of the traiing batteries (lines marked with Order == 1) and the number of training sessions in column N_sessions. There is also the texts that should be used for the Notes, the Start and End screens. Do not change remove the ".1" at the end of the column names. The lines are written in the sprintf syntax, and the "%i" and "%s" are markers that are replaced with the right name and session number. Please do not add or remove such markers if changingthe text, the code will break. In this tab, "Order" does not mean anything, but make sure to put negativ e values in the rows that do not contain the elements of the batteries. 
- Task Orders (R-Friendly) : similar to "Training Batteries", but with the assessment sessions. In this case, the Order column marks the id of the order in which the tasks will be presented. Again, negative orders should be used for the lines that do not contain battery elements 
- Server Codes Details : Contains the explicit and human-friendly conditions corresponding to each server code. DO NOT AMEND THIS TAB. This is the only tab that is automatically created by the R code according to the information in "Meta-Data" (basically making all the cross combinations). In this case, I have amended the R code such that only the server codes for younger adults and "in-lab" location were kept. 



