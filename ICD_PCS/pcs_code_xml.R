library(tidyverse)
library(jsonlite)

### SKIP TO `read / write to file` section to get the data

### ABOUT SOURCE FILE
###   This JSON comes from the XML file < icd10pcs_index_2020.xml > which was 
###   downlaoded from the CMS website <link1> on 2020-08-19. I converted the 
###   XML to JSON using an online converter <link2>
###   
###   - Link1: <https://www.cms.gov/Medicare/Coding/ICD10/2020-ICD-10-PCS>
###   - Link2: <https://www.convertjson.com/xml-to-json.htm>
raw_json <- jsonlite::fromJSON("~/Github/HCUP_tools/ICD_PCS/Resources/pcs_codes.json")[["ICD10PCS.tabular"]][["pcsTable"]]
json_tbl <- tibble(data = raw_json)


### STRUCTURE OF THE DATA
###    The general structure is outlined below. For each pcsTable (the first
###    3 chars in the code), there will only be three axis items (for postions 
###    1, 2, and 3). There can be one or more pcsRows for each pcsTable, which
###    represent all the permutations of codes for the remaining positions. 
###    Within each pcsRow multiple labels may exist, each providing the pairs
###    of the code for that position and what it stands for
###    The term "title" referes to the header for that postion (e.g. body part 
###    or device)
###    
# pcsTable       [x880]
# ├─axis (pos=1)
# │ ├─title      
# │ └─label      
# │   ├─code     
# │   └─text     
# ├─axis (pos=2)
# │ ├─title      
# │ └─label      
# │   ├─code     
# │   └─text     
# ├─axis (pos=3)
# │ ├─title      
# │ ├─label      
# │ │ ├─code     
# │ │ └─text     
# │ └─definition  # only one with definition
# └─pcsRow       [one or more]
#     ├─axis (pos=4)
#     │ ├─title      
#     │ └─label  [one or more]
#     │   ├─code  
#     │   └─text     
#     ├─axis (pos=5)
#     │ ├─title      
#     │ └─label  [one or more]
#     │   ├─code  
#     │   └─text     
#     ├─axis (pos=6)
#     │ ├─title      
#     │ └─label  [one or more]
#     │   ├─code  
#     │   └─text     
#     ├─axis (pos=7)
#       ├─title      
#       └─label  [one or more]
#         ├─code  
#         └─text     




###################
###   First 3   ###
### ----------- ###
##  Expand out the portion of the data describing the first 3 
##  codes. Ignore the warning from `unnest()`
first3_tbl <- tibble(axis = pluck(raw_json, "axis")) %>%
  mutate(pcsTableid = row_number()) %>%
  
  # Next step
  unnest_wider(axis) %>%
  rename(pos_title     = `_pos`,
         n_values      = `_values`) %>%
  unnest(title, label,  pos_title, n_values, definition) %>%
  
  unnest_wider(label) %>%
  rename(code = `_code`,
         text = `__text`) %>%
  
  mutate(text = str_glue("[{title}] {text}")) %>%
  
  # Make it in long format
  select(-title, -contains("definition")) %>%
  pivot_longer(cols = c(code, text)) %>%
  
  # Pivot wider and save values as list
  pivot_wider(id_cols     = c(ends_with("id"), 
                              starts_with("n_")),
              values_from = c(value),
              names_from  = c(name, pos_title),
              values_fn   = list) %>%
  
  # Unnest the Cartesian product
  unnest(cols = c(code_1, text_1)) %>%
  unnest(cols = c(code_2, text_2)) %>%
  unnest(cols = c(code_3, text_3)) %>%
  
  # Join codes together
  mutate(first3 = paste0(code_1, code_2, code_3)) %>%
  
  select(ends_with("id"), starts_with("n_"),
         first3,
         starts_with("code"),
         starts_with("text"),
         everything())



####################
###   PCS ROWS   ###
### ------------ ###
## For each of these, has a data frame with each row
## being the pcsRow (i.e. table with 1 row only had one pcsRow)
codes_explained <- tibble(axis = pluck(raw_json, "pcsRow")) %>%
  mutate(pcsTableid = row_number()) %>%
  
  # Join to table with first 3 codes
  inner_join( select(first3_tbl, pcsTableid, first3)) %>%
  relocate(axis, .after=first3) %>%

  # Spread out to make each 'pcsRow' a row
  hoist(axis, codes = "_codes") %>%
  unnest_wider(axis) %>%
  unnest(c(codes, axis)) %>%
  unnest_wider(axis) %>%
  
  # recode names (n_codes_group is total number of codes 
  # in the pcsRow)
  rename(n_codes_group = codes,
         pos_title     = `_pos`,
         n_values      = `_values`) %>%
  
  # Give each pcsRow a unique id (with the first3 as prefix)
  group_by(first3) %>%
  mutate(pcsRowid = paste0(first3, "_id", row_number())) %>%
  ungroup() %>%
  relocate(pcsRowid, .after=first3) %>%
  
  # Unnest the four remaining positions
  unnest(c(title, pos_title, n_values, label)) %>%
  
  # Our only remaining nested data is the label for each
  # code charachter and it's corresponding description.
  # This is the last unnesting
  unnest_wider(label) %>%
  rename(code = `_code`,
         text = `__text`) %>%
  unnest_legacy(code, text) %>%
  relocate(code, text, .after=n_values) %>%
  
  #####   FILTER TO DEBUG   #####
  # makes problem smaller 
  # filter(pcsRowid %in% c("001_id2", "007_id1", "002_id1")) %>%

  # Now, move the title into the description so we can
  # pivot into long format, which allows us to pivot
  # wider (like we did for the first3 table) in the
  # next step
  mutate(text = str_glue("[{title}] {text}")) %>%
  select(-title, -n_values) %>%
  pivot_longer(cols = c(code, text)) %>%
  
  # Pivot wider and save values as list
  pivot_wider(id_cols     = c(ends_with("id"), 
                              starts_with("n_"),
                              first3),
              values_from = c(value),
              names_from  = c(name, pos_title),
              values_fn   = list) %>%
  
  # Unnest the Cartesian product to create only
  # codes that we should be creating
  unnest(cols = c(code_4, text_4)) %>%
  unnest(cols = c(code_5, text_5)) %>%
  unnest(cols = c(code_6, text_6)) %>%
  unnest(cols = c(code_7, text_7)) %>%
  
  # Join back to the first3 table
  inner_join(first3_tbl, .) %>%
  select(-n_values) %>%
  select(pcsTableid, first3, pcsRowid, n_codes_group,
         starts_with("code_"), starts_with("text_"),
         everything()) %>%
  
  # Create column with the full PCS code
  mutate(I10_PCS = paste0(code_1, code_2, code_3, code_4, 
                          code_5, code_6, code_7)) %>%
  relocate(I10_PCS)

######################################
###   Idetify problematic tables   ###
### ------------------------------ ###
codes_explained %>%
  group_by(pcsRowid) %>%
  summarise(expected = as.numeric(unique(n_codes_group)),
            actual   = length(unique(I10_PCS))) %>%
  mutate(failure = expected != actual) %>%
  filter(failure)


################################
###   Read / write to file   ###
### ------------------------ ###
codes_explained %>% write_csv("~/Github/HCUP_tools/ICD_PCS/PCS_2020_codes_explained.csv")

codes_explained <- read_csv("~/Github/HCUP_tools/ICD_PCS/PCS_2020_codes_explained.csv", 
         col_types = cols(.default      = col_character(),
                          n_codes_group = col_double()))


##########################
###   Extract titles   ###
### ------------------ ###
with_titles <- codes_explained %>%
  mutate(across(where(is.character), as.character)) %>%
  mutate(across(.cols  = starts_with("text_"),
                fns    = ~str_extract(.x, "\\[.+\\]"),
                .names = "title_{.col}")) %>%
  rename_with(.fn = ~str_remove(.x, "text_"),
              .cols = starts_with("title_")) %>%
  mutate(across(starts_with("title_"), ~str_extract(.x, "\\[.+\\]"))) %>%
  mutate(across(starts_with("title_"), ~str_remove(.x, "\\]"))) %>%
  mutate(across(starts_with("title_"), ~str_remove(.x, "\\[")))
  

# We can see most codes share similar enough titles
with_titles %>% count(title_1, title_2, title_3, title_4, title_5, title_6, title_7, sort=T) 

# # y %>%
# #   group_by(pcsRowid) %>%
# #   distinct(title, pos_title) %>%
# #   glimpse()
# # 
# # count(sort = T)
# # 
# # 
# # y %>%
# #   filter(pcsRowid %in% c("abc2_1", "abc621_57")) %>%
# #   expand(nesting(pcsRowid), 
# #          nesting(code, text), 
# #          nesting(pos_title, title)) %>%
# #   View()
# 
# 
# y %>%
#   unnest_wider(label) %>%
#   hoist(label, code = `_code`,
#         text = `__text`) %>%
#   unnest(code, text)
# 
# 
# 
# 
# # str(max.level=2)
# # map(dim)
# 
# 
# 
# 
# # pluck(z, "label")
# # 
# # hoist(label, 
# #       code = "_code",
# #       text = "__text")
# # 
# # z %>% unnest_longer("code")
# # 
# # pluck("label")
# # hoist(label,
# #       code="_code")
# # unnest_longer("label") 
# # select(-`_codes`) %>%
# #   unnest_auto(everything())
# # unnest_longer("title", "_pos", names_repair = "unique")
# # names()
# # hoist(axis) %>%
# #   hoist(axis,
# #         title="title") %>%
# #   as_tibble()
# # 
# # 
# # 
# # select(-`_values`) %>%
# #   rename(title_pos=`_pos`) %>%
# #   dplyr::relocate(label, .after=definition) %>%
# #   unnest_wider("label") 
# # View()
# # 
# # x %>% pluck("title")
# # x %>% pluck("_pos")
# # x %>% unnest_longer("label")
# # 
# # 
# # x %>% pluck("label", 3, "_code")
# # x %>% pluck("label", 1, "__text")
# # 
# # dim(x)
# 
# processed_pcsRowid <- y %>% 
#   filter(pcsRowid=="001_id2") %>%
#   mutate(text = str_glue("[{title}] {text}")) %>%
#   select(-title, -n_values) %>%
#   pivot_longer(cols = c(code, text)) %>%
#   # expand(nesting(code, text), pos_title)
#   pivot_wider(values_from = c(value),
#               names_from = c(name, pos_title),
#               values_fn = list) %>%
#   unnest(cols = c(starts_with("code"), starts_with("text")))
# 
# testthat::expect_equal(nrow(processed_pcsRowid),
#                        mean(as.numeric(processed_pcsRowid$n_codes_group)))
# 
# 
# 
# y %>% 
#   filter(pcsRowid=="005_id1") %>%
#   mutate(text = str_glue("[{title}] {text}")) %>%
#   select(-title, -n_values) %>%
#   pivot_longer(cols = c(code, text)) %>%
#   # expand(nesting(code, text), pos_title)
#   pivot_wider(values_from = c(value),
#               names_from = c(name, pos_title),
#               values_fn = list) %>%
#   # Unnest the Cartesian product
#   unnest(cols = c(code_4, text_4)) %>%
#   unnest(cols = c(code_5, text_5)) %>%
#   unnest(cols = c(code_6, text_6)) %>%
#   unnest(cols = c(code_7, text_7)) 
# 
# 
# y %>%
#   filter(pcsRowid %in% c("001_2", "007_1", "002_1")) %>%
#   mutate(text = str_glue("[{title}] {text}")) %>%
#   select(-title, -n_values) %>%
#   pivot_longer(cols = c(code, text)) %>%
#   
#   # Pivot wider and save values as list
#   pivot_wider(id_cols     = c(ends_with("id"), 
#                               starts_with("n_"),
#                               first3),
#               values_from = c(value),
#               names_from  = c(name, pos_title),
#               values_fn   = list) %>%
#   
#   # Unnest the Cartesian product
#   unnest(cols = c(code_4, text_4)) %>%
#   unnest(cols = c(code_5, text_5)) %>%
#   unnest(cols = c(code_6, text_6)) %>%
#   unnest(cols = c(code_7, text_7)) %>%
#   
#   # Join codes together
#   mutate(pcs_code = paste0(first3, code_4, 
#                            code_5, code_6, code_7)) %>%
#   select(ends_with("id"), starts_with("n_"),
#          first3, pcs_code,
#          starts_with("code"),
#          starts_with("text"),
#          everything()) %>% View()
# 
# 
# ####################
# ###   Old code   ###
# ### ------------ ###
# 
# 
# x <- pluck(json_tbl, "data", "axis")[[1]] 
# y <- pluck(json_tbl, "data", "pcsRow")[[1]] 
# 
# # Do one of the axis first
# x %>% unnest_longer("title") %>%
#   select(-`_values`) %>%
#   rename(title_pos=`_pos`) %>%
#   dplyr::relocate(label, .after=definition) %>%
#   # unnest_wider("label")
#   hoist(label, 
#         code = "_code",
#         text = "__text") 
# 
# 
# 
# # Now PCS tables
# z <- y %>% 
#   mutate(row = row_number()) %>%
#   unnest_longer("axis") %>%
#   
#   # group_by(row_number()) %>%
#   pluck("axis") %>%  # heres the issue
#   
#   select(-`_values`) %>%
#   rename(title_pos=`_pos`) %>%
#   dplyr::relocate(label, .after=title_pos) %>%
#   
#   unnest_longer("label")
# 
# 
# # Maybe expand grid???
# z2 <- y %>% 
#   mutate(row = row_number()) %>%
#   unnest_wider("axis")
# z2
# 
# 
# 
