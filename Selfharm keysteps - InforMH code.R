selfharm_standard_icd <- "X[67][0-9]\\D*|X8[0-4]\\D*|Y87\\D*0?$"

selfharm_icd <- "X[67][0-9]\\D*|X8[0-4]\\D*|Y87\\D*0?$|T50.9|T39.*[013489]|T42.*[2467]|T43.*[0239]"

selfharm_text <- ".*?\\b(od|over.*dose|^(?:(?!threat.*?|thoug.*?|plan.*?|risk|feel.*?).)*(?=suicide)((?!risk|idea.*|thoug.*|intent.*|plan.*|threat.*).)*$|^(?:(?!threat.*?|thoug.*?|plan.*?|risk|feel.*?).)*(?=self.*(harm.*|inflict.*))((?!risk|idea.*|thoug.*|intent.*|plan.*|threat.*).)*$|^(?:(?!food.*).)*(poison.*))\\b.*"

#' remove if following words in the text
selfharm_text_rm <- "\\b(accidental.*|unintentional.*|not intentional)"

#' regular expression for self-harm ideation
ideation_icd <- "R45\\D*8[19]?\\b"

ideation_text <- "(?=.*?\\b(plan.*|threat.*|thoug.*|feel.*|risk)\\b)(?=.*?\\b(suici.*|self.*(harm.*|inflict.*))\\b)((?!od|overdose|poison.*).)*$|(?=.*?\\b(suici.*|self.*(harm.*|inflict.*))\\b)(?=.*?\\b(idea.*|risk|thoug.*|intent.*|plan.*|threat.*)\\b)((?!od|overdose|poison.*).)*$"

icd_list <- list(selfharm_standard_icd = selfharm_standard_icd, 
                 selfharm_icd = selfharm_icd, 
                 ideation_icd = ideation_icd)

text_list <- list(selfharm_text = selfharm_text, 
                  ideation_text = ideation_text,
                  selfharm_text_rm = selfharm_text_rm)


# --- Function to normalise the "presenting problem" fields ----
normalisation_text <- function(data, by) {
  
  
  colBy <- as.list(substitute(by))
  if (colBy[1L] != 'c') {
    colBy <- colBy
  } else {
    colBy <- colBy[-1L]
  }
  colBy <- sapply(colBy, deparse)
  
  #' Normalisation process for presenting_problem
  colCheck <- c(colBy, "icd", "presenting_problem")
  
  if (sum(colCheck %in% colnames(data)) < length(colCheck))
    stop(paste(colCheck, collapse = ", "), " are not all in the data")
  
  initial <- copy(data)
  
  initial %>%
    .[, `:=`(icd = stringr::str_to_upper(icd), 
             presenting_problem_text = 
               stringr::str_replace_all(stringr::str_to_lower(presenting_problem),  
                                        c("[^[:alpha:]]" = " ",          
                                          "\\W" = " ",
                                          "\\s+" = " ")))
    ] %>%    
    .[!presenting_problem_text %in% c("", " ", NA)] %>%
    .[, presenting_problem := NULL]
  
  #' Normalisation process
  initial %>% 
    unnest_tokens(normalised_text, presenting_problem_text) %>%
    .[!normalised_text %chin% my_stop_words$word] %>%
    .[, normalised_test := textstem::lemmatize_words(normalised_text)] %>%
    .[!normalised_text %chin% c("", " ", NA)] %>%
    .[, .(presenting_problem_normalised = 
            stringr::str_c(normalised_text, collapse = " ")),
      by = eval(colBy)
    ]
}

# --- Initial self-harm detection ----
text_model_FUN <- function(data, icd_regex, text_regex) {
  
  data <- copy(data)
  
  data %>% 
    #' flag self-harm
    .[, selfharm_icd_standard_result := 
        fifelse(str_detect(icd, regex(icd_regex$selfharm_standard_icd)), 1, 0)
    ] %>%
    .[, `:=`(selfharm_icd_result = 
               fifelse(str_detect(icd, regex(icd_regex$selfharm_icd)), 1, 0), 
             selfharm_text_result = 
               fifelse(str_detect(presenting_problem_normalised, 
                                  regex(text_regex$selfharm_text, 
                                        ignore_case = TRUE)), 1, 0)
    )
    ] %>%
    .[, `:=`(selfharm_result = fifelse(selfharm_text_result == 1, 1, 
                                       fifelse(selfharm_icd_result == 1, 1, 0))
    )
    ] %>% 
    #' flag ideation
    .[, `:=`(ideation_icd_result = 
               fifelse(str_detect(icd, regex(icd_regex$ideation_icd)), 1, 0), 
             ideation_text_result = 
               fifelse(str_detect(presenting_problem_normalised, 
                                  regex(text_regex$ideation_text, 
                                        ignore_case = TRUE)), 1, 0)
    )
    ] %>%
    .[, ideation_result := fifelse(ideation_icd_result == 1, 1, 
                                   fifelse(ideation_text_result == 1, 1, 0))
    ] %>%
    #' remove 'accidental' and 'unintentional'
    .[str_detect(presenting_problem_normalised, 
                 regex(text_regex$selfharm_text_rm, 
                       ignore_case = TRUE)) & 
        selfharm_icd_standard_result == 0, 
      `:=`(selfharm_icd_result = 0, selfharm_text_result = 0, selfharm_result = 0)
    ] %>%
    #' correction if icd_ideation = 1 and icd_result == 1
    # 1. if ICD_ideation  = 1, this is an ideation ED
    # 2. if ICD or ICD_Standard = 1, this is a self-harm ED. 
    .[selfharm_icd_result == 1 | selfharm_icd_standard_result == 1, 
      `:=`(ideation_text_result = 0, ideation_result = 0)
    ] %>%
    .[ideation_icd_result == 1, 
      `:=`(selfharm_text_result = 0, selfharm_result = 0)
    ] %>%
    # Correction if self-harm presenting problem and ideation presenting problem 
    # both flagged, then it is a self-harm case
    .[selfharm_text_result == 1 & ideation_text_result == 1, 
      `:=`(ideation_text_result = 0, ideation_result = 0)
    ] %>%
    #' result for either self-harm or ideation
    .[, selfharm_or_ideation_result := 
        fifelse(selfharm_result == 1 | ideation_result == 1, 1, 0)
    ] %>%
    .[presenting_problem_normalised == '', presenting_problem_normalised := NA] %>%
    .[icd == '', icd := NA]
}

#----------------- Further filtering step --------------------------

EDDC %>%
  .[, age := as.numeric(as.period(interval(birth_date, arrival_date)), 'years')
  ] %>%
  .[year(birth_date) == 9999, age := NA] %>%

  #' If age < 10, then all results set to 0
  .[age < 10, (resultCols) := 0] %>%
  .[is.na(presenting_problem) & is.na(icd), (resultCols) := NA] %>%
  #' if ED visit type is not emergent or unplanned, then results set to 0
  .[!ed_visit_type %in% c('1', '3', '01', '03') & 
      !is.na(selfharm_or_ideation_result),(resultCols) := 0] 
