require(tidyverse)
require(patentsview)
require(here)

# this pulls a table of fields and their descriptions which can be called
# to identify any other fields of interest
fieldsTbl <- tibble::as_tibble(fieldsdf) %>%
  dplyr::filter(endpoint == "patents")

# the query is defined here as it will be the same in each of the 3 searches
#
# with_qfuns is a helper function used by patentsview to build a query for
# the patentsview API. if you enter query into the command line you'll see
# the gobbledygook it generates
query <- patentsview::with_qfuns(
  and(
    eq(patent_firstnamed_inventor_state = "IL"),
    or(
      eq(patent_firstnamed_inventor_city = "Bloomington"),
      eq(patent_firstnamed_inventor_city = "Normal")
    ),
    eq(patent_year = 2019)
  )
)

# I am wrapping this in a function because somebody told me that is good
# practice
countPatents <- function() {
  
  # these are the fields which the search will return
  fields <- c(
    "patent_number",
    "patent_year",
    "patent_date",
    "patent_firstnamed_inventor_city",
    "patent_firstnamed_inventor_state"
  )
  
  # the following lines execute the search and then process the table
  # the %>% operator takes the output of the previous function and 
  # "pipelines" it through as the first argument of the next function
  pv_res <- search_pv(query, fields, endpoint = "patents", all_pages = TRUE) %>%
    as_tibble() %>%
    select(data) %>%
    unnest(data) %>%
    unique() %>%
    group_by(patent_number) %>%
    summarize(city = first(patent_firstnamed_inventor_city)) %>%
    group_by(city) %>%
    count(city) %>%
    arrange(desc(n)) %>%
    bind_rows(bind_cols(city = "Bloomington-Normal", n = sum(pv_res$n)))
}

# the other two functions work using the same logic as the first
countPatentsByOrg <- function() {
  
  fields <- c(
    "assignee_organization",
    "patent_number",
    "patent_year",
    "patent_date",
    "patent_firstnamed_inventor_city",
    "patent_firstnamed_inventor_state"
  )
  
  pv_res <- search_pv(query, fields, endpoint = "patents", all_pages = TRUE) %>%
    as_tibble() %>%
    select(data) %>%
    unnest(data) %>%
    unique() %>%
    select(assignees) %>%
    unnest(assignees) %>%
    group_by(assignee_organization) %>%
    tally() %>%
    arrange(desc(n))
}

getPatentTable <- function() {
  
  fields <- c(
    "assignee_organization",
    "patent_number",
    "patent_date",
    "patent_title",
    "patent_abstract"
  )
  
  pv_res <- search_pv(query, fields, endpoint = "patents", all_pages = TRUE) %>%
    as_tibble() %>%
    select(data) %>%
    unnest(data) %>%
    unique() %>%
    unnest(assignees)
    
}

# these lines call each function, store the output as objects in R and also
# in .csv files
patentCount <- countPatents() %>%
  write_csv(here("patent-count-by-city.csv"))

patentCountByOrg <- countPatentsByOrg() %>%
  write_csv(here("patent-count-by-organization.csv"))

patentTable <- getPatentTable() %>%
  write_csv(here("patent-data-table.csv"))

