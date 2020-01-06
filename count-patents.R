require(tidyverse)
require(patentsview)
require(here)

fieldsTbl <- as_tibble(fieldsdf) %>%
  filter(endpoint == "patents")

query <- with_qfuns(
  and(
    eq(patent_firstnamed_inventor_state = "IL"),
    or(
      eq(patent_firstnamed_inventor_city = "Bloomington"),
      eq(patent_firstnamed_inventor_city = "Normal")
    ),
    eq(patent_year = 2019)
  )
)

countPatents <- function() {
  fields <- c("patent_number", "patent_year", "patent_date", "patent_firstnamed_inventor_city", "patent_firstnamed_inventor_state")
  
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

countPatentsByOrg <- function() {
  fields <- c("assignee_organization", "patent_number", "patent_year", "patent_date", "patent_firstnamed_inventor_city", "patent_firstnamed_inventor_state")
  
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
  fields <- c("assignee_organization", "patent_number", "patent_date", "patent_title", "patent_abstract")
  
  pv_res <- search_pv(query, fields, endpoint = "patents", all_pages = TRUE) %>%
    as_tibble() %>%
    select(data) %>%
    unnest(data) %>%
    unique() %>%
    unnest(assignees)
    
}

patentCount <- countPatents() %>%
  write_csv(here("patent-count-by-city.csv"))

patentCountByOrg <- countPatentsByOrg() %>%
  write_csv(here("patent-count-by-organization.csv"))

patentTable <- getPatentTable() %>%
  write_csv(here("patent-data-table.csv"))

