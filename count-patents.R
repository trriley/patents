require(tidyverse)
require(patentsview)

countPatents <- function(searchYear) {
  query <- with_qfuns(
    and(
      eq(inventor_county_fips = "17113"),
      gte(patent_year = eval(2019))
    )
  )

  fields <- c("patent_number", "patent_year", "patent_firstnamed_inventor_city", "patent_firstnamed_inventor_state")
  
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
    rename("number_of_patents" = n)
}

patents <- countPatents(searchYear = 2005) %>%
  print()


query <- with_qfuns(
  and(
    gte(patent_year = 2007)
  )
)

fields <- c("patent_number", "patent_date")

test <- as_tibble(pv_res) %>%
  select(data) %>%
  unnest(data) %>%
  unique() %>%
  group_by(patent_number) %>%
  summarize(city = first(patent_firstnamed_inventor_city)) %>%
  group_by(city) %>%
  count(city) %>%
  arrange(desc(n)) %>%
  rename("number_of_patents" = n)
