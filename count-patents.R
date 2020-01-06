require(tidyverse)
require(patentsview)
require(here)

fieldsTbl <- as_tibble(fieldsdf) %>%
  filter(endpoint == "patents")

countPatents <- function() {
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

patents <- countPatents() %>%
  write_csv(here("patents.csv"))
