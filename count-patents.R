require(tidyverse)
require(patentsview)

countPatents <- function(searchYear = 2019) {
  query <- with_qfuns(
    and(
      eq(inventor_county_fips = "17113"),
      gte(patent_year = eval(searchYear))
    )
  )
  
  fields <- c("patent_number", "patent_year", "patent_date", "inventor_city", "inventor_county_fips")
  
  pv_res <- search_pv(query, fields, all_pages = TRUE) %>%
    as_tibble() %>%
    select(data) %>%
    unnest(data) %>%
    unique() %>%
    unnest(inventors) %>%
    group_by(patent_number) %>%
    summarize(city = first(inventor_city)) %>%
    group_by(city) %>%
    count(city) %>%
    arrange(desc(n)) %>%
    rename("number_of_patents" = n)
}

patents <- countPatents(searchYear = 2018)