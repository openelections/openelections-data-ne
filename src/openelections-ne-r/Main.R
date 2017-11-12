source('Manual.R')
source('PDF.R')

dfs <- ls()[endsWith(ls(), 'County')]

walk(dfs, function(dfName) {
  cdf <- get(dfName)
  county <- cdf[[1,'county']] %>% tolower() %>% gsub(x=., pattern=' ', replacement='_')
  fn <- paste0('20161108__ne__general__', county, '__precinct.csv')
  fn <- paste0('../../2016/', fn)
  writeLines(fn)
  cdf %>%
    select(county,precinct,office,district,party,candidate,votes) %>%
    write_csv(fn)
})