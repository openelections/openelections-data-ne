library(tabulizer)
library(tesseract)

candidateLookup <- c('Trump/Pence','Clinton/Kaine','Johnson/Weld','Stein/Baraka','WRITE-IN')
partyLookup <- c('Republican','Democrat','Libertarian','By Petition',NA_character_)
precinctLookup <- c(
  '0001-First Ward',
  '0002-Second Ward',
  '0003-Third Ward',
  '0004-Fourth Ward',
  '0005-Fifth Ward',
  '0006-Brule',
  '0007-Logan',
  '0008-Lonergan',
  '0009-Paxton',
  '0010-Rural Ogalala',
  '0011-Whitetail',
  '0013-Absentee'
)

# tabulizer package did not succeed with this file; converted it w browser tabula
KeithCounty <- suppressMessages(read_csv('transformed/tabula-Keith NE EMR report for public records request-President.csv', col_names=FALSE)) %>%
  mutate(
    candidate=candidateLookup[rep(1:5, 12)],
    party=partyLookup[rep(1:5, 12)],
    precinct=precinctLookup[rep(1:12, rep(5,12))],
    votes=X3,
    office='President',
    district=NA_character_,
    county='Keith'
  ) %>%
  mutate(votes=gsub(x=votes, pattern='B', replacement='8'),
         votes=gsub(x=votes, pattern='O', replacement='0'),
         votes=gsub(x=votes, pattern='I', replacement='1'),
         votes=as.integer(votes)) %>%
  select(candidate, party, precinct, votes, office, district, county)

candidateLookup <- c('Smith','WRITE-IN')
partyLookup <- c('Republican',NA_character_)
KeithCounty <- KeithCounty %>%
  bind_rows(
    suppressMessages(read_csv('transformed/tabula-Keith NE EMR report for public records request-Congress.csv', col_names=FALSE)) %>%
      mutate(
        candidate=candidateLookup[rep(1:2, 12)],
        party=partyLookup[rep(1:2, 12)],
        precinct=precinctLookup[rep(1:12, rep(2,12))],
        votes=X4,
        office='U.S. House',
        district='3',
        county='Keith'
      ) %>%
      mutate(votes=gsub(x=votes, pattern='B', replacement='8'),
             votes=gsub(x=votes, pattern='O', replacement='0'),
             votes=gsub(x=votes, pattern='I', replacement='1'),
             votes=as.integer(votes)) %>%
      select(candidate, party, precinct, votes, office, district, county)
  )

candidateLookup <- c('Karl Elmshaeuser','Steve Erdman', 'WRITE-IN')
partyLookup <- rep(NA_character_, 3)
KeithCounty <- KeithCounty %>%
  bind_rows(
    suppressMessages(read_csv('transformed/tabula-Keith NE EMR report for public records request-Leg.csv', col_names=FALSE)) %>%
      mutate(
        candidate=candidateLookup[rep(1:3, 12)],
        party=partyLookup[rep(1:3, 12)],
        precinct=precinctLookup[rep(1:12, rep(3,12))],
        votes=X3,
        office='State Senate',
        district=NA_character_,
        county='Keith'
      ) %>%
      mutate(votes=gsub(x=votes, pattern='B', replacement='8'),
             votes=gsub(x=votes, pattern='O', replacement='0'),
             votes=gsub(x=votes, pattern='I|l', replacement='1'),
             votes=as.integer(votes)) %>%
      select(candidate, party, precinct, votes, office, district, county)
  )

# created this tiff by opening the PDF in MacOS Preview, and exporting as tiff
OtoeText <- ocr('transformed/2016_Ne_otoe_General_official_results.tiff')
tempCsv <- tempfile()
OtoeLines <- read_lines(OtoeText)[c(8,10,12,14,16,19,21,23,36,38)] %>%
  gsub(x=., pattern='([A-Z][^0-9]+)([0-9].+)', replacement='\\2')
split <- strsplit(OtoeLines[9], ' ') %>% .[[1]]
OtoeLines[9] <- c(split[1:8], NA_character_, split[9], NA_character_, split[10:13]) %>% paste(collapse=' ')
split <- strsplit(OtoeLines[10], ' ') %>% .[[1]]
OtoeLines[10] <- c(split[1:8], NA_character_, split[9], NA_character_, split[10:13]) %>% paste(collapse=' ')

writeLines(OtoeLines, tempCsv)

OtoeCounty <- read_delim(tempCsv, delim=' ', col_names=FALSE) %>%
  mutate_all(gsub, pattern='O', replacement='0') %>%
  mutate_all(as.integer)

colnames(OtoeCounty) <- c('Absentee', 'Ber', 'Del', 'McW', 'Syr', 'Pal', 'Rus', 'SBr', 'NC11', 'NC12', 'NC13', 'NC14', 'NC15', 'NC16', 'Total')

OtoeCounty$candidate <- c('Trump/Pence', 'Clinton/Kaine', 'Johnson/Weld', 'Stein/Baraka', 'Write-Ins', 'Jeff Fortenberry',
                          'Daniel M. Wik', 'Write-Ins', 'Dan Watermeier', 'Write-Ins')
OtoeCounty$party <- c('Republican', 'Democrat', 'Libertarian', 'By Petition', NA_character_, 'Republican',
                      'Democrat', NA_character_, NA_character_, NA_character_)
OtoeCounty$district <- c(rep(NA_character_, 5), 1, 1, 1, rep(NA_character_, 2))
OtoeCounty$office <- c(rep('President', 5), rep('U.S. House', 3), rep('State Senate', 2))

OtoeCounty <- OtoeCounty %>%
  gather(key='precinct', value='votes', -candidate, -party, -district, -office) %>%
  mutate(county='Otoe') %>%
  select(county, precinct, candidate, party, office, district, votes) %>%
  filter(precinct != 'Total')

WebsterCounty <- extract_tables('../../../openelections-sources-ne/2016/2016_ne_webster_General_official_results.pdf', pages=1,
                                    area=list(c(119.8095,202.0476,238.0000,771.9524)), method='character') %>% unlist() %>%
  read_tsv(col_names=FALSE, n_max=7) %>% select(-X1) %>%
  mutate(office='President', district=NA_character_, party=c('Republican', 'Democrat', 'Libertarian', 'By Petition', NA_character_,
                                                             NA_character_, NA_character_)) %>%
  bind_rows(
    WebsterCountyRep <- extract_tables('../../../openelections-sources-ne/2016/2016_ne_webster_General_official_results.pdf', pages=1,
                                       area=list(c(250.9524,203.6667,327.0476,772)), method='character') %>% unlist() %>%
      read_tsv(col_names=FALSE, n_max=4) %>% select(-X1) %>%
      mutate(office='U.S. House', district='3', party='Republican')
  ) %>% mutate(county='Webster') %>%
  rename(candidate=X2, Bladen=X3, `Blue Hill`=X4, `Guide Rock`=X5, `RC 1st`=X6, `RC 2nd`=X7, Absentee=X8) %>%
  select(-X9) %>%
  gather(key='precinct', value='votes', -candidate, -party, -district, -office, -county) %>%
  select(county, precinct, candidate, party, office, district, votes)

precincts <- c('Bartley','Beaver','Indianola','Northwest','Southwest',
               'W-1 P-1','W-1 P-2','W-1 P-3','W-2 P-1','W-2 P-2','W-3 P-1','W-3 P-3','W-4 P-1','Early Voters','Provisional')
RedWillowCounty <- extract_tables('../../../openelections-sources-ne/2016/2016_ne_redwillow_General_official_results.pdf', pages=1,
                                  area=list(c(346.6667,171.0000,418.6667,587.3333)), method='character') %>% unlist() %>%
  read_tsv(col_names=FALSE) %>%
  select(-X16) %>%
  set_names(precincts) %>%
  mutate(office='President', district=NA_character_, party=c('Republican', 'Democrat', 'Libertarian', 'By Petition', NA_character_)) %>%
  bind_rows(
    extract_tables('../../../openelections-sources-ne/2016/2016_ne_redwillow_General_official_results.pdf', pages=1,
                                  area=list(c(441,166,480,590)), method='character') %>% unlist() %>%
      read_tsv(col_names=FALSE) %>%
      select(-X16) %>%
      set_names(precincts) %>%
      mutate(office='U.S. House', district='3', party='Republican')
  ) %>% mutate(county='Red Willow', candidate=c('Trump/Pence','Clinton/Kaine','Johnson/Weld', 'Stein/Baraka', 'Write-In',
                                                'Adrian Smith', 'Write-In')) %>%
  gather(key='precinct', value='votes', -candidate, -party, -district, -office, -county) %>%
  select(county, precinct, candidate, party, office, district, votes)

HoltCounty <- extract_tables('../../../openelections-sources-ne/2016/2016_ne_holt_General_official_results.pdf', pages=1,
                             area=list(c(262,371,391.8095,959.7619)), method='character') %>% unlist() %>%
  read_tsv(col_names=FALSE) %>%
  filter(is.na(X1) | X1 != 'Total') %>%
  set_names(c('Hand Count', '1-Page', '2-Atkinson', '3-Chambers', '4-KC Hall', '5-Ewing', '6-Inman', '7-Stuart',
              '8-KC Hall', '9-Atk Ward I', '10-NECC Ward I', '11-NECC Ward II', '12-O\'Neill Armory Ward III',
              'O\'Neill Armory Ward IV', 'New/Former Resident', 'Provisional', 'Military', 'Early Voter', 'Totals')) %>%
  mutate(county='Holt', candidate=c('Trump/Pence','Clinton/Kaine','Johnson/Weld', 'Stein/Baraka', 'Adrian Smith'),
         party=c('Republican', 'Democrat', 'Libertarian', 'By Petition', 'Republican'),
         office=c(rep('President', 4), 'U.S. House'),
         district=c(rep(NA_character_, 4), '3')) %>%
  select(-Totals) %>%
  gather(key='precinct', value='votes', -candidate, -party, -district, -office, -county) %>%
  select(county, precinct, candidate, party, office, district, votes)
