library(tidyverse)
offices <- "Representative|Governor|Secretary of State|State Treasurer|Attorney General|Auditor|Legislature"


gen2010 <- read_csv("2010 General Election by Precinct.csv")

distmap <- gen2010 %>% filter(grepl("FED", X9) & Party=="REP1") %>%
    select(County, PrecinctID, X9) %>%
    mutate(CtyPrec = paste0(County, PrecinctID)) %>%
    mutate(X9 = substr(X9, nchar(X9), nchar(X9))) 
distList <- setNames(distmap$X9, distmap$CtyPrec)

gen2010 <- mutate(gen2010, CtyPrec = paste0(County, PrecinctID))
gen2010 <- mutate(gen2010, CongDist = distList[CtyPrec])

ne2010  <-gen2010 %>% 
            select(County, PrecinctID, RaceName, X9, X10, CongDist, Party, CandidateFirst, CandidateLast, Votes) %>%
            filter(grepl(offices, RaceName)) %>%
            mutate(Candidate = paste(CandidateFirst, CandidateLast)) %>%
            mutate(Party = substr(Party,1,3)) %>% 
            mutate(Office = substr(RaceName, 5, length(RaceName))) %>% 
            mutate(Precinct = ifelse(nchar(PrecinctID) > 4, substr(PrecinctID,1,4), PrecinctID)) %>%
            mutate(X9 = paste0(X9, X10)) %>%
            mutate(legdist=ifelse(grepl("LEG", X9), 
                            substr(X9,nchar(X9)-2, nchar(X9)), substr(X9, 1,3))) %>%
            select(county = County, precinct = Precinct, office = Office, 
                   district = CongDist, party = Party, candidate = Candidate, votes = Votes, legdist)

  ne2010 <- ne2010 %>%
            mutate(legdist = sub(" ", "", legdist)) %>%
            mutate(legdist = sub("NA", "", legdist)) %>%
            mutate(office = ifelse(grepl("Governor", office),       substr(office,1, 8),  office)) %>%
            mutate(office = ifelse(grepl("Representative", office), substr(office,1, 14), office)) %>%
            mutate(office = ifelse(grepl("Auditor", office),        substr(office,1, 7),  office)) %>%
            mutate(office = ifelse(grepl("Legislature", office),    substr(office,15,25), office))

 ne2010 <- ne2010 %>%
           mutate(district =
             ifelse(county=="Cedar" & precinct=="0017" 
                    & grepl("Smith|Davis|Hill", candidate), 
             3, district))

write_csv(ne2010, "20101102_ne_general_statewide_precinct.csv")
