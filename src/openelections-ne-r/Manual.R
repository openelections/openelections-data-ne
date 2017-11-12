library(tidyverse)

# Counties in this file had results PDFs that would not parse with tabula and were not suitable for OCR
# Data entered by hand and visually checked

HitchcockCounty <- '
Donald J. Trump,Republican,President,433,125,252,260,162,1232
Hillary Clinton,Democrat,President,45,11,26,36,43,161
Gary Johnson,Libertarian,President,18,10,14,9,10,61
Jill Stein,By Petition,President,2,1,0,3,1,7
Adrian Smith,Republican,U.S. House,455,131,255,278,187,1306
' %>% read_csv(col_names=c('candidate', 'party', 'office', 'Culbertson', 'Palisade', 'Stratton', 'Trenton', 'Early', 'Total')) %>%
  gather(key='precinct', value='votes', -candidate, -party, -office) %>%
  mutate(county='Hitchcock', district=ifelse(grepl(x=office, pattern='House'), '3', NA_character_))

NanceCounty <- '
BGCP,220,38,4,0,2,216,0
BGCP/Early Votes,41,8,5,0,1,41,1
CLFWE,143,21,1,0,1,123,2
CLFWE/Early Votes,56,14,0,0,1,49,0
CTS,106,20,5,8,2,111,0
CTS/Early Votes,16,3,0,0,1,16,0
Fullerton,300,74,11,4,8,315,2
Fullerton/Early Votes,76,35,5,0,0,88,0
Genoa I,134,35,9,1,1,149,3
Genoa I/Early Votes,12,3,0,0,0,12,2
Genoa II,133,25,5,1,0,135,4
Genoa II/Early Votes,23,4,2,0,0,21,1
' %>% read_csv(col_names=c('precinct', 'Trump', 'Clinton', 'Johnson', 'Stein', 'Write-In', 'Smith', 'Write-InH')) %>%
  gather(key='candidate', value='votes', -precinct) %>%
  mutate(county='Nance',
         office=case_when(candidate %in% c('Smith', 'Write-InH') ~ 'U.S. House', TRUE ~ 'President'),
         candidate=gsub(x=candidate, pattern='InH', replacement='In'),
         district=ifelse(grepl(x=office, pattern='House'), '3', NA_character_),
         party=case_when(
           candidate %in% c('Trump', 'Smith') ~ 'Republican',
           candidate=='Clinton' ~ 'Democrat',
           candidate=='Stein' ~ 'By Petition',
           candidate=='Johnson' ~ 'Libertarian',
           TRUE ~ NA_character_
         ))

WayneCounty <- '
0001-Hoskins/Garfield,307,26,5,1,4,302,0
0002-Chapin/Hancock/Winside,312,42,13,1,2,321,6
0003-Deer Creek/Sherman,156,24,16,2,3,183,0
0004-Brenna/Plum Creek/Hunter,218,40,9,1,7,233,2
0005-Strahan/Wilbur,185,24,13,0,2,211,0
0006-Logan/Leslie,177,43,7,1,4,203,3
0007-First Ward,223,76,16,5,6,272,4
0008-Second Ward,199,73,16,7,10,244,5
0009-Third Ward,203,97,32,5,7,296,5
0010-Fourth Ward,300,145,36,5,9,414,16
0013-ABSENTEE,395,238,39,7,11,524,20
' %>% read_csv(col_names=c('precinct', 'Trump/Pence', 'Clinton/Kaine', 'Johnson/Weld', 'Stein/Baraka', 'Write-In', 'Smith', 'Write-InH')) %>%
  gather(key='candidate', value='votes', -precinct) %>%
  mutate(county='Wayne',
         office=case_when(candidate %in% c('Smith', 'Write-InH') ~ 'U.S. House', TRUE ~ 'President'),
         candidate=gsub(x=candidate, pattern='InH', replacement='In'),
         district=ifelse(grepl(x=office, pattern='House'), '3', NA_character_),
         party=case_when(
           candidate %in% c('Trump/Pence', 'Smith') ~ 'Republican',
           candidate=='Clinton/Kaine' ~ 'Democrat',
           candidate=='Stein/Baraka' ~ 'By Petition',
           candidate=='Johnson/Weld' ~ 'Libertarian',
           TRUE ~ NA_character_
         ))

SewardCounty <- '
B,139,25,10,3,1,143,37,1
C,199,59,14,2,6,231,46,0
E,340,69,25,7,9,371,67,1
G,165,54,6,0,5,185,37,0
H,375,130,27,1,9,414,121,1
I,416,100,22,12,11,448,94,1
J,150,40,7,1,3,153,42,0
K,220,67,12,1,3,241,59,0
M-BX,217,42,13,4,4,233,43,0
M-CO,175,41,5,1,2,178,46,1
O-1,312,68,18,3,9,341,66,1
O-2,415,96,21,3,6,447,84,0
S-1,542,205,33,2,19,654,136,1
S-2,537,254,22,9,24,680,156,0
S-3,412,182,32,13,25,490,160,1
S-4,255,101,25,7,11,316,75,0
Absentee,585,342,45,15,20,728,249,1
' %>% read_csv(col_names=c('precinct', 'Trump/Pence', 'Clinton/Kaine', 'Johnson/Weld', 'Stein/Baraka', 'Write-In', 'Jeff Fortenberry', 'Daniel M. Wik', 'Write-InH')) %>%
  gather(key='candidate', value='votes', -precinct) %>%
  mutate(county='Seward',
         office=case_when(candidate %in% c('Jeff Fortenberry', 'Daniel M. Wik', 'Write-InH') ~ 'U.S. House', TRUE ~ 'President'),
         candidate=gsub(x=candidate, pattern='InH', replacement='In'),
         district=ifelse(grepl(x=office, pattern='House'), '1', NA_character_),
         party=case_when(
           candidate %in% c('Trump/Pence', 'Jeff Fortenberry') ~ 'Republican',
           candidate %in% c('Clinton/Kaine', 'Daniel M. Wik') ~ 'Democrat',
           candidate=='Stein/Baraka' ~ 'By Petition',
           candidate=='Johnson/Weld' ~ 'Libertarian',
           TRUE ~ NA_character_
         ))

PlatteCounty <- '
1A,431,122,26,7,10,450,133,1
1B,271,67,10,1,12,279,65,0
1C,301,73,20,5,7,311,73,0
1D,328,81,15,5,6,321,93,0
2A,176,51,18,5,6,191,59,1
2B,406,78,18,3,13,422,80,0
2C,297,89,18,2,7,314,86,1
2D,256,97,24,1,8,297,81,0
3A,534,107,19,3,10,572,87,0
3B,258,92,17,5,5,289,81,1
3C,515,87,22,2,13,531,92,0
3D,489,107,17,1,10,508,95,2
4A,258,92,32,5,5,278,108,2
4B,330,95,13,2,9,358,90,1
4C,444,116,21,1,14,484,88,1
4D,255,74,13,5,5,269,71,0
Columbus Twp A,330,55,12,2,9,334,58,1
Columbus Twp B,542,97,17,4,15,592,74,2
Bismark,185,20,6,0,3,186,18,0
Sherman/Grand Prairie,253,16,6,2,1,247,14,0
Creston,162,7,2,0,3,152,14,0
Shell Creek,309,43,7,2,4,299,57,0
Humphrey,159,18,0,2,1,154,13,0
Butler/Loup,242,34,6,0,6,229,42,1
Oconee/Monroe,225,37,8,1,2,225,39,1
Lost Creek/Burrows,249,29,11,3,0,240,36,0
Granville,403,44,8,0,5,388,39,0
Joliet,53,2,1,0,1,54,2,0
St Bernard,234,8,7,1,3,220,19,0
Walker/Woodville,152,15,5,0,3,159,15,0
Absentee,1917,793,96,16,48,2036,646,5
' %>% read_csv(col_names=c('precinct', 'Trump/Pence', 'Clinton/Kaine', 'Johnson/Weld', 'Stein/Baraka', 'Write-In', 'Jeff Fortenberry', 'Daniel M. Wik', 'Write-InH')) %>%
  gather(key='candidate', value='votes', -precinct) %>%
  mutate(county='Platte',
         office=case_when(candidate %in% c('Jeff Fortenberry', 'Daniel M. Wik', 'Write-InH') ~ 'U.S. House', TRUE ~ 'President'),
         candidate=gsub(x=candidate, pattern='InH', replacement='In'),
         district=ifelse(grepl(x=office, pattern='House'), '1', NA_character_),
         party=case_when(
           candidate %in% c('Trump/Pence', 'Jeff Fortenberry') ~ 'Republican',
           candidate %in% c('Clinton/Kaine', 'Daniel M. Wik') ~ 'Democrat',
           candidate=='Stein/Baraka' ~ 'By Petition',
           candidate=='Johnson/Weld' ~ 'Libertarian',
           TRUE ~ NA_character_
         ))

PawneeCounty <- '
Pawnee 1,239,93,19,5,3,290,1,292,3
Pawnee 1-Absentee,48,27,6,1,2,56,0,64,0
Pawnee 2,218,81,14,2,2,264,6,261,3
Pawnee 2-Absentee,31,11,3,0,0,42,0,37,0
Burchard,160,24,6,0,0,169,1,164,0
Burchard-Absentee,27,3,1,0,0,28,0,27,0
Table Rock,167,29,8,2,0,175,4,168,2
Table Rock-Absentee,17,4,2,0,0,20,2,19,1
Steinauer,186,52,8,1,4,210,5,211,1
Steinauer-Absentee,25,13,1,1,3,37,1,35,1
' %>% read_csv(col_names=c('precinct', 'Trump/Pence', 'Clinton/Kaine', 'Johnson/Weld', 'Stein/Baraka', 'Write-In', 'Adrian Smith', 'Write-InH', 'Dan Watermeier', 'Write-InS')) %>%
  gather(key='candidate', value='votes', -precinct) %>%
  mutate(county='Pawnee',
         office=case_when(candidate %in% c('Adrian Smith', 'Write-InH') ~ 'U.S. House',
                          candidate %in% c('Dan Watermeier', 'Write-InS') ~ 'State Senate',
                          TRUE ~ 'President'),
         candidate=gsub(x=candidate, pattern='InH', replacement='In'),
         candidate=gsub(x=candidate, pattern='InS', replacement='In'),
         district=ifelse(grepl(x=office, pattern='House'), '3', NA_character_),
         party=case_when(
           candidate %in% c('Trump/Pence', 'Adrian Smith') ~ 'Republican',
           candidate=='Clinton/Kaine' ~ 'Democrat',
           candidate=='Stein/Baraka' ~ 'By Petition',
           candidate=='Johnson/Weld' ~ 'Libertarian',
           TRUE ~ NA_character_
         ))
