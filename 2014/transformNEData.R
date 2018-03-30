      library(readxl) # needed for read_excel
      library(tidyr)  # needed for gather
      library(reshape2) # needed for colsplit
      library(dplyr) # needed for filter
      library(readr) # needed for write_csv

      findOfficeStartRow <- function(DF, office) {
	  row <- which(apply(DF, 1, function(x) any(grepl(office, x))))
	  return(row[1] + 1)  # the desired block is first instance of office
			      # block starts in the next row
      }

      findTotalsRows <- function(DF) {
	  return( which(apply(DF, 2, function(x) grepl("Total", x))))
      } # this function is only used inside the findOfficeEndRow function

      findOfficeEndRow <- function(DF, office) {
	  start <- findOfficeStartRow(DF, office)
	  totalsRows <- findTotalsRows(DF)
	  endrow <- min(totalsRows[ totalsRows > start ])
	  return(endrow - 1) # don't include the Totals row
      }

      findTotalsColumns <- function(DF, office) {
	  sr <- findOfficeStartRow(DF, office)
	  return( which(apply(countyDF[sr,], 1, function(x) grepl("Total", x))))
      }

      findOfficeLeftBlocks <- function(DF, office) {
	  tc <- findTotalsColumns(DF, office)
	  return <- c(2, tc[-length(tc)]+1)   # prepend 2, drop last column,
					      # return each next column
      }

      findOfficeRightBlocks <- function(DF, office) {
	  tc <- findTotalsColumns(DF, office)
	  return( tc-1 )  # return each previous column
      }

      neCounties <- c("Adams", "Antelope", "Arthur", "Banner", "Blaine",
      		"Boone", "Box Butte", "Boyd", "Brown", "Buffalo",
      		"Burt", "Butler", "Cass", "Cedar", "Chase", "Cherry",
      		"Cheyenne", "Clay", "Colfax", "Cuming", "Custer",
      		"Dakota", "Dawes", "Dawson", "Deuel", "Dixon",
      		"Dodge", "Douglas", "Dundy", "Fillmore", "Franklin",
      		"Frontier", "Furnas", "Gage", "Garden", "Garfield",
      		"Gosper", "Grant", "Greeley", "Hall", "Hamilton",
      		"Harlan", "Hayes", "Hitchcock", "Holt", "Hooker",
      		"Howard", "Jefferson", "Johnson", "Kearney", "Keith",
      		"Keya Paha", "Kimball", "Knox", "Lancaster",
      		"Lincoln", "Logan", "Loup", "McPherson", "Madison",
      		"Merrick", "Morrill", "Nance", "Nemaha", "Nuckolls",
      		"Otoe", "Pawnee", "Perkins", "Phelps", "Pierce",
      		"Platte", "Polk", "Red Willow", "Richardson", "Rock",
      		"Saline", "Sarpy", "Saunders", "Scotts Bluff",
      		"Seward", "Sheridan", "Sherman", "Sioux", "Stanton",
      		"Thayer", "Thomas", "Thurston", "Valley",
      		"Washington", "Wayne", "Webster", "Wheeler", "York")

      congDist <- list("Adams"=3, "Antelope"=3, "Arthur"=3,
		       "Banner"=3, "Blaine"=3, "Boone"=3, "Box Butte"=3,
		       "Boyd"=3, "Brown"=3, "Buffalo"=3, "Burt"=1, "Butler"=1,
		       "Cass"=1, "Cedar"=3, "Chase"=3, "Cherry"=3,
		       "Cheyenne"=3, "Clay"=3, "Colfax"=1, "Cuming"=1, "Custer"=3,
		       "Dakota"=3, "Dawes"=3, "Dawson"=3, "Deuel"=3,
		       "Dixon"=3, "Dodge"=1, "Douglas"=2, "Dundy"=3,
		       "Fillmore"=3, "Franklin"=3, "Frontier"=3, "Furnas"=3,
		       "Gage"=3, "Garden"=3, "Garfield"=3, "Gosper"=3,
		       "Grant"=3, "Greeley"=3,
		       "Hall"=3, "Hamilton"=3, "Harlan"=3,
		       "Hayes"=3, "Hitchcock"=3, "Holt"=3, "Hooker"=3, "Howard"=3,
		       "Jefferson"=3, "Johnson"=3,
		       "Kearney"=3, "Keith"=3, "Keya Paha"=3, "Kimball"=3,
		       "Knox"=3,
		       "Lancaster"=1, "Lincoln"=3, "Logan"=3, "Loup"=3,
		       "McPherson"=3, "Madison"=1, "Merrick"=3, "Morrill"=3,
		       "Nance"=3, "Nemaha"=3, "Nuckolls"=3,
		       "Otoe"=1,
		       "Pawnee"=3, "Perkins"=3, "Phelps"=3, "Pierce"=3,
		       "Platte"=1, "Polk"=1,
		       "Red Willow"=3, "Richardson"=3, "Rock"=3,
		       "Saline"=3, "Sarpy"=2, "Saunders"=1,
		       "Scotts Bluff"=3, "Seward"=1,
		       "Sheridan"=3, "Sherman"=3, "Sioux"=3, "Stanton"=1,
		       "Thayer"=3, "Thomas"=3, "Thurston"=1,
		       "Valley"=3,
		       "Washington"=1, "Wayne"=3, "Webster"=3, "Wheeler"=3,
		       "York"=3)

      congDist1Sarpy <- list("Precinct 1$", "Precinct 2$", "Precinct 3$", 
			    "Precinct 4$", "Precinct 5$", "Precinct 6$", 
			    "Precinct 7$", "Precinct 8$", "Precinct 9$", 
			    "Precinct 10$", "Precinct 11$", "Precinct 12$", 
			    "Precinct 13$", "Precinct 16$", "Precinct 17$", 
			    "Precinct 18$", "Precinct 19$", "Precinct 20$",
			    "Precinct 21$", "Precinct 22$", "Precinct 24$",
			    "Precinct 25$", "Precinct 26$")
    # in 2014 election there was no Precent 14 or 15.  Precinct 23 is in District 2
    # Note that parts of District 19 and 21 are in BOTH District 1 and District 2!

      offices <- c("Senator",
		   "Representative",
		   "Governor",
		   "Secretary of State",
		   "State Treasurer",
		   "Attorney General",
		   "Auditor",
		   "Legislature"
      )

      for (county in neCounties) {

	 countyPrecinctResults <- data.frame(county=character(),
					      precinct=character(),
					      office=character(),
					      district=character(),
					      candidate=character(),
					      party=character(),
					      votes=integer()
	 ) # initialize data frame for the tidy form of county results
	    # Note that this is not the final column order, a reorder comes later. 

	 fname <- paste0(county, ".xls") # note use of paste0
	 countyDF <- suppressWarnings(read_excel(fname, col_types = "text"))
	 # In some counties, e.g. Douglas, with multiple districts for
	 # Legislature or the Natural Resource Districts, the strings in
	 # race heading rows and list of candidates appear in columns
	 # which previously were empty or had other variable types.  This
	 # causes read_excel to issue many warning messages.  One guard to
	 # fix this is to explicitly declare col_types as text.  I
	 # additionally wrap the read_excel() with suppressWarnings() to
	 # eliminate the warnings.

          for (office in offices) {
              if (as.logical(sum(grepl(office, countyDF)))) {
              # check to see if county has an office race;
              # if TRUE, proceed

	     startRow <- findOfficeStartRow(countyDF, office)
	     endRow <- findOfficeEndRow(countyDF, office)
	     leftBlocks <- findOfficeLeftBlocks(countyDF, office)
	     rightBlocks <- findOfficeRightBlocks(countyDF, office)
	     if (office == "Legislature") {
                 districtsLD <- 
                     sub("For Member of the Legislature - ", "", 
                         grep("LD*", countyDF[startRow-1,], value=TRUE))
             }

	     for (race in 1:length(leftBlocks)) {
		 leftEdge <- leftBlocks[race]
		 rightEdge <- rightBlocks[race] 

		 cands <- c("precinct",
			    unname(unlist(countyDF[startRow,leftEdge:rightEdge])))

		 officeResults <- countyDF[(startRow+1):endRow, leftEdge:rightEdge]
					    # careful with operator precedence!
		 officeResults <- cbind(countyDF[(startRow+1):endRow, 1], officeResults)


		 colnames(officeResults) <- cands

		 # here is the tidyverse magic that turns the spreadsheet
		 # array into tidy data
		 officeTibble <-
		     gather(officeResults, key=candparty, value=votes, -precinct)

		 newColNames <- c("candidate", "party")
		 newCols <- colsplit(officeTibble$candparty, " \r\n", newColNames)
		 newCols$candidate <- gsub("[ ]+", " ", newCols$candidate)
		 # many names have two spaces between first and last,
		 # seems inconvenient for search so clean it up here with
		 # a substitution

		 officeTibble <- cbind(officeTibble, newCols)
		 officeTibble$candparty <- NULL
		 # eliminate the old candidate and party combined column

		 officeTibble$county <- rep(county, NROW(officeTibble))
		 officeTibble$office <- rep(office, NROW(officeTibble))

		 if (office == "Legislature") {
		     officeTibble$district <-
			 rep(districtsLD[race], NROW(officeTibble))
		 } else { 
		     officeTibble$district <-
			 rep(congDist[[county]], NROW(officeTibble))
		   if (county == "Sarpy")  {
		   for (precinct in congDist1Sarpy) {
		       rowsPrecinct <- grep(precinct, officeTibble$precinct)
		       officeTibble$district[rowsPrecinct] <- 1 
                   }    # fix the 23 precincts in Sarpy that 
		       # are actually in Congressional District 1
                   }
                 }
                 
		 # reorder columns
		 officeTibble <- officeTibble[c("county", "precinct", "office", 
				     "district", "party", "candidate", "votes")]

		 countyPrecinctResults <- rbind(countyPrecinctResults, officeTibble)
		 # append current office results to the county results
	     }
          }
	 # convert votes column to integer, got read in as chr, somehow, somewhere
             countyPrecinctResults$votes <- as.numeric(countyPrecinctResults$votes)
         }   
	 lcCounty <- tolower(gsub(" ", "_", county))
	 csvName <- paste0("20141104_ne_general_", lcCounty, "_precinct.csv")
	 write_csv(countyPrecinctResults, csvName)
      }
