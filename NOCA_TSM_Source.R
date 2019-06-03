###########################################################
# NOCA_TSM_Source.R: 
#
# This program does two big things. 
# First, it cleans up the database
# Second it does a TSM analysis on the banding data. 
#
###########################################################

###########################################################
# Clean up the R environment so we are staring from scratch
#
###########################################################
rm(list=ls())
assign("last.warning", NULL, envir = baseenv())

###########################################################
# Get the programming environment setup including setting
# user defined variables.
# 
###########################################################
# Load the nessisary R packages. I'm ont sure all of these are currently
# being used in this version, but they have been used in the past. 
library(RMark)
library(ggplot2)   
library(msm)
library(data.table)
library(stringi)
library(stringr)
#library(rlist)
library(plyr)
#library(multcomp)
#library(lsmeans)
library(dplyr)
#library(sendmailR)

# Set the name of the combined labo/malaria database
filenameBirbs = 'labo database malaria.csv'

################################################################################
# Step 1: Load in the data
#
################################################################################
    # Print a message to the screen
    cat('NOCA_TSM_Source.R: Msg: Step 1: Reading CSV data\n')

    # Actually load the data. stringsAsFactors=F tells R to no convert strings to factors
    # which is a better approach in most cases. 
    birbs <- read.csv(filenameBirbs, stringsAsFactors=FALSE)

################################################################################
# Step 2: Clean up the data. 
#
# There are several steps we need to do to clean up the data. 
#
# First, we need to fix some basic features of the data at a global level.
# We need to fix a few column names, make sure dates are treated as dates.
#
# Next we need to fix a few anomolies in the date date. In particular
# there are some back-to-back banding occations that I think are probably
# recording errors. 
#
# Next we need to strip the data down to just those records that we really 
# care aboue for this analysis. In this case we just want NOCAs from
# Bluebonnet. 
#
# Next we need to fix some anomolies in the records. Note that a "record"
# is a record of capture. A single bird might have multiple records. 
#
# To fix the anomolies we need to:
#    1) Remove redunent records. Some captures are recorded twice in the database
#       and we need to remove these. 
#
#    2) Fix sex anomolies for birds. Some birds have multiple sex designations.
#
#    3) Fix age anomolies for birds. Make sure ages are chronological and fix a few other things. 
#
#    4) 
#
################################################################################
    # Print out a status message so we know where we are inthe script. 
    cat('NOCA_TSM_Source.R: Msg: Step 2: Clean the data\n')

################################################################################
# Step 2.1: Rename columns
#
################################################################################
    # Change the column name "BandingStation" to just "Station"
    setnames(birbs, "BandingStation", "Station")

    # Change the column name "BandingDate" to just "Date"
    setnames(birbs, "BandingDate", "Date")

################################################################################
# Step 2.2: Recast columns to their correct type 
#
# The exception to this is casting columns to factors
# which is done below. 
################################################################################
    birbs$Date = paste(birbs$Year, birbs$Month, birbs$Day, sep = "-")
    birbs$Date = as.Date(birbs$Date, "%Y-%m-%d")

# This block fixed a certain kind of problem with the sex data
# This is better delt with below. 
    # Find the birds that have no sex code assigned to them.
    # I don't think this line of code does anything at this point. 
    # It doesn't hurt anything, but it isn't used either. 
    #index = birbs$Sex == ''

    # Set the sex of all birds without any sex code to "U"
    #birbs$Sex[birbs$Sex== ""] = "U"

    # Set the age of all birds without any age code to "U"
    #birbs$Age[birbs$Age== ""] = "U"

################################################################################
# Step 2.4: Fix some broken dates.
#
################################################################################

    # We need to fix the dates before we narrow the data base because
    # date is one criteria used to narrow the data base, so we need to 
    # make sure they are correct before going any further. 

    # The database records two instances of back-to-back banding dates. 
    # One pair of dates includes April 16 and 17, 2011.
    # The other pair is February 27 and 28, 2014. 
    # I suspect that some of this is misrecorded data. But maybe not.
    # At any rate, for simplicity, we'll merge each pair into a single date.

    # This pair is a Monday and a Tuesday. 
    birbs$Date[birbs$Date == as.Date("2011-04-17")] = as.Date("2011-04-16")

    # This pair is a Thrusday and a Friday
    birbs$Date[birbs$Date == as.Date("2014-02-28")] = as.Date("2014-02-27")

    # Check for other back-to-back banding dates.
    # Need to do this per station.
    stationList = list( c('HOME','BAMB'), 'PISP')
    for ( st in stationList ) {
        cat('NOCA_TSM_Source.R:: Msg: Checking back-to-back banding dates for station(s):'); print(st)
        temp = subset(birbs, Station%in%st)

        dateCheck = tibble( Date = temp$Date %>% unique() %>% sort(),
                            delta = c(0, Date[2:length(Date)] - Date[2:length(Date) - 1 ]) )
        if ( sum(dateCheck$delta == 1) > 1 ) {
            cat('NOCA_TSM_Source.R: Warning : There are ',sum(dateCheck$delta == 1),' back-to-back banding dates.\n')
            print( subset(dateCheck, delta == 1) )
        } else {
            cat('NOCA_TSM_Source.R: Msg: There are ',sum(dateCheck$delta == 1),' back-to-back banding dates.\n')
        } 
    }
    # Clean up the namespace
    remove(list = c('dateCheck'))


################################################################################
# Step 2.3: Strip the data down to just the species, station and dates that we want
#
# Eric, 
#     I know you don't want to do things this way, but I don't really understand
#     why you want to process all the birds at once. Moreover, your code below is very 
#     confusing since you keep switching back and forth between the entire bird
#     database and just the NOCAs. But you strip out Palmetto. I just don't get it. 
#
#  IMPORTANT:
#  IMPORTANT:
#  IMPORTANT:
#  IMPORTANT:
#  IMPORTANT: Get the capture occations out of the data 
#  IMPORTANT:
#  IMPORTANT:
#  IMPORTANT:
#  IMPORTANT:
#
#
#
################################################################################

    working = subset( birbs, SpeciesCode == 'NOCA' & Station %in% c('HOME','BAMB') & Date > as.Date('2010-03-30') ) 

# Deal with sex in one place. See below.
    ###########################################################
    # Do a sanity check on the database and find any birds that
    # have sex codes other that M, F, or U.
    ###########################################################

    # Make a subset of the data with birds with invalid sex codes
    # unknownSexBirds = subset( birbs, !(Sex%in%c('M','F','U')) )

    # If the invalid sex code data set has one or more entires, print out an 
    # error message. 
    #if ( nrow(unknownSexBirds) ) {
    #    # The error message
    #    cat('NOCA_TSM_Source.R: Error: There are birds with sex codes other than M/F/U\n')
    #    # Record the problem birds for subsequent follow up.
    #    # Check to see if the problemSexBirds.csv file exists. If it does, remove it.
    #    if ( file.exists('problemSexBirds.csv') ) { file.remove('problemSexBirds.csv') }
    #    # Write the problemSexBirds.csv data. 
    #    write.csv(unknownSexBirds, 'problemSexBirds.csv', row.names=F)
    #}


################################################################################
# Step 2.4: Find report and remove the number of same day recaptures.  
#
################################################################################
    index = working$Code == 'S'
    cat('NOCA_TSM_Source.R: Msg: Number of same day recaptures = ', sum(index), '\n')

    # Remove same day recapture records since they don't really contribute to 
    # survivorship estimates. While we could in principle use this data, I don't 
    # think it is a good idea for numerical reasons. 
    cat('NOCA_TSM_Source.R: Msg: Removing same day recaps\n')
    working <- working[which(working$Code != "S"),]

################################################################################
# Step 2.5: Fix any date errors in the database. 
# 
# This section finds captures that got double recorded in the LABO database
# and removes them. These are not same-day recaptures, they are errors in the LABO database. 
#
################################################################################
    # Create an auxiliar data.frame that has one row for each band number and the list of capture dates
    # associated with that band number. 
    dateDouble = aggregate(Date~BandNumber, working, paste, collapse=",")

    # Cycle over the unique band numbers here. For each
    # band number, check the list of dates. If there are replicates 
    # remove the redudent records. 
    #
    # There are more efficient ways to do this in R, but this
    # works and is sensible. 
    for (i in 1:nrow(dateDouble)){

        # Get the band number
        bandNum        = dateDouble$BandNumber[i] 
 
        # Split dates for each bird into a list
        capDates       = strsplit(dateDouble$Date[i], ",", fixed = TRUE) %>% unlist()

        # SMD: This whole section is rather clever
        # Make a table out of each of these dates
        capDatesTable  = table(capDates) 

        # Find the dates with that occur more than once. 
        dupCapDates    = names(capDatesTable)[ capDatesTable > 1 ]
        
        # Cycle over the duplicate band/date records and remove the duplicates. 
        for ( date in dupCapDates ) {
            # index will hold the record number of all the duplicated records
            index = which( working$BandNumber == bandNum & working$Date == date ) 
            # Get all the duplicates except the first record
            index = index[2:length(index)]
            # Drop all of the duplicate records
            working = working[ -index,  ]
        }
    }

    # Now double check that everything is OK. If there are still errors
    # at this point, they will require human intervension to fix. 
    # Print out a list of problem records.
    dateDouble            = aggregate(Date~BandNumber, working, paste, collapse=",")
    dateDouble$hasProblem = 0

    for (i in 1:nrow(dateDouble)){
      
      bandNum        = dateDouble$BandNumber[i]
      capDates       = strsplit(dateDouble$Date[i], ",", fixed = TRUE)[[1]]
      capDatesTable  = table(capDates)
      dupCapDates    = names(capDatesTable)[ capDatesTable > 1 ]
      
      if ( length(dupCapDates) != 0  ) { dateDouble$hasProblem[i] = 1 }
      
    }

    problemBirds = subset(dateDouble, hasProblem == 1 )

    if ( file.exists('NOCA_TSM_Source_01_date_double.csv') ) { file.remove('NOCA_TSM_Source_01_date_double.csv', showWarnings = F) }
    if ( nrow(problemBirds) != 0  ) {
      print("MSM_Tobin_Original.R: Msg: There are birds with double dates\n")
      problemRecord = subset(working, BandNumber %in% problemBirds$BandNumber)
      write.csv(problemRecord, 'NOCA_TSM_Source_01_date_double.csv', row.names=F, quote=F)
    }

    # Clean up the namespace. 
    remove(list = c('dateDouble','problemBirds'))


    ################################################################################
    # Step 1: Set your Site (Includes BREC peculiarities)
    #
    ################################################################################
    # Removing all dates before 2010 April to remove inital few capture session
    # SMD: Why?
    #
    # Including this now since it removes a lot of the doubles. 
    #birbs = subset(birbs,Date >= as.Date('2010-03-30') )

    # Extract just the Bluebonnet data. 
    #birbs            <- subset(birbs,Station=="HOME"|Station=="BAMB")
    #cat('NOCA_TSM_Source.R: Msg: Station is set to HOME/BAMB\n')

################################################################################
# Step 2.5: Check net 99
#
################################################################################
    # We need to check how many NOCAs are we recaptured in net 99.
    # Net 99 is a designator of all one-off nets used during banding.
    # We need to find out how many captures and how many birds were
    # captured in these special nets to assess whether or not they 
    # will have a significant impact on the results and if we need
    # to modify the model structure to account for them. 
    # Refer to project log v.2 on 21feb2019 to see if this needs changing
    cat('NOCA_TSM_Source.R: Msg: Number of NOCA captures in net 99 = ', sum(working$Net == 99), '\n' )
    cat('NOCA_TSM_Source.R: Msg: Number of unique NOCAs in net 99  = ', subset(working, Net == 99)$BandNumber %>% unique() %>% length(), '\n' )



#################################################################
# Step 2.6: Sex sanity check. (For NOCA only.)
#
# There are some birds with ambiguous sex records. We need to fix these. 
#
#
#################################################################

    # We are already only using NOCAs, so no need for this. 
    # First, make a subset that is just the NOCAs
    #NOCA <- birbs[which(birbs$SpeciesCode == "NOCA"),]

    # Step 1: Fix blank sex records
    #
    # An inspection of the sex records for birds reveals that there are 
    # four values for sex: "M", "F", "U" and "".
    # We are going to reclassify the "" records as "U".
    #
    working$Sex[ working$Sex == ''] = 'U'

    # Step 2: Find all birds with ambiguous sex records and fix them.

    # Step 2.1:
    #
    # Make an auxiliary data.frame of unique band numbers and list all the sex catagories for each bird.
    # If a bird was only every catagorized as one sex, then the Sex column will have just one
    # entry. If the bird was catagoried with more than one sex catagory, then there will be
    # multiple entries for sex. 
    #
    sexDF        = aggregate(Sex~BandNumber, working, function(sex) { sex %>% unlist() %>% sort() %>% unique() %>% paste(collapse=',')} )

    # Step 2.2: Find out how many sex catagories are associated with each band
    sexDF$numSex = apply(sexDF, 1, function(row) { row[2] %>% strsplit(split=',') %>% unlist() %>% length() })

    # Step 2.3: Keep only the band numbers that have more than one sex catagory
    sexDF        = subset(sexDF, numSex != 1 ) 

    # Step 2.4: "Fix" ambiguous records
    if ( nrow(sexDF) != 0 ) {
        # Let the user know there is a problem 
        cat('NOCA_TSM_Source.R: Msg: There are', nrow(sexDF), 'birds that have multiple sex catagories\n')
        cat('NOCA_TSM_Source.R: Msg: Begin resexing','\n')

        # Cycle over the band numbers with ambiguous records. 
        for ( bn in sexDF$BandNumber ) {
            # Get the index for the records associated with this bird 
            index = which( working$BandNumber == bn)

            # Find the number of records for this bird
            numRecords = nrow( subset(working, BandNumber == bn ) )

            # Find the number of times the bird was classified as male
            numAsMale   = nrow( subset(working, BandNumber == bn & Sex == 'M' ) )

            # Find the number of times the bird was classified as female
            numAsFemale = nrow( subset(working, BandNumber == bn & Sex == 'F' ) )
            
            # Find the number of times the bird was classified as unknown
            numAsNeuter = nrow( subset(working, BandNumber == bn & Sex == 'U') )
            
            if ( numAsNeuter == numRecords ) {
                # If the bird was always classed as U, there is nothing we can do.
                # In the else part, we know that the bird was assigned at least one
                # of male or female. So we will select the sex based on which 
                # catagory had the most hits
                cat('NOCA_TSM_Source.R: Msg: Found a bird with only sex == U, band number = ',bn,'\n')

            } else if ( numAsMale > numAsFemale ) {
                working$Sex[index] = 'M'

            } else if ( numAsMale < numAsFemale ) {
                working$Sex[index] = 'F'

            } else {
                # In this case numAsMale == numAsFemale, so sex = U
                # We might be able to solve this by checking CP, BP, weight and wing length 
                # For now, we'll just report the number of such birds.
                working$Sex[index] = 'U'
                cat('NOCA_TSM_Source.R: Warning: Found a bird with #M == #F, band number = ',bn,'\n')
            }
        }
    } else {
        cat('NOCA_TSM_Source.R: Msg: All birds have unique sex\n')
    }

################################################################################
# Step 2.7: Age Sanity check
# 13 Feb 2019
#
# OK. I think the point of this section of code is to do a couple of things. 
# 
# First, we need to simplify the age class system. An inspection of the entire
# LABO database revealse the following age classes:
# "A" "O" "S" "U" "T" "Y" "L" ""  "H" "J" "F" "K" "X"
# 
# These mean:
# A = after hatch-year
# 0 = after second-year
# S = second-year
# U = unknown
# T = third year
# Y = after third year
# L = local (?)
# '' = missing date
# H = hatch-year
# J = juvenile ?
# F = ? (Formative?)
# K = ???
# X = not attempted
#
# For our current analysis, we don't need this many codes. We basically need juvenile and adult. 
# So, we will do the following recatigorization:
#
# A = Adult (A)
# O = A
# S = A
# U = Unknown (U)
# T = A
# Y = A
# L = Juvenile (J)
# '' = U
# H = J
# J = J
# F = U
# K = U
# X = U
#
# However, this approach may not be appropriate for all species. So we'll print a 
# warning to the user. 
#
#
################################################################################
    # subset the data down to just the NOCAs
    #NOCA             <- subset( birbs, SpeciesCode == "NOCA" )

    # Get the unqiue age classes. 
    ages             <- unique( working$Age )

    cat('NOCA_TSM_Source.R: Msg: NOCA age classes, before sanitation and transformation:',ages,'\n')

    cat('NOCA_TSM_Source.R: WARNING: #########################################################\n') 
    cat('NOCA_TSM_Source.R: WARNING: Reclassifying age classes, make sure this is doing what you want\n') 
    cat('NOCA_TSM_Source.R: WARNING: before continuing. \n') 
    cat('NOCA_TSM_Source.R: WARNING:\n') 
    cat('NOCA_TSM_Source.R: WARNING: #########################################################\n') 
    working$Age[ working$Age %in% c('A', 'O', 'S', 'T', 'Y') ] = 'Adult'
    working$Age[ working$Age %in% c('L', 'J', 'H') ]           = 'Juvenile'
    working$Age[ working$Age %in% c('U', '',  'F', 'K', 'X') ] = 'Unknown'

    # Check that we got that right
    ageList = unique(working$Age)
    if ( length(ageList) != 3 ) {
        stop('NOCA_TSM_Source.R: Error: Hmmm, the first reaging step did not work\n')
    }

    # I think the goal of Eric's original code is to remove birds that only have
    # unknown ages. I think my replacement code does the same thing with fewer steps.
    # My code is if == T and Eric's code is if == F
    if ( T ) {
        # Step 1: Create an auxiliar data.frame that contains the list of unique ages given to each bird
        ageDF = aggregate(Age~BandNumber, working, function(age) { age %>% unlist() %>% unique() %>% sort() %>% paste(collapse=',')} )

        # Step 2: Remove any birds that where only ever given an age class of Unknown.
        index = ageDF$Age == 'Unknown'
        cat('NOCA_TSM_Source.R: Msg: Removing ',sum(index),' birds that where only ever classed as Age == Unknown (U, \"\", F, K, X)\n')
        working = subset(working, BandNumber %in% ageDF$BandNumber[index] )

        # Step 3: Make sure ages occure in the correct order. 
        
        # Just work on birds that have more than one age class
        ageDF$numAge = apply(ageDF, 1, function(row) { row[2] %>% strsplit(split=',') %>% unlist() %>% length() })
        ageDF = subset(ageDF, numAge > 1 ) 

        # Cycle over birds that have more than one age class 
        # and make sure those ages occurred in the correct order. 
        for ( bn in ageDF$BandNumber ) {
            bird = subset(working, BandNumber == bn )
            bird = bird[ order(bird$Date), ]
        }

    } else {
        # Get the NOCA records with an age of U
        uagebirds        <- subset( NOCA, Age == 'U')

        # Get the band numbers for birds with an age of U
        uagebirds        <- unique( uagebirds$BandNumber )

        # Get all the records for birds with an age of U
        # Note that a bird might have several records and so several age values
        # This gets all the records of the birds that have at least one age == U record
        uagebirds        <- subset( NOCA, BandNumber %in% uagebirds)

        # Create an auxiliary data.frame that has the range of dates
        # for each band/age combination
        uagebirds        <- aggregate( Date~BandNumber+Age, uagebirds, paste, collapse=',' )

        # Not sure why this function is defined here. 
        '%!in%'          <- function(x,y) {!('%in%'(x,y))}

        # Find the band numbers for birds that have at least one non-U age
        knownunknownages <- unique( uagebirds$BandNumber[ which(uagebirds$Age != 'U') ] )

        # Find the band numbers for birds that have at least one U age
        unknownages      <- unique( uagebirds$BandNumber[ which(uagebirds$Age == 'U') ] )

        # Get the band numbers for birds with at least one unknown age that have no known ages.
        # knownunknownages is a list of bands that have at least one known age
        # unknownages is a lsit of bands that have at least one unknown age
        # the %in% command creates a true/false list the same length as unknownages
        # that is true when a band number from unknownages is in the knownunknownages
        # The final == "FALSE" inverse the true/false list. Could also be done with !. 
        unknownages      <- unknownages[ which( unknownages     %in% knownunknownages == "FALSE" ) ]

        # Finally, subset the NOCA data.frame so it only contains birds that only have
        # unknown ages class designation. 
        unknownages      <- NOCA[        which( NOCA$BandNumber %in% unknownages ), ]

        # Create an auxiliar data.frame that gives the list of capture dates for each band number
        # associated with birds that only have unknown age class designation. 
        checkAge         <- aggregate( BandNumber~Date, unknownages, paste, collapse=',' )

        # Get the unique band numbers for birds with only unknown age designation
        unknownages      <- unique( unknownages$BandNumber)

        # Find out how many records there are in the birbs data base. 
        beforeAgeRemoval <- nrow( birbs)

        # Subset birbs 
        # I don't really get how this is supposed to work. There are really too many 
        # steps here to make this understandable.
        birbs            <- birbs[ which( birbs$BandNumber %!in% unknownages ), ]

        # Find out how many records there are in the birbs data base for birds (NOCAs?) 
        # that only have U age. 
        afterAgeRemoval  <- nrow( birbs )

        # Check what has happened as a result of removing birds with no known age. 
        if( ( beforeAgeRemoval-afterAgeRemoval )!=length( unknownages ) ){
          cat( 'MSM_Tobin_Original.R: Msg: Error in unknown age removals! More birds tossed or captured more than once.\n' )
          browser( )
        }else{
          cat( 'MSM_Tobin_Original.R: Msg: Removed', length(unknownages), 'birds with unkown ages, captured only once.\n' )
        }
    }

################################################################################
# Step 2b: Initial Age Assignment
#
################################################################################
# Clean up the name space
remove(list   = c('checkAge','uagebirds','unknownSexBirds','NOCA','problemBirds'))

# Create an auxiliar data frame that contains a list of all ages 
# assocaited with each band number.
intAge        <- aggregate(Age~BandNumber, birbs, paste, collapse=',')

# Get a list of everyone's ages
#
# Checked by hand, but should build a script to determine that none of the birds get
# a younger age class after getting and older age class. That is, make sure age is only
# increasing. 
# We also need to put some for loops in here to convert the initial ages into Y/A
# for now, I'm going to give everyone their initial age classes in birbs and go from there
birbs$IntAge <- 0

# Give each record the full list of ages assigned to the associated bird
for (band in intAge$BandNumber){
    birbs$IntAge[birbs$BandNumber == band] = intAge$Ages[intAge$BandNumber == band]
}

# Now reset IntAge for each record so that it contains the initial age of the bird.
for (i in 1:nrow(birbs)){
  indivAge <- birbs$IntAge[i]
  agesplit <- strsplit(indivAge,",",fixed = TRUE)[[1]]
  birbs$IntAge[i] <- agesplit[1]
}

# Assign everyone their full list of ages and set them to the first index 
# SMD: It is note clear that this code really does what you want.
# SMD: Birds can have one of 13 different age codes: "A" "O" "S" "U" "T" "Y" "L" ""  "H" "J" "F" "K" "X"
# SMD: Here you only check for H, J, L, S, A, and U. So the classes O, T, Y, "", F, K and X are all ommited. 
# SMD: This may not be a problem for the NOCA's but that isn't they way you built this script. 
# SMD:
agelessbirds <- as.list(NULL)
for (b in 1:nrow(birbs)){
    if (birbs$IntAge[b] == "H" || birbs$IntAge[b]=="J" || birbs$IntAge[b]=="L" ){
        # If the initial age for a bird's record is H, J or L, then reset it to L
        # H = hatch year
        # J = Juvenile
        # L = ???
        birbs$IntAge[b] = "Y"
    } else if (birbs$IntAge[b] == "S"|| birbs$IntAge[b] == "A"){
        # If the initial age for a bird's record is S (second year) or A (after hatch year)
        # then set it to "A" (adult).
        birbs$IntAge[b] = "A"
    } else if (birbs$IntAge[b] == "U"){
        # Keep a list of birds that had an age of U as the initial age. 
        agelessbirds <- list.append(agelessbirds, c(birbs$BandNumber[b], birbs$SpeciesCode[b]) )
    }
}
cat("Removing NOCAs with bad first ages. Come back to this after conference.")
agey<-transpose( as.data.frame(agelessbirds))
agey <- agey[which(agey$V2=="NOCA"),]

AgelessNOCA = subset(birbs, BandNumber%in%agey$V1)
AgelessNOCA <- unique(AgelessNOCA$BandNumber)
birbs       = subset(birbs, BandNumber%!in%AgelessNOCA)
ageless = length(AgelessNOCA)
#okay, we have 93 observations of NOCAs with a bad first age class, but there are only 20 individuals....
#should fix this later, but we'll just toss em for now. This comes out as a result of converting age classes. I should take thes and backtrack cycle codes when i have more time. I also got birds tossed correctly, in that all obs are removed.

#Redefine initial age. We're going to need to refine this loop with the date thingy (before spring are still considered young)
cat("There are",ageless,"NOCAs with a bad initial age class")
################################################################################
# Step 3: Create your date index for your site and Blank CH for each bird
#
################################################################################
#Do this before subsetting by species. There are days you sample but don't capture your species

####Create your date list, index, and CH
datelist       <- as.data.frame( unique(  birbs$Date  ) )
#get the unique dates. We've combined two dates thusfar
datelist       <- datelist[order(datelist$`unique(birbs$Date)`) , ] 
#port the dates into an ordered list. You can't do in a df, so overwrite it
datelist       <- as.data.frame(datelist)
#get the list of dates into a DF so you can play around with them
################################
###We have a problem here, it can't sort based on a df column, only list
#Solved that, read comments above. 
################################
datelist$Index <- seq.int( nrow(  datelist  ) )
#create your index. This puts a counter by each date, telling you the order in sequence. 

datelist$CH    <- 0
#create blank capture history column. 

setnames(datelist, "datelist", "Date")
#rename to make it easier

birbs$CH       <- paste0(datelist$CH, collapse="")
#apply blank capture history to every bird
##I think this is no longer used, we have a different way of doing things now to create the CH. 
##Kept until confirmed unused. 23Jan2019
birbs$season = "Nonbreeding"
for (i in 1:nrow(birbs)){
  if (birbs$Month[i]==4|birbs$Month[i]==5|birbs$Month[i]==6|birbs$Month[i]==7){
    birbs$season[i] = "Breeding"
} else if(birbs$Month[i] == 8 && birbs$Day[i] <= 7){
  birbs$season[i] = "Breeding"
} else{}
}

#added the effect of season, we can use as a grouping variable
################################################################################
# Step 4: Subset to species 
################################################################################

###
NOCA      <- birbs[which(birbs$SpeciesCode == "NOCA"),]
#Subset to NOCA. Keep original birbs
###
nobandNOCA <- length(unique(NOCA$BandNumber))
NOCA       <- subset(NOCA, !is.na(NOCA$BandNumber))
nobandNOCA <- nobandNOCA - length(unique(NOCA$BandNumber))
#Remove missing band numbers. Lose 14.
#Problem: these missing band number birds are missing. Somewhere, we purged non-banded birds. Talk to Scott
cat('MSM_Tobin_Original.R: Msg: We do not have any birds missing bands. Removed', nobandNOCA,'NOCAs with no band number')
premoval  <- length( unique( NOCA$BandNumber ) ) 

NOCA      <- NOCA[which(NOCA$Sex != "U"),]
postmoval <- length( unique( NOCA$BandNumber ) )
cat('MSM_Tobin_Original.R: Msg: Removed U sex birds. This removes', (premoval-postmoval) ,'NOCA individuals from the population.\n')
#browser()
NOCA$Fat[NOCA$Fat== ""] = "U"
nofatNOCAs <- NOCA[which(NOCA$Fat == 'U'),]
nofatobs   <- length(nofatNOCAs$BandNumber)
nofatnum <- unique(nofatNOCAs$BandNumber)
if(nofatobs != length(nofatnum)){
  cat('MSM_Tobin_Original.R: Msg: There are', nofatobs ,'NOCA observations from the population, but there are multiple captures of at least one. Investigate.\n')
  browser()
} else{
  cat('MSM_Tobin_Original.R: Msg: There are', length(nofatnum) ,'NOCA individuals caught only once with no fat observation. Removed from analysis.\n')
}
NOCA <- NOCA[which(NOCA$Fat != 'U'),]
#we need to reduce our dimensionality, so we are making fat into two categories: High (B/F/H) and Low (L/T/N)
NOCA$Fat[NOCA$Fat=="B"] = "H"
NOCA$Fat[NOCA$Fat=="F"] = "H"
NOCA$Fat[NOCA$Fat=="T"] = "L"
NOCA$Fat[NOCA$Fat=="N"] = "L"

################################################################################
# Step 5: Create Summary Variables
################################################################################
cat('MSM_Tobin_Original.R: Msg: Creating summary variables')
NOCABands <- unique(NOCA$BandNumber)

MeanTable <- aggregate(RightWing~BandNumber, NOCA, mean, na.rm=T)

NOCA$CJS  <- 1

NOCA$MeanWing <- 0

for ( band in MeanTable$BandNumber ) {
  NOCA$MeanWing[ NOCA$BandNumber == band ] = MeanTable$RightWing[ MeanTable$BandNumber == band]
}

beforeWingRemoval <- length(unique(NOCA$BandNumber))
NOCA <- subset(NOCA, !is.na(NOCA$MeanWing))
afterWingRemoval <- beforeWingRemoval - length(unique(NOCA$BandNumber))
nowingNOCA <- NOCA[which(NOCA$MeanWing == 0),]
NOCA <- NOCA[which(NOCA$MeanWing != 0),]
RemovedNoWing <-length(unique(NOCA$BandNumber)) - (length(unique(NOCA$BandNumber)) - length(unique(nowingNOCA$BandNumber))) + afterWingRemoval


if (length(unique(nowingNOCA$BandNumber)) != RemovedNoWing){
cat('MSM_Tobin_Original.R: Msg: Removed birds without winglength. This removes', RemovedNoWing ,'NOCA observations from the population, but there are',length(unique(nowingNOCA$BandNumber)) ,'NOCA individuals. This means some are captured more than once without a winglength. Investigate.\n')
browser()
}else {
  cat('MSM_Tobin_Original.R: Msg: Removed birds without winglength captured only once. This removes',length(unique(nowingNOCA$BandNumber)) ,'NOCA individuals.\n')
}


################################################################################
################################################################################
# Make columns that should be treated as factors into actual factors.
# They have been stored as strings up until this point.
# 
# 
# 
################################################################################
################################################################################
birbs$Malaria = as.factor(birbs$Malaria)
birbs$Station = as.factor(birbs$Station)
birbs$Sex     = as.factor(birbs$Sex)
birbs$Net     = as.factor(birbs$Net)



################################################################################
# Step 6: Create MARK sheet with MSM History
################################################################################
Marky <- as.data.frame(unique(NOCA$BandNumber))
setnames(Marky, "unique(NOCA$BandNumber)", "BandNumber")
#Using BandNumber for merge function below. This prevents mishaps from ordering
##order doesn't matter, it'll match by function. 

Marky$CH <- 0
#Create column to hold capture histories

###################
#Here you make your MSM variable in question
###################
#####
#FAT
#we have turned missing into U above
beforeFatRem  <- length(unique(NOCA$BandNumber))
NOCA <- NOCA[which(NOCA$Fat != 'U'),]
afterFatRem  <- length(unique(NOCA$BandNumber))
diffy = beforeFatRem - afterFatRem

cat('MSM_Tobin_Original.R: Msg: Removed birds without fat captured only once. This removes',diffy ,'NOCA individuals.\n')

#NOCAfat = aggregate(NOCA$Fat~NOCA$BandNumber + NOCA$Date, NOCA, paste, collapse=',')
#create DF with unique fat for each bird on each day of its individual capture
#Is and should be same length as NOCA
NOCAfat = aggregate(NOCA$CJS~NOCA$BandNumber + NOCA$Date, NOCA, paste, collapse=',')



setnames(NOCAfat, 'NOCA$BandNumber', 'BandNumber')
setnames(NOCAfat, 'NOCA$Date',       'Date')
setnames(NOCAfat, 'NOCA$CJS',        'Fat')
#Change the names to make life easier. 
###################

NOCAcaps <- aggregate(NOCA$Date~NOCA$BandNumber, NOCA, paste, collapse=",")
names(NOCAcaps) = c('BandNumber', 'DateList')
#This will be how we create the different MSMs. Let's try with CJS, make sure it works

Marky = merge( Marky, NOCAcaps, by=c('BandNumber') )
#Go through by hand and make sure they are correct
###get a random sample, check them puppies
#creates a df with BandNumber, blank CH, and the DateList. This marries dates of capture with band# and CH
#You get the datelist from NOCAcaps applied to Marky. Guess we could've just done one dataframe here? Don't see why we need two...

#Below we create the capture histories from the date lists
#using full date index, you make string at that length of zeros subbing in your 
##variables you toook out in the above subsection
if (F){
for ( r in 1:nrow(Marky) ) {
  
  thisBird = subset(NOCAfat, BandNumber == Marky$BandNumber[r])
  #creates dataframe with BN, each dates, and fat reading
  ##new one made for each bird
  temp = merge(datelist, thisBird, by.x='Date', by.y='Date', all.x=TRUE)
  #create holding frame that is datelist, but added band number and fat
  temp$Fat[is.na(temp$Fat)] = '0'
  #this will sub in zeros for fat on days you didn't capture. 
  ##why it's important to convert na to something before, otherwise a false negative capture
  Marky$CH[r] = paste( temp$Fat, collapse='')
  #this collapses the fat column into a ch. Puts back into marky by index position
}
}
if (T){ #this is for using CJS models and Pradels
  for ( r in 1:nrow(Marky) ) {
    
    thisBird = subset(NOCAfat, BandNumber == Marky$BandNumber[r])
    #creates dataframe with BN, each dates, and fat reading
    ##new one made for each bird
    temp = merge(datelist, thisBird, by.x='Date', by.y='Date', all.x=TRUE)
    #create holding frame that is datelist, but added band number and fat
    temp$Fat[is.na(temp$Fat)] = '0'
    #this will sub in zeros for fat on days you didn't capture. 
    ##why it's important to convert na to something before, otherwise a false negative capture
    Marky$CH[r] = paste( temp$Fat, collapse='')
    #this collapses the fat column into a ch. Puts back into marky by index position
  }
}

################################################################################
# Step 7: Add summary variables to the MARK data
################################################################################
NOCA_Unique <- NOCA
NOCA_Unique <- NOCA_Unique %>% distinct(NOCA$BandNumber, .keep_all = TRUE)
#this plucks off the first reading for each bird. Since we applied sex and winglength for everyone, this should be fine to pull the variables from
###you'll have to modify the pims from the non summary data, but how useful is that? Your msm captures daily, unique variation

NOCA_Unique <- NOCA_Unique[c("BandNumber", "Sex", "MeanWing", "Malaria", "CJS", "IntAge","season")]
#Pare down the NOCA frame from merging

Marky = merge( Marky, NOCA_Unique, by=c('BandNumber') )
#Merge on bandnumber

Marky$DateList <- NULL
#remove the datelist, not needed anymore
#combine into mark dataframe, remove everyone who didn't have a mean wing
##loses 40 individuals
#browser()
setnames(Marky, "BandNumber", "id")
setnames(Marky, "CJS", "freq")
setnames(Marky, "CH", 'ch')
#give them mark names

################################################################################
# Step 8: Make capture intervals (not equal, so needs to be set)
################################################################################
##create a new data frame called ranker to rank the date.
cat('Making Cap.Int data\n')
ranker=data.frame( date = sort( unique(datelist$Date)) )
###This makes a one column dataframe with each date as a seperate entry
#sorted so it matches up with interval

caps     = nrow(ranker) #Number of captures
interval = rep(NA,caps) #Empty list with N/A
# first create a new string named interval by subtracting the subsequent capture dates
###First step is to put #caps into a var

for(i in 2:nrow(ranker)){
  interval[i] = (ranker$date[i]-ranker$date[i-1])
}
###This fills in each space in interval with the number of days between capture occassions

###Test message to track where we are
cat('Create Cap.Int Data\n')

# now create a data frame
###This gives the interval since last and applies to date. First is NA since new
cap.int=data.frame(ranker$date,interval)
cap.int$occIndex = 1:nrow(cap.int)

## create a new column in ranker data frame to rank the dates
ranker$rank<-1:nrow(ranker)
#I'm not sure this is still neccessary since we sort things already. This may be an artifact of Binab.

## add new column for interval in month
###This gives you the amount of time in between captures in terms of months
cat('Create interval data\n')
cap.int$monthlyinterval=round(cap.int$interval/30,digits=3)

################################################################################
# Step 9: Massage for MARK (Remove missing values)
################################################################################
cat('Process data\n')
Marky$IntAge <- as.factor(Marky$IntAge)
Marky$Malaria <- as.factor(Marky$Malaria)
Marky$season <- as.factor(Marky$season)

#CH must be a character
Marky$ch <- as.character(Marky$ch)

NOCA.proc <- process.data(Marky, model = "Pradrec", groups = c("Sex", "season","IntAge"), time.intervals = cap.int$monthlyinterval[2:caps])

numNocaEnd <- length(unique(Marky$id))

cat('MSM_Tobin_Original.R: Msg: Through the data sanitation process, we have lost', numNocaBegin-numNocaEnd,'for missing data. This is a loss of',  (((numNocaBegin- numNocaEnd)/numNocaBegin)*100),'% of the NOCAs.\n')

cat('Make design data\n')
timestart = format(Sys.time(), "%d_%b_%Y_%H_%M")
timestart = as.character(timestart)
NameForFile =paste0('NOCA.ddl_',timestart)
NameForFile2 =paste0('NOCA.proc_',timestart)


NOCA.ddl <- make.design.data(NOCA.proc)

save(NOCA.ddl, file= NameForFile)
save(NOCA.proc, file = NameForFile2)

#NOCA.ddl = add.design.data(NOCA.proc, NOCA.ddl, parameter = "S", type = "time", bins=c(0,11.95,23.4,35.4,47.4,59.4,71.4,83.4,95.4,107.4), name = "year")
#NOCA.ddl = add.design.data(NOCA.proc, NOCA.ddl, parameter = "S", type = "time", bins=c(0, 11.95 + 0:8*12 ), name = "year", replace=TRUE)
#This isn't the way to do it, at least not with uneven intervals (months!=30 days always)
#you can do prime factors 5*73, but we're gonna manually change the years

######
#08March2019
#start here by adding the age.now variable. You can set the if statement up with ors for bins of years. 
#if int.age = y and time =(0-12) then convert all the other int.age for that cohort into adult
##but how do you separate for each cohort? gotta do add.design.data
###Actually, look at the .ddl
#par.index model.index      group cohort   age  time occ occ.cohort stratum Cohort   Age  Time Sex   season IntAge H L   year
#1         1           1 FBreedingA      1     0     1   1          1       H      0 0.000 0.000   F Breeding      A 1 0 [0,12]
#2         2           2 FBreedingA      1 0.233 1.233   2          1       H      0 0.233 0.233   F Breeding      A 1 0 [0,12]
#3         3           3 FBreedingA      1   0.7   1.7   3          1       H      0 0.700 0.700   F Breeding      A 1 0 [0,12]
#4         4           4 FBreedingA      1   1.2   2.2   4          1       H      0 1.200 1.200   F Breeding      A 1 0 [0,12]
#5         5           5 FBreedingA      1 1.733 2.733   5          1       H      0 1.733 1.733   F Breeding      A 1 0 [0,12]
#6         6           6 FBreedingA      1 1.966 2.966   6          1       H      0 1.966 1.966   F Breeding      A 1 0 [0,12]
#I really think you can just change this in Marky and use it as a grouping variable... if you only have the two age classes

#I wanna check to make sure that season is correct. I think when I added it, the NOCA.proc got it as the first instance for that bird, which is incorrect. This next part is modified and lifted from Binab code

## first create a dataframe with details of seasons
############
cap.int$seasons=character(nrow(cap.int))
cap.int$seasons2=character(nrow(cap.int))
cat('Create Season and year Data\n')
seasons= data.frame(
  season=c("Breeding","NonBreeding","NonBreeding"),
  offset  =c(1, 1, 0 ),
  start=c ("04-01", "08-08", "01-01"),
  end  =c("08-07","12-31", "03-31"), stringsAsFactors=F
)
cap.int$yearReal <- NA
for (i in 1:nrow(cap.int)) {
  cap.int$yearReal[i] <- as.numeric( strsplit(as.character(cap.int$ranker.date[i]),"-",fixed = TRUE)[[1]] )
#create a year factor for cap.int, the reference date dataframe
#you're gonna get warnings, they are fine to ignore
}
cap.int$year = cap.int$yearReal - min(cap.int$yearReal) + 1

#cap.int$year <- as.Date(cap.int$year, "%Y")
#probably need to coerce into a date, but meh, it works?
yearlist=unique(cap.int$yearReal)
# creating the list of unique years

##yearlist= unique(format(cap.int$ranker.date,"%y")) does the same thing as above code
#now binning the dates into seasons
cap.int$smdYear = 0
cnt = 0
for( y in yearlist){
  for ( i in 1:nrow(seasons)){
    startdate = as.Date(paste(y,'-',seasons$start[i],sep=''))
    enddate   = as.Date(paste(y,'-',seasons$end  [i],sep=''))
    index=cap.int$ranker.date>=startdate & cap.int$ranker.date<=enddate
    cap.int$seasons[index]=seasons$season[i]
    cap.int$smdYear[index] = as.integer(y) + seasons$offset[i] - 2010
   
    
  }
}
cap.int$smdYear = as.factor( cap.int$smdYear )

# to find out which capture occassion are in which season. 

occasion=rep(NA,nrow(cap.int))
occasion[1]=1
for(i in 1:(nrow(cap.int)-1)){occasion[i+1]=(occasion[i]+cap.int$monthlyinterval[i+1])}



cap.int$occasion=occasion
cap.int$year=format(cap.int$ranker.date,"20%y")

cat('Adding Season to DDL\n')
Breeding_occasion= cap.int$occasion[cap.int$seasons=="Breeding"]
NonBreeding_occasion= cap.int$occasion[cap.int$seasons=="NonBreeding"]
#assign season to the different ranges of dates

NOCA.ddl$Phi$season='Breeding'
NOCA.ddl$Phi$season[ NOCA.ddl$Phi$time%in%(NonBreeding_occasion) ]='NonBreeding'
#add season effect to S
NOCA.ddl$p$season='Breeding'
NOCA.ddl$p$season[ NOCA.ddl$p$time%in%(NonBreeding_occasion) ]='NonBreeding'
#add season effect to p
NOCA.ddl$f$season='Breeding'
NOCA.ddl$f$season[ NOCA.ddl$f$time%in%(NonBreeding_occasion) ]='NonBreeding'
#add season effect to f
NOCA.ddl$f$season=factor(NOCA.ddl$f$season)
NOCA.ddl$p$season=factor(NOCA.ddl$p$season)
NOCA.ddl$Phi$season=factor(NOCA.ddl$Phi$season)
#with(NOCA.ddl$S, table(season, time))  


cat('Adding Year to DDL\n')

year1= cap.int$occasion[cap.int$smdYear==1]
year2= cap.int$occasion[cap.int$smdYear==2]
year3= cap.int$occasion[cap.int$smdYear==3]
year4= cap.int$occasion[cap.int$smdYear==4]
year5= cap.int$occasion[cap.int$smdYear==5]
year6= cap.int$occasion[cap.int$smdYear==6]
year7= cap.int$occasion[cap.int$smdYear==7]
year8= cap.int$occasion[cap.int$smdYear==8]
year9= cap.int$occasion[cap.int$smdYear==9]

NOCA.ddl$Phi$year='1'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year2) ]='2'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year3) ]='3'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year4) ]='4'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year5) ]='5'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year6) ]='6'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year7) ]='7'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year8) ]='8'
NOCA.ddl$Phi$year[ NOCA.ddl$Phi$time%in%(year9) ]='9'
#add year effect to S
NOCA.ddl$p$year="1"
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year2) ]='2'
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year3) ]='3'
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year4) ]='4'
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year5) ]='5'
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year6) ]='6'
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year7) ]='7'
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year8) ]='8'
NOCA.ddl$p$year[ NOCA.ddl$p$time%in%(year9) ]='9'
#add year effect to p

NOCA.ddl$f$year="1"
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year2) ]='2'
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year3) ]='3'
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year4) ]='4'
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year5) ]='5'
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year6) ]='6'
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year7) ]='7'
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year8) ]='8'
NOCA.ddl$f$year[ NOCA.ddl$f$time%in%(year9) ]='9'
#add year effect to f

NOCA.ddl$f$year=factor(NOCA.ddl$f$year)
NOCA.ddl$p$year=factor(NOCA.ddl$p$year)
NOCA.ddl$Phi$year=factor(NOCA.ddl$Phi$year)

cat('We cannot add ageNow to DDL. This section is toggled for more exploration with Scott (07April2019)\n')
browser()
if(F){
NOCA.ddl$f$ageNow = NOCA.ddl$f$IntAge
NOCA.ddl$Phi$ageNow = NOCA.ddl$Phi$IntAge
NOCA.ddl$p$ageNow = NOCA.ddl$p$IntAge
#attach(NOCA.ddl$Phi)
for (i in 1:nrow(NOCA.ddl$Phi)){
  if (NOCA.ddl$Phi$ageNow[i] == "Y"){
    #horty <- occ.cohort[i]
    
    occFirst = NOCA.ddl$Phi$occ.cohort[i]
    occThis  = NOCA.ddl$Phi$occ[i]
    
    yearFirst = as.numeric( cap.int$smdYear[ cap.int$occIndex == occFirst ] )
    yearThis  = as.numeric( cap.int$smdYear[ cap.int$occIndex == occThis  ] )
    if ( yearFirst == yearThis ) {
        NULL; #Do nothing, this case is OK.   
    } else if ( yearFirst < yearThis ) {
        NOCA.ddl$Phi$ageNow[i] = 'A'
    } else {
        cat('WARNING: Something bad has happend. \n')
    }
    
      
  }
  else if (NOCA.ddl$Phi$ageNow[i] == "A"){
  }
}
#detach(NOCA.ddl$Phi)
#attach(NOCA.ddl$p)
for (i in 1:nrow(NOCA.ddl$p)){
  if (NOCA.ddl$p$ageNow[i] == "Y"){
    #horty <- occ.cohort[i]
    
    occFirst = NOCA.ddl$p$occ.cohort[i]
    occThis  = NOCA.ddl$p$occ[i]
    
    yearFirst = as.numeric( cap.int$smdYear[ cap.int$occIndex == occFirst ] )
    yearThis  = as.numeric( cap.int$smdYear[ cap.int$occIndex == occThis  ] )
    if ( yearFirst == yearThis ) {
      NULL; #Do nothing, this case is OK.   
    } else if ( yearFirst < yearThis ) {
      NOCA.ddl$p$ageNow[i] = 'A'
    } else {
      cat('WARNING: Something bad has happend. \n')
    }
    
    
  }
  else if (NOCA.ddl$p$ageNow[i] == "A"){
  }
}

for (i in 1:nrow(NOCA.ddl$f)){
  if (NOCA.ddl$f$ageNow[i] == "Y"){
    #horty <- occ.cohort[i]
    
    occFirst = NOCA.ddl$f$occ.cohort[i]
    occThis  = NOCA.ddl$f$occ[i]
    
    yearFirst = as.numeric( cap.int$smdYear[ cap.int$occIndex == occFirst ] )
    yearThis  = as.numeric( cap.int$smdYear[ cap.int$occIndex == occThis  ] )
    if ( yearFirst == yearThis ) {
      NULL; #Do nothing, this case is OK.   
    } else if ( yearFirst < yearThis ) {
      NOCA.ddl$f$ageNow[i] = 'A'
    } else {
      cat('WARNING: Something bad has happend. \n')
    }
    
    
  }
  else if (NOCA.ddl$f$ageNow[i] == "A"){
  }
}

NOCA.ddl$f$ageNow = as.factor(NOCA.ddl$f$ageNow)
NOCA.ddl$Phi$ageNow = as.factor(NOCA.ddl$Phi$ageNow)
NOCA.ddl$p$ageNow = as.factor(NOCA.ddl$p$ageNow)
}
#detach(NOCA.ddl$p)
#load('NOCA.ddl_21_Feb_2019_19_40')
#NOCA.ddl$Phi$age.now <- 0
#sendmail("<erictobinull@gmail.com>","<erictobinull@gmail.com>","DDL Generation Complete","DDL has been created with initial age class, sex, and malaria as grouping variables.",control=list(smtpServer="ASPMX.L.GOOGLE.COM")) 

#browser()
cat('Saving Modified DDL\n')
#sendmail("<erictobinull@gmail.com>","<erictobinull@gmail.com>","MSM Model Begins Run","MSM Model has begun running. Check notes for 26Feb2019 on model specs. This is NOCA_MSM_Source_Trial2_LoadDDL.R",control=list(smtpServer="ASPMX.L.GOOGLE.COM")) 
timestart = format(Sys.time(), "%d_%b_%Y_%H_%M")
timestart = as.character(timestart)
NameForFile =paste0('NOCA.ddl_modified_',timestart)
NameForFile2 =paste0('NOCA.proc_modified_',timestart)

save(NOCA.ddl, file= NameForFile)
save(NOCA.proc, file = NameForFile2)

initial.analysis=function(){
  
  ###################  
  #List of p models
  p.dot                       =list(formula=~1)     
  p.Sex                       =list(formula=~Sex)   
  #p.season                    =list(formula=~season)
  #p.ageNow                    =list(formula=~ageNow)
  #p.year                      =list(formula=~year)  
  #p.Malaria                   =list(formula=~Malaria)
  #p.Sex.season                =list(formula=~Sex+season)
  #p.ageNow.year               =list(formula=~ageNow+year)
  
  #p interactions
  #p.Sex_season 				  =list(formula=~Sex*season)
 #p.ageNow_year 			  =list(formula=~ageNow*year)
  
  ###################   
  #List of S models
  Phi.dot                       =list(formula=~1)
  Phi.Sex                       =list(formula=~Sex)
  #Phi.season                    =list(formula=~season)
  #Phi.ageNow                    =list(formula=~ageNow)
  #Phi.year                      =list(formula=~year)
  Phi.MeanWing                  =list(formula=~MeanWing)
  #Phi.stratum                   =list(formula=~-1+stratum)
  #Phi.Malaria                   =list(formula=~Malaria)
  #Phi.Sex.Season                =list(formula=~Sex+season)
  #Phi.Season.year               =list(formula=~season+year)
  #Phi.Malaria.Sex               =list(formula=~Malaria+Sex)
  #Phi.Malaria.MeanWing          =list(formula=~Malaria+MeanWing)
  #Phi.Malaria.season            =list(formula=~Malaria+season)
  ###################   
  #S interactions  
  #Phi.Sex_Season                =list(formula=~Sex*season)
  #Phi.Season_year               =list(formula=~season*year)
  #S.Malaria_Sex               =list(formula=~Malaria*Sex)
  #S.Malaria_MeanWing          =list(formula=~Malaria*MeanWing)
  #S.Malaria_season            =list(formula=~Malaria*season)
  ###################   
  #List of Psi models  
  #Psi.s                       =list(formula=~-1+stratum:tostratum, link="logit")
  f.dot                        =list(formula=~1)
  f.Sex                        =list(formula=~Sex)
  #use create.model.list to construct the  models and store in object named woodrat.cml
  NOCA.cml=create.model.list("Pradrec")
  #use mark.wrapper with model list NOCA.CML and the processed data and design data to fit each of the models in MARK
  model.list=mark.wrapper(NOCA.cml,data=NOCA.proc,ddl=NOCA.ddl,output=F, external = T, threads = 2)        
  #results=mark.wrapper(NOCA.cml,data=NOCA.proc,ddl=NOCA.ddl,adjust=T,invisible=F)
  #return the list of model results as the value of the fnction
  return(model.list)
  return(NOCA.cml)
}
cat('Running Analysis\n')
#browser()
#NOCA.results=initial.analysis()
#I don't know that this works for this formulation. I'm having a problem accessing individual models
#I may have to do this a different way. See below
#I think we have to generate a list of the model names, every iteration of Phi.var_p.var_f.var, then give to collect.models.
initial.analysis()
NOCA.results = collect.models()


timestart = format(Sys.time(), "%d_%b_%Y_%H_%M")
timestart = as.character(timestart)
NameForFile3 =paste0('NOCA.results_PradelTrial_',timestart)
cat('Results saved in', NameForFile3, '\n')
save(NOCA.results,file=NameForFile3)

#sendmail("<erictobinull@gmail.com>","<erictobinull@gmail.com>","MSM Complete","MSM Model run with 2 CPUs, psi.logit link, and .ddl loading is complete.",control=list(smtpServer="ASPMX.L.GOOGLE.COM"))
