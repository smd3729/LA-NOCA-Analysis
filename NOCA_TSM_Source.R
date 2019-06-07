################################################################################
################################################################################
## NOCA_TSM_Source.R: 
##
##
## This program runs a TSM model on the data. 
## To do this, we need to take a couple of steps.
##   0) Get the R environment ready to go
##
##   1) Load the data
##
##   2) Clean up the banding data at a global level.
##      There are a few things that need to be addressed 
##      at the level of the entire database. So we do those first.
##      These tasks include:
##            1) Renaming a few columns to make the code simpler
##            2) Making sure the date column has the correct type.
##            3) Fix some broken dates.
##            4) Create a list of capture dates and intervals. 
##
##   3) Trim the data down to just the records we want. 
##      This involves limiting the data by species, station and record. 
##      
##   4) Next we need to clean up the data at a species level. 
##      This involves fixing some details regarding age, sex, fat, etc ...
##      This step also includes rebinning some of the data into
##      new classes that are more meaningful for this analysis. 
##      These steps include:
##            1) Setting ages to either "Adult", "Juvenile" or "Unknown"
##            2) Removing birds with only unknown ages. 
##            3) Fixing sex and removing birds with only unknown ages. 
##
##   5) Next, we need to reformat the data into a format that 
##      can be used in the mark recapture analysis. This means
##      creating a capture history (100101011010...) and defining
##      the covariates for each bird (age, sex, size, etc ...)
##
##
################################################################################
################################################################################

################################################################################
################################################################################
## Step 0: Get the environment set up and ready to run
## check
################################################################################
################################################################################

    # Step 0.1: Clean up the name space so we are starting from scratch.
        rm(list=ls())
        assign("last.warning", NULL, envir = baseenv())

    # Step 0.2: Load the nessisary R packages. 
    # I'm ont sure all of these are currently
    # being used in this version, but they have 
    # been used in the past. 
        library(data.table)
        library(dplyr)
        library(ggplot2)   
        #library(lsmeans)
        library(magrittr)
        library(msm)
        #library(multcomp)
        library(plyr)
        library(rlist)
        library(RMark)
        #library(sendmailR)
        library(stringi)
        library(stringr)

    # Step 0.3: Define user configurable parameters
        filenameBirbs = 'labo database malaria.csv'
    
        filenameTemplate  = 'NOCA_TSM_Source_<field>.<type>'
        filenameDLL       = filenameTemplate %>% str_replace('<field>','_DLL') %>% str_replace('<type>','dll')
        filenameProc      = filenameTemplate %>% str_replace('<field>','_Proc') %>% str_replace('<type>','proc')

################################################################################
################################################################################
## Step 1: Load in the data
## check
################################################################################
################################################################################

    # Print a message to the screen
    cat('NOCA_TSM_Source.R: Msg: Step 1: Reading CSV data\n')

    # Actually load the data. stringsAsFactors=F tells R to no convert strings to factors
    # which is a better approach in most cases. 
    birbs <- read.csv(filenameBirbs, stringsAsFactors=FALSE)

################################################################################
################################################################################
## Step 2: Clean up the data at a global level 
##
## There are several steps we need to do to clean up the data. 
##
## First, we need to fix some basic features of the data at a global level.
## We need to fix a few column names, make sure dates are treated as dates.
##
## Next we need to fix a few anomolies in the date date. In particular
## there are some back-to-back banding occasions that I think are probably
## recording errors. 
##
## Next we need to strip the data down to just those records that we really 
## care aboue for this analysis. In this case we just want NOCAs from
## Bluebonnet. 
##
## Next we need to fix some anomolies in the records. Note that a "record"
## is a record of capture. A single bird might have multiple records. 
##
## To fix the anomolies we need to:
##    1) Remove redunent records. Some captures are recorded twice in the database
##       and we need to remove these. 
##
##    2) Fix sex anomolies for birds. Some birds have multiple sex designations.
##
##    3) Fix age anomolies for birds. Make sure ages are chronological and fix a few other things. 
##
##    4) 
## check
################################################################################
################################################################################

    # Print out a status message so we know where we are inthe script. 
    cat('NOCA_TSM_Source.R: Msg: Step 2: Clean the data\n')
    
    ################################################################################
    # Step 2.0: Make a copy of the database for comparison.
    #
    #           Part of what we need to do here is remove damaged
    #           records. We'll make a copy of the database before anything
    #           is removed, and then we can compare the cleaned database
    #           and the original one to see what changed. 
    #
    #           There are other, more efficient ways to do this
    #           but this way works and makes other aspects of the coding easier. 
    # check
    ################################################################################
        birbsOriginal = birbs

    ################################################################################
    # Step 2.1: Rename columns
    # check
    ################################################################################
        cat('NOCA_TSM_Source.R: Msg: Step 2.1: Renaming columns\n')

        # Change the column name "BandingStation" to just "Station"
        setnames(birbs, "BandingStation", "Station")

        # Change the column name "BandingDate" to just "Date"
        setnames(birbs, "BandingDate", "Date")

    ################################################################################
    # Step 2.2: Find, report and remove the number of same day recaptures.  
    #           There are eight Codes used for birds: N, R, S, C, L, D, "", and U
    #           N = new
    #           R = recap
    #           S = same day recap
    #           C = band change
    #           L = lost
    #           D = destroyed
    #           "" = ?
    #           U = unknown
    #             
    #           For now I'm just going to get rid of the same day recap records. 
    # check
    ################################################################################
    cat('NOCA_TSM_Source.R: Msg: Step 2.2: Removing same day recaptures\n')
    birbs = subset( birbs, Code != "S" )

    cat('NOCA_TSM_Source.R: Msg: Step 2.2: Removing lost bands\n')
    birbs = subset( birbs, Code != "L" )


    ################################################################################
    # Step 2.3: Deal with the Date information in the database. 
    #           This includes casting the dates to the correct class
    #           and creating some auxiliary date information like season.
    #           We will also fix some broken date information.
    # check 
    ################################################################################
        cat('NOCA_TSM_Source.R: Msg: Step 2.3: Processing date information\n')

        # Step 2.3.1: Casting the date column to the correct type
        #
        cat('NOCA_TSM_Source.R: Msg: Step 2.3: Casting date column\n')
        birbs$Date    = as.Date(birbs$Date, '%Y-%m-%d')

        # Step 2.3.2: Remove records with NA dates.
        #             There are 15 records that have a date of NA. 
        #             These need to be removed. 
        #
        # check
        cat('NOCA_TSM_Source.R: Msg: Step 2.3: Removing records with NA dates\n')
        birbs = subset(birbs, !is.na(Date) )

        # Step 2.3.3: Fix some odd date information.
        #             We need to fix the dates before we narrow the data base because
        #             date is one criteria used to narrow the data base, so we need to 
        #             make sure they are correct before going any further. 
        #
        #             The database records several instances of "back-to-back" banding
        #             dates. Some of these are clearly clerical errors. 
        #             On June 5, 2019 I got the following results for Bluebonnet:
        #
        #              First      Second     numFirst numSecond numBoth
        #              <date>     <date>        <int>     <int>   <int>
        #            1 2011-04-16 2011-04-17       48        52       7
        #            2 2014-02-27 2014-02-28       39         2       1
        #            3 2014-05-25 2014-05-26       21         4       0
        #            4 2014-07-06 2014-07-07        2        40       0
        #            5 2015-01-10 2015-01-11        1        27       0
        #            6 2015-04-11 2015-04-12       75        38       0
        #            7 2015-06-07 2015-06-08        8         9       0
        #            8 2015-06-08 2015-06-09        9         3       0
        #            9 2015-06-21 2015-06-22       32        15       0
        #           10 2015-07-12 2015-07-13        4        15       1
        #           11 2016-06-09 2016-06-10        8         2       0
        #           12 2017-03-26 2017-03-27       25         4       0
        #           13 2017-03-27 2017-03-28        4         3       0
        #           14 2017-04-09 2017-04-10       16         2       0
        #           15 2017-04-23 2017-04-24       21         4       0
        #           16 2017-05-07 2017-05-08       11         6       0
        #           17 2017-06-04 2017-06-05        9         6       0
        #
        #             Note that same day capatures (Code == 'S') have already 
        #             been removed from the database.
        #             
        #             I'm going to change the date information for these
        #             back-to-back banding dates by re-dating records recorded
        #             on the second day to have the first day's date. 
        #             This will throw off the estimate of p and Phi a little
        #             but given the number of records involved, that we are
        #             only shifting the dates by one day and that some of these
        #             double dates are certainly clerical errors, I don't think
        #             this is going to be a big deal. This approach will also
        #             will also reduce the number of capture occations and make
        #             the parameter search process a little more robust. 
        #             
        #             I will also note that we will be using Mark to estimate
        #             monthly values for p and Phi. By having a one-day intervals
        #             in the data, we will be asking Mark to raise p and Phi to 
        #             1/30 (a single day measured in months). Since p and Phi
        #             are already probabilities (bewteen 0 and 1), this will result
        #             in values very close to 1 if the monthly estimate for p or Phi
        #             are even moderaly large. This might cause numerical problems 
        #             for the search algorithm. These problems might be addressed 
        #             by using a different link function, but removing single day
        #             intervals is another solution. 
        #             
        #             Again, I don't think this is going to mess with the results
        #             much. So, I'm going to do it. We can always take this out
        #             and see what happens. 
        #
        # check
        cat('NOCA_TSM_Source.R: Msg: Step 2.3.3: Checking back-to-back banding dates\n')

        # Check for other back-to-back banding dates.
        # Need to do this per station.
        stationList = list( c('HOME','BAMB'), 'PISP')
        for ( st in stationList ) {
            temp = subset(birbs, Station%in%st)

            # Make a dataframe to hold the unique capture dates and 
            # the length of time to the next capture date (delta). The last
            # date will have a delta of zero.
            dateCheck = tibble( Date  = temp$Date %>% unique() %>% sort(),
                                delta = c(Date[2:length(Date)] - Date[2:length(Date) - 1 ], 0) )
            index = which ( dateCheck$delta == 1 ) 
            if ( length(index) ) {
                cat('NOCA_TSM_Source.R: Warning : There are ',length(index),' back-to-back banding dates for'); print(st)
                cat('NOCA_TSM_Source.R: Warning : The pairs and number of records are:\n')
                reportDF = tibble( First = dateCheck$Date[index],
                                   Second = dateCheck$Date[index+1] )
                reportDF$numFirst  = apply(reportDF, 1, function(row) { sum( birbs$Date == row['First']   ) } )
                reportDF$numSecond = apply(reportDF, 1, function(row) { sum( birbs$Date == row['Second']  ) } )
                reportDF$numBoth   = apply(reportDF, 1, function(row) { 
                    b1 = birbs$BandNumber[ birbs$Date == row['First'] ] %>% unique()
                    b2 = birbs$BandNumber[ birbs$Date == row['Second'] ] %>% unique()  
                    which ( b1 %in% b2 ) %>% length() %>% return() 
                    } )
                print(reportDF)

                cat('NOCA_TSM_Source.R: Msg : Merging back-to-back records onto a single date\n')
                cat('NOCA_TSM_Source.R: Msg : See comments for how this is done.\n')
                cat('NOCA_TSM_Source.R: Msg : Note that this may create up to ',sum(reportDF$numBoth),' duplicate records \n')
                cat('NOCA_TSM_Source.R: Msg : These duplicate records may be removed later\n')
                for ( i in 1:nrow(reportDF) ) {
                    d1 = reportDF$First[i]
                    d2 = reportDF$Second[i]
                    if ( reportDF$numFirst[i] >= reportDF$numSecond[i] ) {
                        birbs$Date[ birbs$Date == d2 ] = d1
                    } else {
                        birbs$Date[ birbs$Date == d1 ] = d2
                    }
                }

            } else {
                cat('NOCA_TSM_Source.R: Msg : There are no back-to-back banding dates.\n')
            } 

        }

        # Step 2.3.4: Remove double records. 
        #             This section finds captures that got double recorded in the LABO database
        #             and removes them. These are not same-day recaptures. Those were
        #             removed in an earlier step. 
        # check
            cat('NOCA_TSM_Source.R: Msg: Step 2.2.3: Removing double records\n')
   
            # Step 1: Create an auxiliary data.frame that has one row 
            # for each band number and the list of capture dates
            # associated with that band number. 
            dateDouble = aggregate(Date~BandNumber, birbs, paste, collapse=",")

            # Step 2: Cycle over the unique band numbers here. For each
            # band number, check the list of dates. If there are replicates 
            # remove the redundent records. There are more efficient ways 
            # to do this in R, but this works and is sensible. 
            count = list(duplicates = 0, unique = 0 )
            for (   i in 1:nrow(dateDouble)   ) {

                # Get the band number
                bandNum        = dateDouble$BandNumber[i] 
         
                # Split dates for each bird into a list
                capDates       = strsplit(dateDouble$Date[i], ",", fixed = TRUE) %>% unlist()

                # SMD: This whole section is rather clever
                # Make a table out of each of these dates
                capDatesTable  = table(capDates) 

                # Find the dates with that occur more than once. 
                dupCapDates    = names(capDatesTable)[ capDatesTable > 1 ]
                
                if ( length(dupCapDates) ) {
                    #cat('NOCA_TSM_Source.R: Warning: Found a set of duplicate record. Details as follows:\n')

                    # Cycle over the duplicate band/date records and remove the duplicates. 
                    for ( date in dupCapDates ) {
                        # index will hold the record number of all the duplicated records
                        index = which( birbs$BandNumber == bandNum & birbs$Date == date ) 

                        reportDF     = birbs[index, c('BandNumber', 'Date', 'Code', 'CaptureTime', 'SpeciesCode', 'Station')]
                        capTimeTable = table(reportDF$CaptureTime)

                        if ( length(capTimeTable) == 1 ) {
                            #cat('NOCA_TSM_Source.R: Warning: Records appear to be duplicates\n')
                            count$duplicates = count$duplicates + 1
                        } else {
                            #cat('NOCA_TSM_Source.R: Warning: Records appear to be unique (based on CaptureTime)\n')
                            #print(reportDF)
                            count$unique = count$unique + 1
                        }

                        # Get all the duplicates except the first record
                        index = index[2:length(index)]

                        # Drop all of the duplicate records
                        birbs = birbs[ -index,  ]
                    }
                }
            }
            cat('NOCA_TSM_Source.R: Warning: Number of records that appear to be duplicates: ', count$duplicate, '\n')
            cat('NOCA_TSM_Source.R: Warning: Number of records that appear to be unique    : ', count$unique, '\n')

            # Step 3: Now double check that everything is OK. If there are still errors
            # at this point, they will require human intervension to fix. 
            # Print out a list of problem records.
            dateDouble            = aggregate(Date~BandNumber, birbs, paste, collapse=",")
            dateDouble$hasProblem = 0

            for (i in 1:nrow(dateDouble)){
              
              bandNum        = dateDouble$BandNumber[i]
              capDates       = strsplit(dateDouble$Date[i], ",", fixed = TRUE)[[1]]
              capDatesTable  = table(capDates)
              dupCapDates    = names(capDatesTable)[ capDatesTable > 1 ]
              
              if ( length(dupCapDates) != 0  ) { dateDouble$hasProblem[i] = 1 }
              
            }

            problemBirds = subset(dateDouble, hasProblem == 1 )

            if ( nrow(problemBirds) != 0  ) {
              cat("MSM_Tobin_Original.R: Error: There are still birds with double records after the fix\n")
            }

            # Step 4: Clean up the namespace. 
            remove(list = c('dateDouble','problemBirds'))

        # Step 2.2.5: Add the season definitions. 
        #             The breeding season will be defined as dates between 
        #             April 1st and August 7th, inclusive. 
        # check
            cat('NOCA_TSM_Source.R: Msg: Step 2.2.4: Adding seasons\n')
            index               = (4 <= birbs$Month) & ( birbs$Month < 8 | ( birbs$Month == 8 & birbs$Day <= 7 ) )
            birbs$Season        = 'Nonbreeding'
            birbs$Season[index] = 'Breeding'

            # Make sure this worked correctly. 
            if ( F ) {
                check       = birbs[ , c('Date','Season') ] %>% unique()
                check$year  = year(check$Date)

                for (   y in (check$year %>% unique() %>% sort())   ) {
                    cat('NOCA_TSM_Source.R: Msg: Year = ', y ,'\n')
                    cat('NOCA_TSM_Source.R: Msg: Breeding season dates are:\n')
                    check$Date[ check$year == y & check$Season == 'Breeding'] %>% sort() %>% print()
                
                }
            }

    ################################################################################
    # Step 2.3: Create capture date dataframe
    #
    #           At this point we need to also get a full list of capture dates 
    #           from the "raw" banding database. Technically, we cannot use the 
    #           NOCA portion of the database to do this because if we captured no 
    #           NOCA's on a date, then that capture occasion will not appear in the 
    #           NOCA database. This omission could mess with the estimates of p, 
    #           the capture probability. However, there are two points to raise 
    #           here. First, if we captured no birds at all on a banding date, then
    #           trying to extract the capture occasions from the banding database 
    #           is the wrong approach and we should use another part of the banding 
    #           log to get this. Second, I think we could use just the NOCA capture
    #           dates for analyzing the NOCAs. If there was a banding date when we 
    #           didn't capture NOCAs, this would just be included as a single p 
    #           probability for that date with no associated phi. But this value 
    #           could just be folded into the next p probability. Since Mark will 
    #           know the length of time between capture dates and will raise p to 
    #           the appropriate value, I'm not sure this is important. 
    # 
    #           Based on code later in Eric's version of the script, the capture date
    #           dataframe needs to have the following columns:
    #           
    #           season   = breeding vs non-breeding.
    #           yearReal = calendar year, (2010, 2011, etc ...)
    #           year     = year count starting at 1.
    #           smdYear  = special year that counts years synchronous with the start
    #                      of the breeding season.
    #           
    #           However, some of these details, like season, are redunant with 
    #           features from step 2.2.4. It would be worthwhile checking whether or
    #           not step 2.2.4 is needed. 
    # check
    ################################################################################
    cat('NOCA_TSM_Source.R: Msg: Step 2.3: Creating capture date dataframe\n')

    cat('NOCA_TSM_Source.R: Msg: Step 2.3: NOTE: This needs to be synchronized with the reduction of the\n')
    cat('NOCA_TSM_Source.R: Msg: Step 2.3: NOTE: banding data in the next section to make sure the same \n')
    cat('NOCA_TSM_Source.R: Msg: Step 2.3: NOTE: station and date range is selected. \n')
    temp      = subset( birbs, Station %in% c('HOME','BAMB') & Date > as.Date('2010-03-30') ) 
    capDateDF = tibble( Date = temp$Date %>% unique() %>% sort(), 
                        delta = c(0, Date[2:length(Date)] - Date[2:length(Date)-1] ),
                        month = month(Date),
                        day   = mday(Date) )

    # Mesure the intervals in months. 
    capDateDF$monthlyinterval = capDateDF$delta/30

    index                   = (4 <= capDateDF$month) & ( (capDateDF$month < 8 ) | ( capDateDF$month == 8 & capDateDF$day<= 7 ) ) 
    capDateDF$season        = 'Nonbreeding'
    capDateDF$season[index] = 'Breeding'
    capDateDF$yearReal      = year(capDateDF$Date)
    capDateDF$year          = capDateDF %>% with( { yearReal - min(yearReal) + 1 })
    capDateDF$occasion      = cumsum(capDateDF$monthlyinterval)

    # Build the smdYear which counts year starting at April 1. 
    firstYear         = capDateDF$Date %>% year() %>% min()
    lastYear          = capDateDF$Date %>% year() %>% max()
    numYear           = lastYear - firstYear + 1
    breedYearDefn     = seq( paste(firstYear,'-04-01',sep='') %>% as.Date() , length=numYear, by='year') 
    capDateDF$smdYear = findInterval(capDateDF$Date, breedYearDefn)

    # copy everything over to the name cap.int, which is the
    # name used by Eric's original code. We'll refactor this 
    # eventually. 
    cap.int = capDateDF

################################################################################
################################################################################
## Step 3: Strip the data down to just the species, station and dates 
##         that we want
## check
################################################################################
################################################################################

    cat('NOCA_TSM_Source.R: Msg: Step 3: Subsetting by species, station and date\n')
    working = subset( birbs, SpeciesCode == 'NOCA' & Station %in% c('HOME','BAMB') & Date > as.Date('2010-03-30') ) 

    ################################################################################
    # Step 3.1: Check net 99
    #           We need to check how many NOCAs are we recaptured in net 99.
    #           Net 99 is a designator of all one-off nets used during banding.
    #           We need to find out how many captures and how many birds were
    #           captured in these special nets to assess whether or not they 
    #           will have a significant impact on the results and if we need
    #           to modify the model structure to account for them. 
    #           Refer to project log v.2 on 21feb2019 to see if this needs changing
    # check
    ################################################################################
        cat('NOCA_TSM_Source.R: Msg: Step 3: Reporting Net 99 records\n')
        cat('NOCA_TSM_Source.R: Msg: Number of NOCA captures in net 99 = ', sum(working$Net == 99), '\n' )
        cat('NOCA_TSM_Source.R: Msg: Number of unique NOCAs in net 99  = ', subset(working, Net == 99)$BandNumber %>% unique() %>% length(), '\n' )

################################################################################
################################################################################
## Step 4: Clean up the data. 
##         In this section we need to fix the anomolies with 
##         the sex classification for birds, their age, fat, etc ...
##
##
################################################################################
################################################################################
cat('NOCA_TSM_Source.R: Msg: Step 4: Fixing anomolies\n')

    #################################################
    # Step 4.1: Sex sanity check. (For NOCA only.)
    #
    # There are some birds with ambiguous sex records. 
    # We need to fix these. 
    #
    # check
    #################################################
    cat('NOCA_TSM_Source.R: Msg: Step 4: Fixing sex\n')

        # Step 1: Fix blank sex records
        #         An inspection of the sex records for birds reveals that there are 
        #         four values for sex: "M", "F", "U" and "".
        #         We are going to reclassify the "" records as "U".
        # check
        working$Sex[ working$Sex == ''] = 'U'

        # Step 2: Find all birds with ambiguous sex records and fix them.
        #
        # check

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
                    # Find the number of records for this bird
                    numRecords = nrow( subset(working, BandNumber == bn ) )

                    # Find the number of times the bird was classified as male
                    numAsMale   = nrow( subset(working, BandNumber == bn & Sex == 'M' ) )

                    # Find the number of times the bird was classified as female
                    numAsFemale = nrow( subset(working, BandNumber == bn & Sex == 'F' ) )
                    
                    # Find the number of times the bird was classified as unknown
                    numAsNeuter = nrow( subset(working, BandNumber == bn & Sex == 'U') )
                    
                    # If the bird was always classed as U, there is nothing we can do.
                    # In the else part, we know that the bird was assigned at least one
                    # of male or female. So we will select the sex based on which 
                    # catagory had the most hits
                    if ( numAsNeuter == numRecords ) {
                        cat('NOCA_TSM_Source.R: Msg: Found a bird with only sex == U, band number = ',bn,'\n')

                    } else if ( numAsMale > numAsFemale ) {
                        working$Sex[ working$BandNumber == bn ] = 'M'

                    } else if ( numAsMale < numAsFemale ) {
                        working$Sex[ working$BandNumber == bn ] = 'F'

                    } else {
                        # In this case numAsMale == numAsFemale, so sex = U
                        # We might be able to solve this by checking CP, BP, weight and wing length 
                        # For now, we'll just report the number of such birds.
                        cat('NOCA_TSM_Source.R: Warning: Found a bird with #M == #F\n')
                        cat('NOCA_TSM_Source.R: Warning: Using last non-U sex code as sex\n')
                        reportDF = working[ working$BandNumber == bn, c('BandNumber','Date','Sex','CP','BP')]
                        print(reportDF)

                        theBird    = subset(working, BandNumber == bn & Sex != 'U' )
                        theBird    = theBird[ order(theBird$Date), ]
                        theBirdSex = theBird$Sex[ nrow(theBird) ] 

                        working$Sex[ working$BandNumber == bn ] = theBirdSex 
                    }
                }
        } else {
            cat('NOCA_TSM_Source.R: Msg: All birds have unique sex\n')
        }

    ################################################################################
    # Step 4.2: Age Sanity check
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
    # check
    ################################################################################

        cat('NOCA_TSM_Source.R: Msg: Step 4: Fixing age\n')

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

        # Step 1: Create an auxiliar data.frame that contains the list of unique ages given to each bird
        #
        # check
        ageDF = aggregate(Age~BandNumber, working, function(age) { age %>% unlist() %>% unique() %>% sort() %>% paste(collapse=',')} )

        # Step 2: Remove any birds that where only ever given an age class of Unknown.
        #
        # check
        index = ageDF$Age == 'Unknown'
        cat('NOCA_TSM_Source.R: Msg: Removing ',sum(index),' birds that where only ever classed as Age == Unknown (U, \"\", F, K, X)\n')
        working = subset(working, !(BandNumber %in% ageDF$BandNumber[index]) )

        # Clean up the name space.
        remove(ageList, ageDF, index)
       
    ################################################################################
    # Step 4.3: Initial Age Assignment.
    #            In this section we are creating a new column to store the initial
    #            age of the bird. This will allow us to modify the PIM for the 
    #            model to compute a different p and phi for junenile birds vs adult birds.
    #            
    # check
    ################################################################################
        cat('NOCA_TSM_Source.R: Msg: Step 4: Creating initial age\n')

        # The sequence of events is:
        #          1) Make sure the records are ordered by date
        #          2) Create an auxiliary dataframe collating the list of ages by band number
        #          3) Modify the auxiliary dataframe by adding a column of initial ages.
        #          4) Push initial age into the working dataframe
        #

        # Step 1: Make sure the database is sorted by date.
        #             To make sure we actually get the first age, we 
        #             need to sort the data frame by date.
        #         
        # check 
        working = working[ order(working$Date), ]

        # Step 2: Next, create an auxiliary dataframe that 
        #             lists all ages associated with each band number
        #             Equivelent to Step EJT-1 below
        #
        # check
        ageDF         = aggregate(Age ~ BandNumber, working, paste, collapse=',')

        # Step 3: Make sure the ages occure in the correct order 'Juvenile' -> 'Adult'  
        #
        # check
        ageDF$isOrdered  = apply(ageDF, 1, function(row) { 
                            temp = row['Age'] %>% strsplit(split=',') %>% unlist()
                            temp = temp[ temp != 'Unknown' ]
                            temp = match( temp, c('Juvenile', 'Adult') )
                            if ( temp %>% is.na() %>% sum() ) { return(F) }
                            return( ( (temp != sort(temp)) %>% sum()) == 0  )
                            } )
        if ( sum(!ageDF$isOrdered) ) {
            cat('NOCA_TSM_Source.R: Warning: Ages do not appear to occure in the correct order\n')
            cat('NOCA_TSM_Source.R: Warning: Problem records are:\n')
            reportDF = subset(working, BandNumber %in% ageDF$BandNumber[ ageDF$isOrdered == F], c('BandNumber','Date','Age','Sex'))
            print(reportDF[ order(reportDF$BandNumber, reportDF$Date), ])
        }

        # Step 4: Next, get the initial age for each bird
        #
        # check 
        ageDF$initAge = apply(ageDF, 1, function(row) { (row['Age'] %>% strsplit(split=',') %>% unlist() )[1] } )

        # Step 5: Add the initial age to each record in "working"
        #
        # check 
        working$initAge = NA 
        for ( i in 1:nrow(ageDF) ) {
            band    = ageDF$BandNumber[i]
            initAge = ageDF$initAge[i]
            working$initAge[ working$BandNumber == band] = initAge
        }

        # Step 6: Reclassify birds with unknown initial age as adults.
        #         
        #         An examination of the records with unknown initial age
        #         indicatest that these birds were only ever classifed
        #         as either unknown or adult. So, I'm just going to classify
        #         them all as adults. 
        #         
        # check 
        
        # Step 6.1: Report how many bands and records will be changed.
        # 
        # check
        cat('NOCA_TSM_Source.R: Msg: Chaning records with unknown initial age to \"Adult\"\n')
        cat('NOCA_TSM_Source.R: Msg: Number of birds involved   = ',sum( ageDF$initAge == 'Unknown' ) ,'\n')
        cat('NOCA_TSM_Source.R: Msg: Number of records involved = ',sum( working$initAge == 'Unknown'),'\n')
        cat('NOCA_TSM_Source.R: Msg: Affected records are:\n')
        print(ageDF[ ageDF$initAge == 'Unknown', ])

        # Step 6.2: Change their initial age status
        # 
        # check
        working$initAge[ working$initAge == 'Unknown' ] = 'Adult'

        # clean up the namespace
        remove(ageDF) 
    
    ################################################################################
    # Step 4.4: Prep the fat data
    #
    # The original banding data provides the following
    # fat classes:
    # N = None
    # T = Trace
    # L = Light
    # H = Half
    # F = Full
    # B = Bulging
    # G = Greatly bulging
    # V = Very excessive 
    # "" = ?
    # ! = OMG, how is that bird able to fly
    # 
    # I don't think we can really use all these classes
    # since I'm not sure we have enough birds with values
    # in each class to be statisticaly significant. 
    #
    # For this analysis we will use just three classes: None, Low and High 
    # with the following mapping:
    #
    # None = N
    # Low  = T, L
    # High = H, F, B, G, V, !
    #
    # We'll have to check how many records have no entry.
    #
    # check
    ################################################################################
        cat('NOCA_TSM_Source.R: Msg: Step 4: Fixing fat\n')

        # Step 1: Reclass the fat data.
        #
        #
        working$Fat[ working$Fat %in% c('N') ]                          = 'None'
        working$Fat[ working$Fat %in% c('T', 'L') ]                     = 'Low'
        working$Fat[ working$Fat %in% c('H', 'F', 'B', 'G', 'V', '!') ] = 'High'
        working$Fat[ working$Fat %in% c('') ]                           = 'Unknown'

        
        cat('NOCA_TSM_Source.R: Msg: Number of records in each fat class:\n')
        print( table(working$Fat) )

        # Step 2: Deal with records where fat == 'Unknown'
        #         
        #         An inspection of the records/birds where fat is either
        #         unknown or high indicates that there are 41 records
        #         with high fat values, which accounts for 37 birds.
        #         To be honest, with such low numbers, compared to the total
        #         data set (num NOCA records = 1699), I'm not sure fat is
        #         really going to contribute much to  understanding 
        #         variation in p and phi. We'll include it for now, 
        #         but I don't think it will matter. 
        #         
        #         There are 18 records with unknown fat values, which 
        #         accounts for 18 birds. Of these birds, two were only
        #         captured once and two had high fat, low fat and unknown
        #         fat values. I'm going to suggest we reclassify all unknown
        #         fat values as "None". I don't want to drop birds with unknown
        #         fat records because some of these birds have long capture
        #         histories (once bird was captured 16 times, several others 
        #         have at least 5 captures). This is valuable data. Given 
        #         that I don't think fat really matters, I'm reticent to 
        #         drop these records. 
        #         

        fatDF = aggregate(Fat ~ BandNumber, working, function(fat) { fat %>% unlist() %>% paste(collapse=',') } )

        fatDF$numNone    = apply(fatDF, 1, function(row) {
                                         temp = row['Fat'] %>% strsplit(split=',') %>% unlist()
                                         (temp == 'None') %>% sum() %>% return() } )

        fatDF$numLow    = apply(fatDF, 1, function(row) {
                                         temp = row['Fat'] %>% strsplit(split=',') %>% unlist()
                                         (temp == 'Low') %>% sum() %>% return() } )

        fatDF$numHigh   = apply(fatDF, 1, function(row) {
                                         temp = row['Fat'] %>% strsplit(split=',') %>% unlist()
                                         (temp == 'High') %>% sum() %>% return() } )

        fatDF$numUnknown = apply(fatDF, 1, function(row) {
                                         temp = row['Fat'] %>% strsplit(split=',') %>% unlist()
                                         (temp == 'Unknown') %>% sum() %>% return() } )

        cat('NOCA_TSM_Source.R: Msg: Number of records where fat == \"Unknown\" = ',sum(working$Fat == 'Unknown'),'\n')
        cat('NOCA_TSM_Source.R: Msg: Number of birds with \"Unknown\" fat       = ',nrow(fatDF[fatDF$numUnknown > 0, ] ),'\n')
        cat('NOCA_TSM_Source.R: Msg: Details for \"Unknown\" fat birds:\n')
        print( subset(fatDF, numUnknown > 0) )

        cat('NOCA_TSM_Source.R: Msg: Chaning \"Unknown\" fat records to \"None\"\n')
        cat('NOCA_TSM_Source.R: Msg: What we really need is a method for handling missing data\n')

        working$Fat[ working$Fat == 'Unknown' ] = 'None'

        # Clean up the namespace.
        remove(fatDF)

    ################################################################################
    # Step 4.5: Prep the mean wing length data
    #
    # check
    ################################################################################
        cat('NOCA_TSM_Source.R: Msg: Step 4: Fixing mean wing data\n')

        wingDF      = aggregate(RightWing ~ BandNumber, working, paste, collapse = ',')
        wingDF$N    = apply(wingDF, 1, function(row) {row['RightWing'] %>% strsplit(split=',') %>% unlist() %>% as.numeric() %>% is.na() %>% not() %>% sum() } )
        wingDF$mean = apply(wingDF, 1, function(row) {row['RightWing'] %>% strsplit(split=',') %>% unlist() %>% as.numeric() %>% mean(na.rm=T) } )
        wingDF$sd   = apply(wingDF, 1, function(row) {row['RightWing'] %>% strsplit(split=',') %>% unlist() %>% as.numeric() %>% sd(na.rm=T) } )

        numNoMeanWing = wingDF %>% subset(N == 0) %>% nrow()
        if ( numNoMeanWing ) {
            cat('NOCA_TSM_Source.R: Error: Number of birds with no mean wing length (all data == NA): ',wingDF %>% subset(N == 0) %>% nrow(),'\n')
            wingDF %>% subset(N == 0) %>% print()
            stop()
        } else {
            cat('NOCA_TSM_Source.R: Msg: All birds have a mean wing length\n')
        }

        working$meanWing = 0
        for ( bn in wingDF$BandNumber ) {
            working$meanWing[working$BandNumber == bn] = wingDF$mean[ wingDF$BandNumber == bn]
        }

        # clean up the namespace
        remove(fatDF)

################################################################################
################################################################################
## Step 5: Create Mark formatted data
##
##
################################################################################
################################################################################

    if ( Version == 'SMD' ) {
        # Step 5.1: Create the dataframe that will hold the mark formatted data. 
        # As a first step, just add the band numbers. After the band numbers are
        # added, we need to add the following columns:
        # Sex
        # MeanWing
        # Malaria
        # freq ?
        # IntAge = initial age
        # season 
        # ch = capture history
        #
        markData = tibble( id         = NOCA$BandNumber %>% unique() %>% sort() )

        # Step 5.2: Add Sex
        #         At this point, the working dataframe should be fixed 
        #         so that each bird has only a single sex. 
            sexDF        = aggregate(Sex~BandNumber, working, function(sex) { sex %>% unlist() %>% sort() %>% unique() %>% paste(collapse=',')} )
            sexDF$numSex = apply(sexDF, 1, function(row) { row[2] %>% strsplit(split=',') %>% unlist() %>% length() })
            if ( sum(sexDF$numSex > 1) ) {
                stop('NOCA_TSM_Source.R: Error: One or more birds have multiple sex determinations\n')
            }
            markData = merge(markData, sexSF, by = BandNumber )
            remove('sexDF')

        # Step 5.3: Add MeanWing
            wingDF       = aggregate(RightWing~BandNumber, working, mean, na.rm=T)
            markData     = merge(markData, wingDF, by = BandNumber) %>% rename(MeanWing = RightWing)
            remove('wingDF')

        # Step 5.4: Add Malaria
            malariaDF    = aggregate(Malaria~BandNumber, working, function(mal) {mal %>% unlist() %>% unique() %>% paste(collapse=',')} )
            markData     = merge(markData, malariaDF, by = BandNumber)
            remove('malariaDF')
        
        # Step 5.5: Add freq/CJS
        #         In Eric's code below CJS appears to be just a column of 1's.
        #         Moreover, CJS/freq does not appear to be used. So, I'm going
        #         to skip it for now.
            
        # Step 5.6: Add IntAge
            ageDF    = aggregate(Age~BandNumber, working, function( age ) { (age %>% unlist())[1]   } )
            markData = merge(markData, ageDF, by = BandNumber )
            markData = markData %>% rename(IntAge = Age)
            remove('ageDF')

        # Step 5.7: Add season
        #         I'm not sure how this one is supposed to work.
        #         I'll have to ask Eric.

        # Step 5.8: Add capture history
        markData$ch = ''
        dateDF = aggregate(Date ~ BandNumber, working, paste, collapse=',')
        for ( bandNum in dataDF$BandNumber ) {
            dateList = dateDF$Date %>% strsplit(split=',') %>% unlist() %>% as.Date()
            ch       = c(0,1)[ c(capDateDF$Date %in% dateList + 1) ] %>% paste(collapse='')
            markData$ch[markData$BandNumber == bandNum] = ch
        }

        # Step 9: Give all the columns their correct type.
        markData$Sex     = as.factor(markData$Sex)
        markData$IntAge  = as.factor(markData$IntAge)
        markData$Malaria = as.factor(markData$Malaria)


    } else {
        # SMD: Several things are happening in this section of Eric's code. 
        # SMD: Here is a list of the events and what I've done to move/recreate
        # SMD: these events in my version of the code:
        # SMD: 
        # SMD:      1) Get list of unique band numbers and put it in NOCABands. 
        # SMD:         NOCABands isn't used, so I've omitted it.
        # SMD:         
        # SMD:      2) Add a column called CJS to NOCA dataframe. 
        # SMD:         CJS appears to be just a column of ones. The
        # SMD:         column gets renamed "freq" later, but it isn's used,
        # SMD:         so I've omitted it.
        # SMD:         
        # SMD:      3) Add mean wing length to the NOCA dataframe. 
        # SMD:      4) Remove birds that do not have mean wing lengths from NOCA dataframe.
        # SMD:         I've added code above in Step 4.5 and 5.1 to deal with these two events.
        # SMD:         
        # SMD:      5) Make several columns in the birbs dataframe factors. 
        # SMD:         NOTE: I don't appear to have handled this yet. 
        # SMD:         
        # SMD:      6) Create a dataframe to hold the data that will be given to Mark.
        # SMD:         The dataframe, called Marky, has band numbers and a place to hold
        # SMD:         hold capture histories.
        # SMD:         
        # SMD:         This is handled in my Step 5 above. 
        # SMD:         
        # SMD:      7) Remove birds with no fat data from NOCA dataframe. 
        # SMD:         This is handled in Step 4.4 above.
        # SMD:         
        # SMD:      8) Make an auxiliary dataframe used to create capture histories.
        # SMD:         This dataframe gets called NOCAfat, which is non-intuitive and confusing. 
        # SMD:         
        # SMD:         This is handled in Step 5 above. 
        # SMD:         
        # SMD:      9) Create an auxiliar dataframe, called NOCACaps, that lists capture dates 
        # SMD:         for each band number. 
        # SMD:     10) Merge Marky and the auxiliar dataframe
        # SMD:     11) Actually create the capture history
        # SMD:         
        # SMD:         These last three steps make the final dataframe to hand to Mark. 
        # SMD:         This is handled in Step 5 above. 
        # SMD:         
        # SMD:         

            ################################################################################
            # Step 5: Create Summary Variables
            #
            ################################################################################
            cat('MSM_Tobin_Original.R: Msg: Creating summary variables')

            # Get the NOCA band numbers
            NOCABands <- unique(NOCA$BandNumber)

            # Add a column to NOCA dataframe
            NOCA$CJS  <- 1

            # Step 1: Add mean wing length to the NOCA data frame.  
            #         Create an auxiliary dataframe that contains the mean for each bird/band number
                MeanTable <- aggregate(RightWing~BandNumber, NOCA, mean, na.rm=T)

                # Add a column to NOCA dataframe
                NOCA$MeanWing <- 0

                # Push the mean wing length from the auxiliary dataframe into the NOCA dataframe
                for ( band in MeanTable$BandNumber ) {
                  NOCA$MeanWing[ NOCA$BandNumber == band ] = MeanTable$RightWing[ MeanTable$BandNumber == band]
                }

            # Step 2: Remove any birds that have a mean wing length of NA or 0.
            #         Report the number of birds removed. 
                beforeWingRemoval <- length(unique(NOCA$BandNumber))
                NOCA <- subset(NOCA, !is.na(NOCA$MeanWing))
                afterWingRemoval <- beforeWingRemoval - length(unique(NOCA$BandNumber))
                nowingNOCA <- NOCA[which(NOCA$MeanWing == 0),]
                NOCA <- NOCA[which(NOCA$MeanWing != 0),]
                RemovedNoWing <-length(unique(NOCA$BandNumber)) - (length(unique(NOCA$BandNumber)) - length(unique(nowingNOCA$BandNumber))) + afterWingRemoval


                if (length(unique(nowingNOCA$BandNumber)) != RemovedNoWing){
                    cat('MSM_Tobin_Original.R: Msg: Removed birds without winglength. This removes', RemovedNoWing ,'NOCA observations from the population, but there are',length(unique(nowingNOCA$BandNumber)) ,'NOCA individuals. This means some are captured more than once without a winglength. Investigate.\n')
                }else {
                    cat('MSM_Tobin_Original.R: Msg: Removed birds without winglength captured only once. This removes',length(unique(nowingNOCA$BandNumber)) ,'NOCA individuals.\n')
                }


            ################################################################################
            # Make columns that should be treated as factors into actual factors.
            # They have been stored as strings up until this point.
            # 
            ################################################################################
            birbs$Malaria = as.factor(birbs$Malaria)
            birbs$Station = as.factor(birbs$Station)
            birbs$Sex     = as.factor(birbs$Sex)
            birbs$Net     = as.factor(birbs$Net)



            ################################################################################
            # Step 6: Create MARK sheet with MSM History
            #
            ################################################################################
            Marky <- as.data.frame(unique(NOCA$BandNumber))
            setnames(Marky, "unique(NOCA$BandNumber)", "BandNumber")
            #Using BandNumber for merge function below. This prevents mishaps from ordering
            ##order doesn't matter, it'll match by function. 

            #Create column to hold capture histories
            Marky$CH <- 0

                # SMD:
                # SMD: I've moved this code up so that all the Fat changes are dealt
                # SMD: with in one place. 
                # SMD:
                if ( Version == 'EJT' ) {
                    ################################################################################
                    # Here you make your MSM variable in question
                    #
                    ################################################################################
                    # FAT
                    # we have turned missing into U above
                    beforeFatRem  <- length(unique(NOCA$BandNumber))
                    NOCA <- NOCA[which(NOCA$Fat != 'U'),]
                    afterFatRem  <- length(unique(NOCA$BandNumber))
                    diffy = beforeFatRem - afterFatRem

                    cat('MSM_Tobin_Original.R: Msg: Removed birds without fat captured only once. This removes',diffy ,'NOCA individuals.\n')

                }

            # SMD: I'm not sure what this code is supposed to do. It claims to be about
            # SMD: fat, but the current active code doesn't deal with fat at all. 
            # SMD:
            # SMD: The NOCAfat dataframe will have the following columns: BandNumber, Date and CJS
            # SMD: I think CJS is just a column of 1's. 
            # SMD:
                # NOCAfat = aggregate(NOCA$Fat~NOCA$BandNumber + NOCA$Date, NOCA, paste, collapse=',')
                # create DF with unique fat for each bird on each day of its individual capture
                # Is and should be same length as NOCA
                NOCAfat = aggregate(NOCA$CJS~NOCA$BandNumber + NOCA$Date, NOCA, paste, collapse=',')

                #Change the names to make life easier. 
                setnames(NOCAfat, 'NOCA$BandNumber', 'BandNumber')
                setnames(NOCAfat, 'NOCA$Date',       'Date')
                setnames(NOCAfat, 'NOCA$CJS',        'Fat')


            # SMD: Create an auxiliary dataframe that lists the capture dates for each band number
                # This will be how we create the different MSMs. Let's try with CJS, make sure it works
                NOCAcaps <- aggregate(Date~BandNumber, NOCA, paste, collapse=",")
                #names(NOCAcaps) = c('BandNumber', 'DateList')

            # SMD: merge the NOCAcaps auxiliary dataframe with the Marky dataframe. 
            # SMD: After this Marky should have the following columns: BandNumber, CH, Date
            # SMD: where:
            # SMD: BandNumber are band numbers
            # SMD: CH is the blank capture history
            # SMD: Date is the list of capture dates for each bird. 
            # SMD:
                # Go through by hand and make sure they are correct
                # get a random sample, check them puppies
                # creates a df with BandNumber, blank CH, and the DateList. This marries dates of capture with band# and CH
                # You get the datelist from NOCAcaps applied to Marky. Guess we could've just done one dataframe here? Don't see why we need two...
                Marky = merge( Marky, NOCAcaps, by=c('BandNumber') )

            # SMD: Unused code:
                #Below we create the capture histories from the date lists
                #using full date index, you make string at that length of zeros subbing in your 
                ##variables you toook out in the above subsection
                    if ( F ) {
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

            # SMD: This creates the capture history for each bird.
            # SMD: What I don't like about this code is the dataframe
            # SMD: NCOAfat isn't really being used to deal with fat.
            # SMD: Instead the name is just being reused in a confusing 
            # SMD: and non-intuitive manner. 
            # SMD: 
                # This is for using CJS models and Pradels
                    if ( T ){ 
                        # SMD: Cycle over the birds. 
                        for ( r in 1:nrow(Marky) ) {

                            # SMD: Get just the current bird
                                # Creates dataframe with BN, each dates, and fat reading
                                # new one made for each bird
                                thisBird = subset(NOCAfat, BandNumber == Marky$BandNumber[r])

                            # SMD: Merge the full list of capture dates 
                            # SMD: and the data for the current bird
                                # Create holding frame that is datelist, but added band number and fat
                                temp = merge(datelist, thisBird, by.x='Date', by.y='Date', all.x=TRUE)

                            # This will sub in zeros for fat on days you didn't capture. 
                                temp$Fat[is.na(temp$Fat)] = '0'

                            # This collapses the fat column into a ch. Puts back into marky by index position
                                Marky$CH[r] = paste( temp$Fat, collapse='')
                        }
                    }
    }

################################################################################
################################################################################
# SMD: This section of Eric's code produces a dataframe that
# SMD: merges the capture history and the covariates for each bird.
# SMD: This code is no longer needed as it is handled in Step 5 above.  
# SMD: 
################################################################################
################################################################################

    if ( F ) {
        ################################################################################
        # Step 7: Add summary variables to the MARK data
        ################################################################################
        
        # This plucks off the first reading for each bird. 
        # Since we applied sex and winglength for everyone, 
        # this should be fine to pull the variables from.
        # You'll have to modify the pims from the non summary data, 
        # but how useful is that? Your msm captures daily, unique variation
        NOCA_Unique <- NOCA
        NOCA_Unique <- NOCA_Unique %>% distinct(NOCA$BandNumber, .keep_all = TRUE)

        # Pare down the NOCA frame from merging
        NOCA_Unique <- NOCA_Unique[c("BandNumber", "Sex", "MeanWing", "Malaria", "CJS", "IntAge","season")]

        # SMD: This will merge the the covariates from NOCA_Unique 
        # SMD: and the capture histories from Marky.
            # Merge on bandnumber
            Marky = merge( Marky, NOCA_Unique, by=c('BandNumber') )

        # SMD: Blank the DateList columns from Marky.
        # SMD: Doesn't really remove it, just sets all
        # SMD: the values to NULL.
            #remove the datelist, not needed anymore
            Marky$DateList <- NULL

        #combine into mark dataframe, remove everyone who didn't have a mean wing
        ##loses 40 individuals
        setnames(Marky, "BandNumber", "id")
        setnames(Marky, "CJS", "freq")
        setnames(Marky, "CH", 'ch')
        #give them mark names
    }

################################################################################
################################################################################
# SMD: This section creates a dataframe with the length of time between 
# SMD: capture events. Time here is measured in months. This code is not
# SMD: needed any longer since I deal with this earlier where all the other date
# SMD: functions are handled.
# SMD: 
################################################################################
################################################################################

    if ( F ) {
        ################################################################################
        # Step 8: Make capture intervals (not equal, so needs to be set)
        ################################################################################

        # Create a new dataframe called ranker to rank the dates.
        # This makes a one column dataframe with each date as a seperate entry
        # sorted in order of dates. 
            cat('Making Cap.Int data\n')
            ranker=data.frame( date = sort( unique(datelist$Date)) )

        # First create a new string named interval by subtracting the subsequent capture dates
        # First step is to put #caps into a var
            caps     = nrow(ranker) #Number of captures
            interval = rep(NA,caps) #Empty list with N/A

        # This fills in each space in interval with the number of days between capture occassions
            for(i in 2:nrow(ranker)){
              interval[i] = (ranker$date[i]-ranker$date[i-1])
            }

        # Message to track where we are
            cat('Create Cap.Int Data\n')

        # Now create a data frame
        # This gives the interval since last and applies to date. First is NA since new
            cap.int=data.frame(ranker$date,interval)
            cap.int$occIndex = 1:nrow(cap.int)

        # Create a new column in ranker data frame to rank the dates
        # I'm not sure this is still neccessary since we sort things already. 
        # This may be an artifact of Binab.
            ranker$rank<-1:nrow(ranker)

        # Add new column for interval in months
        # This gives you the amount of time in between captures in terms of months
            cat('Create interval data\n')
            cap.int$monthlyinterval=round(cap.int$interval/30,digits=3)

        # At the end, cap.int should have the following columns:
        #
        # ranker.date = a ordered list of capture dates. 
        #
        # interval = The interval, in days, a date and the previous capture date.
        #            The first element is NA.
        #
        # occIndex = a sequential index for the dates dates. 
        #
        # monthlyinterval = the intervals measured in months. 
        #
        #
    }


################################################################################
################################################################################
# SMD: In this section Eric is making the process data 
# SMD: and the design data. 
# SMD:
################################################################################
################################################################################
    if ( Version == 'SMD' ) {
        NOCA.proc = process.data( markData, 
                                  model = 'Pradrec', groups = c('Sex', 'season', 'IntAge'),
                                  time.intervals = capDateDF$delta[2:nrow(capDateDF)] )

        NOCA.dll = make.design.date(NOCA.proc)

        save(NOCA.ddl,  file = filenameDLL)
        save(NOCA.proc, file = filenameProc)

    } else {
        ################################################################################
        # Step 9: Massage for MARK (Remove missing values)
        #
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
    }

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

################################################################################
################################################################################
# SMD: Add a column to cap.int that measures the length of time
# SMD: from the start of the banding process to each subsequent capture date. 
# SMD: This code isn't needed any longer as I have dealth with this
# SMD: in Step 2.3 where all the other date/time information is handled. 
# SMD:
# SMD: This section further modifieds cap.int, the dataframe
# SMD: that keeps track of the capture dates. Below is a list
# SMD: of the tasks performed by this section allow with a
# SMD: description of how these tasks are achived in my version
# SMD: of the code. Since all of these tasks are date related
# SMD: I have moved all these tasks up to Step 2.3 where all 
# SMD: of the other date related tasks are handled. 
# SMD: 
# SMD: 1) Add a column to cap.int called seasons of type char
# SMD:    Moved to step 2.3
# SMD:
# SMD: 2) Add a column to cap.int called seasons2 of type char
# SMD:
# SMD: 3) Create an auxiliar dataframe to contain the details that
# SMD:    define each season. 
# SMD:
# SMD: 4) Add yearReal to cap.int and fill it with NA's
# SMD:    This column will store the actual calendar year 
# SMD:    (2010, 2011, etc ...)
# SMD:    Moved to step 2.3
# SMD:
# SMD: 5) Populate yearReal with year values.
# SMD:    Moved to step 2.3
# SMD:
# SMD: 6) Add a year count column to cap.int
# SMD:    Moved to step 2.3
# SMD:
# SMD: 7) Assign dates to seasons
# SMD:    Moved to step 2.3
# SMD:
# SMD: 8) Make durration values. 
# SMD:
################################################################################
################################################################################

if ( F ) {
    # Step 1: Add a column to cap.int called seasons of type character. 
    cap.int$seasons=character(nrow(cap.int))

    # Step 2: Add a column to cap.int called seasons2 of type character. 
    cap.int$seasons2=character(nrow(cap.int))

    cat('Create Season and year Data\n')

    # Step 3: Make an auxiliary dataframe that defines the start and end
    #         date of seasons. 
    seasons= data.frame(
      season=c("Breeding","NonBreeding","NonBreeding"),
      offset  =c(1, 1, 0 ),
      start=c ("04-01", "08-08", "01-01"),
      end  =c("08-07","12-31", "03-31"), stringsAsFactors=F
    )

    # Step 4: SpAdd a column to cap.int called yearReal, filll is NA's
    cap.int$yearReal <- NA

    # Step 5: Cycle over the rows of cap.int. For each row
    #         extract the year and store it in yearReal.
    #         This is really the hard way to to do this. 
    for (i in 1:nrow(cap.int)) {
        # create a year factor for cap.int, the reference date dataframe
        # you're gonna get warnings, they are fine to ignore
        cap.int$yearReal[i] <- as.numeric( strsplit(as.character(cap.int$ranker.date[i]),"-",fixed = TRUE)[[1]] )
    }

    # Step 6: Add a column to cap.int that gives the year count
    #         starting from 1. 
    cap.int$year = cap.int$yearReal - min(cap.int$yearReal) + 1

    # Step 7: Populate the seasons columns and
    #         build smdYear

    # Creating the list of unique years
    # Probably need to coerce into a date, but meh, it works?
    # cap.int$year <- as.Date(cap.int$year, "%Y")
    yearlist=unique(cap.int$yearReal)

    #yearlist= unique(format(cap.int$ranker.date,"%y")) does the same thing as above code

    # This double-nested for-loop assigns seasons to each
    # date. It also creates smdYear is a special year count designator.
    # smdYear counts years as starting on April 1 and 
    # ending on March 31 of the next year.
    # 
    # 
    # Now binning the dates into seasons
    cap.int$smdYear = 0
    cnt = 0
    for( y in yearlist){
      for ( i in 1:nrow(seasons)){
        startdate = as.Date(paste(y,'-',seasons$start[i],sep=''))
        enddate   = as.Date(paste(y,'-',seasons$end  [i],sep=''))
        index     = cap.int$ranker.date>=startdate & cap.int$ranker.date<=enddate
        cap.int$seasons[index] = seasons$season[i]
        cap.int$smdYear[index] = as.integer(y) + seasons$offset[i] - 2010
        
      }
    }
    cap.int$smdYear = as.factor( cap.int$smdYear )

    # to find out which capture occasion are in which season. 

    # Step 8: Get durration values. 
    #         This section figures out the length of time
    #         between the first capture occation and each 
    #         subsequent occation. Seems like the hard way to do 
    #         this. 
    #

    # Make an array of NA's as long as cap.int (the number of capture occasions)
    occasion=rep(NA,nrow(cap.int))
    # Set the first element to 1.
    occasion[1]=1
    # Cycle over all but the last element
    # Set the value of each element of occation 
    # equal to the previous value plus monthlyinterval
    for(i in 1:(nrow(cap.int)-1)){
        occasion[i+1]=(occasion[i]+cap.int$monthlyinterval[i+1])
    }

    # Push the delta time values into cap.int
    cap.int$occasion=occasion

    # Remake the year column to place them in the range 2000.
    # But why do this, these values aren't real years and
    # we already have real years? 
    #
    # cap.int$year isn't used, so this doesn't make
    # a difference. 
    cap.int$year=format(cap.int$ranker.date,"20%y")

}

################################################################################
################################################################################
# SMD: In this section we are modifying the data design layer by adding
# SMD: season to Phi, p, and f
# SMD: 
################################################################################
################################################################################

    # Assign season to the different ranges of dates
    cat('Adding Season to DDL\n')
    Breeding_occasion    = cap.int$occasion[cap.int$seasons=="Breeding"]
    NonBreeding_occasion = cap.int$occasion[cap.int$seasons=="NonBreeding"]

    #add season effect to Phi
    NOCA.ddl$Phi$season = 'Breeding'
    NOCA.ddl$Phi$season[ NOCA.ddl$Phi$time%in%(NonBreeding_occasion) ] = 'NonBreeding'

    #add season effect to p
    NOCA.ddl$p$season='Breeding'
    NOCA.ddl$p$season[ NOCA.ddl$p$time%in%(NonBreeding_occasion) ]='NonBreeding'

    #add season effect to f
    NOCA.ddl$f$season='Breeding'
    NOCA.ddl$f$season[ NOCA.ddl$f$time%in%(NonBreeding_occasion) ]='NonBreeding'

    NOCA.ddl$f$season   = factor(NOCA.ddl$f$season)
    NOCA.ddl$p$season   = factor(NOCA.ddl$p$season)
    NOCA.ddl$Phi$season = factor(NOCA.ddl$Phi$season)
    #with(NOCA.ddl$S, table(season, time))  

################################################################################
################################################################################
# SMD: In this section we are adding year to the DDL.
# SMD: 
################################################################################
################################################################################

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

#
#
#
#
    cat('We cannot add ageNow to DDL. This section is toggled for more exploration with Scott (07April2019)\n')
    if ( F ) {
        NOCA.ddl$f$ageNow = NOCA.ddl$f$IntAge
        NOCA.ddl$Phi$ageNow = NOCA.ddl$Phi$IntAge
        NOCA.ddl$p$ageNow = NOCA.ddl$p$IntAge
        #attach(NOCA.ddl$Phi)
        for (i in 1:nrow(NOCA.ddl$Phi)) {
            if (NOCA.ddl$Phi$ageNow[i] == "Y") {
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

            } else if (NOCA.ddl$Phi$ageNow[i] == "A") {
            }
        }
        #detach(NOCA.ddl$Phi)
        #attach(NOCA.ddl$p)
        for (i in 1:nrow(NOCA.ddl$p)) {
            if (NOCA.ddl$p$ageNow[i] == "Y") {
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

            } else if (NOCA.ddl$p$ageNow[i] == "A") {
            }
        }

        for (i in 1:nrow(NOCA.ddl$f)) {
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

            } else if (NOCA.ddl$f$ageNow[i] == "A"){
            }
        }

        NOCA.ddl$f$ageNow = as.factor(NOCA.ddl$f$ageNow)
        NOCA.ddl$Phi$ageNow = as.factor(NOCA.ddl$Phi$ageNow)
        NOCA.ddl$p$ageNow = as.factor(NOCA.ddl$p$ageNow)
    }
#
#
#
#




################################################################################
################################################################################
# SMD: Save the modified DDL
################################################################################
################################################################################

    #detach(NOCA.ddl$p)
    #load('NOCA.ddl_21_Feb_2019_19_40')
    #NOCA.ddl$Phi$age.now <- 0
    #sendmail("<erictobinull@gmail.com>","<erictobinull@gmail.com>","DDL Generation Complete","DDL has been created with initial age class, sex, and malaria as grouping variables.",control=list(smtpServer="ASPMX.L.GOOGLE.COM")) 

    cat('Saving Modified DDL\n')
    #sendmail("<erictobinull@gmail.com>","<erictobinull@gmail.com>","MSM Model Begins Run","MSM Model has begun running. Check notes for 26Feb2019 on model specs. This is NOCA_MSM_Source_Trial2_LoadDDL.R",control=list(smtpServer="ASPMX.L.GOOGLE.COM")) 
    timestart = format(Sys.time(), "%d_%b_%Y_%H_%M")
    timestart = as.character(timestart)
    NameForFile =paste0('NOCA.ddl_modified_',timestart)
    NameForFile2 =paste0('NOCA.proc_modified_',timestart)

    save(NOCA.ddl, file= NameForFile)
    save(NOCA.proc, file = NameForFile2)

################################################################################
################################################################################
# SMD:  Build the models and run Mark
################################################################################
################################################################################
    initial.analysis=function() 
    {
      
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
    # I don't know that this works for this formulation. I'm having a problem accessing individual models
    # I may have to do this a different way. See below
    # I think we have to generate a list of the model names, every iteration of Phi.var_p.var_f.var, then give to collect.models.
    initial.analysis()
    NOCA.results = collect.models()

################################################################################
################################################################################
# SMD: Back up the final results. 
# SMD:
################################################################################
################################################################################

    timestart = format(Sys.time(), "%d_%b_%Y_%H_%M")
    timestart = as.character(timestart)
    NameForFile3 =paste0('NOCA.results_PradelTrial_',timestart)
    cat('Results saved in', NameForFile3, '\n')
    save(NOCA.results,file=NameForFile3)

    #sendmail("<erictobinull@gmail.com>","<erictobinull@gmail.com>","MSM Complete","MSM Model run with 2 CPUs, psi.logit link, and .ddl loading is complete.",control=list(smtpServer="ASPMX.L.GOOGLE.COM"))
