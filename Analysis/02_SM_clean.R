#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used to CLEAN data needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#==============================================================================================#
#CLEANING UP surv3d data

	#resave type so that types appear in the following order
	surv3d$type <- factor(surv3d$type, levels = c("T4x", "T2x", "W4x", "V2x", "V4x"))

	#create a new date variable that is the same as time of death -- will recode month to a date below
	surv3d$date <- surv3d$timedeath

	#subset data by garden	
	sdat_O <- surv3d[which(surv3d$garden == "Orchard"),]
	sdat_M <- surv3d[which(surv3d$garden == "Majors"),]
	sdat_E <- surv3d[which(surv3d$garden == "Ephraim"),]
	
	#save unique sample month number dates as calendar dates
			#ORCHARD
				sdat_O$date[sdat_O$date == 60] <- "5/20/15"		
				sdat_O$date[sdat_O$date == 53] <- "10/31/14"
				sdat_O$date[sdat_O$date == 47] <- "4/25/14"
				sdat_O$date[sdat_O$date == 42] <- "11/12/13"
				sdat_O$date[sdat_O$date == 38] <- "7/18/13"
				sdat_O$date[sdat_O$date == 36] <- "5/2/13"
				sdat_O$date[sdat_O$date == 27] <- "8/8/12"
				sdat_O$date[sdat_O$date == 25] <- "6/26/12"
				sdat_O$date[sdat_O$date == 23] <- "4/6/12"
				sdat_O$date[sdat_O$date == 17] <- "10/19/11"
				sdat_O$date[sdat_O$date == 15] <- "8/23/11"
				sdat_O$date[sdat_O$date == 12] <- "5/18/11"
				sdat_O$date[sdat_O$date == 11] <- "4/1/11"
				sdat_O$date[sdat_O$date == 6] <- "11/1/10"
			
			sdat_O$date <- as.Date(sdat_O$date, "%m/%d/%y")
		
		#Majors
				sdat_M$date[sdat_M$date == 4] <- "10/18/10"
				sdat_M$date[sdat_M$date == 11] <- "5/17/11"
				sdat_M$date[sdat_M$date == 16] <- "10/17/11"
				sdat_M$date[sdat_M$date == 22] <- "5/10/12"
				sdat_M$date[sdat_M$date == 23] <- "6/12/12"
				sdat_M$date[sdat_M$date == 25] <- "7/23/12"
				sdat_M$date[sdat_M$date == 26] <- "8/14/12"
				sdat_M$date[sdat_M$date == 28] <- "10/17/12"
				sdat_M$date[sdat_M$date == 35] <- "5/28/13"				
				sdat_M$date[sdat_M$date == 38] <- "8/22/13"
				sdat_M$date[sdat_M$date == 46] <- "4/29/14"
				sdat_M$date[sdat_M$date == 57] <- "4/7/15"
		
			sdat_M$date <- as.Date(sdat_M$date, "%m/%d/%y")
		
		
		#Ephraim
				sdat_E$date[sdat_E$date == 5] <- "10/21/10"		
				sdat_E$date[sdat_E$date == 11] <- "4/14/11"
				sdat_E$date[sdat_E$date == 12] <- "5/26/11"
				sdat_E$date[sdat_E$date == 13] <- "6/23/11"
				sdat_E$date[sdat_E$date == 17] <- "10/17/11"
				sdat_E$date[sdat_E$date == 23] <- "4/24/12"
				sdat_E$date[sdat_E$date == 25] <- "6/12/12"
				sdat_E$date[sdat_E$date == 26] <- "7/23/12"
				sdat_E$date[sdat_E$date == 27] <- "8/14/12"
				sdat_E$date[sdat_E$date == 29] <- "10/17/12"
				sdat_E$date[sdat_E$date == 35] <- "4/23/13"
				sdat_E$date[sdat_E$date == 37] <- "6/13/13"
				sdat_E$date[sdat_E$date == 41] <- "10/18/13"
				sdat_E$date[sdat_E$date == 47] <- "4/8/14"
				sdat_E$date[sdat_E$date == 49] <- "6/23/14"
				sdat_E$date[sdat_E$date == 51] <- "8/27/14"
				sdat_E$date[sdat_E$date == 59] <- "4/6/15"
		
			sdat_E$date <- as.Date(sdat_E$date, "%m/%d/%y")
		
		#use row bind to combine the 3 garden datasets
		surv3dd <- rbind(sdat_E, sdat_M, sdat_O)
		
	#aggregate population level data with counts of survival and dead
		surv3counts <- surv3dd %>% group_by(pop, type, garden) %>% 
		                       summarise(death = sum(death), total = n()) %>% 
		                       mutate(surv = total - death, propdead = death / total)
		
#==============================================================================================#


#==============================================================================================#		
#CLEANING UP climate data
	
	#tell R to treat the date in a format it can understand month day year to year month day
	#transforms 1/1/10 to 2010-01-01
	climate$Date <- as.Date(climate$Date, "%m/%d/%y")
#==============================================================================================#


#==============================================================================================#
#CLEANING UP Provenance climate data

	#add derived climate variables
	prov_clim <- with(prov_clim, data.frame(
	     pop,
	     #long,
	     #lat,
	     #elev,
	     adi = (sqrt(dd5))/map,
	     adimindd0 = (sqrt(dd5))/map * mmindd0,
	     d100 = d100,
	     dd0 = dd0,
	     dd0gsp = dd0 / gsp,
	     dd0map = dd0 / map,
	     dd5 = dd5,
	     dd5mtcm = dd5 * mtcm,
	     fday = fday,
	     ffp = ffp,
	     gsdd5 = gsdd5,
	     gsp = gsp,
	     gspdd5 = (gsp * dd5)/1000,
	     gspmtcm = (gsp * mtcm)/1000,
	     gsptd  = (gsp * (mtwm - mtcm))/100,
	     map = map,
	     mapdd5 = (map*dd5)/1000,
	     mapmtcm = (map*mtcm)/1000,
	     maptd  = (map*(mtwm - mtcm))/100,
	     mat = mat,
	     mmax = mmax,
	     mmin = mmin,
	     mmindd0 = mmindd0,
	     mtcm = mtcm,
	     mtcmgsp = mtcm/gsp,
	     mtcmmap = mtcm/map,
	     mtwm = mtwm,
	     pratio = gsp/map,
	     sday = sday,
	     sdi = (gsdd5 ** 0.5)/gsp,
	     sdimindd0 = ((gsdd5 ** 0.5)/gsp) * mmindd0,
	     sdimtcm = (sqrt(gsdd5)/gsp) * mtcm,
	     smrp = smrp,
	     smrpb = smrpb,
	     #smrsprpb = smrsprb, (depreciated variable)
	     sprp = sprp,
	     tdgsp  = (mtwm - mtcm)/gsp,
	     tdiff = mtwm - mtcm,
	     tdmap  = (mtwm - mtcm)/map,
	     winp = winp))
		
		#merge prov. climate data with survival data
		surv3clim <- merge(surv3counts, prov_clim, by = "pop")

#==============================================================================================#