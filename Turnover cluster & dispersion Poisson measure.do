/***********************************************************************
* Turnover Dispersion Set-up .Do file
Coder: Rak
Updated: 2023-10-19
This file calculates turnover dispersion within each time unit (1 month) based on the original measure from Call, Nyberg, Ployhart, & Weekley (2015) AMJ
Required materials:
-	Turnover date, as fine grain as possible. In my case, I have the actual day of turnover and not just a monthly summary.
-	Employee and/or unit ID
-	Stata packages:
		search filelist			// for getting files in a directory
		search nbvargr			// for getting Poisson distributions
***********************************************************************/
	
*==============================*
* Load and clean turnover data *
*==============================*
	set more off
	cd "your directory"
	use "Your Turnover Data.dta", clear			// The turnover data should have turnover dates. Each turnover event is 1 row.

*** Gen turnover dates
	generate turnoverdate = date(turnover_date_variable, "MDY")
	format %td turnoverdate
	gen turnday = day(turnoverdate)

*** Number of turnover events per DAY
	bysort turnoverdate: egen N_turnover = count(ID_variable)

*** Collapse to collective level
	collapse N_turnover, by(turnday time_variable)
	replace N_turnover = 0 if N_turnover == .
	label var N_turnover "Sum of worker turnover per day"
	egen maxTO = max(N_turnover)
	local maxTO = maxTO							// Identify what is the maximum number of observed turnover
	rename turnday day
	sort time day

*** Create temporary file for merging
	tempfile TOcount
	save `TOcount'

*=========================================*
* Set-up daily turnover for all divisions *
*=========================================*
*** Input calendar of all days in the observation period
	use "Turnover Dispersion Long.dta", clear
	/*	This is a long-format data with one row per every day across the entire observation period.
		It also has time indicator ranging from 1-N where N is the maximum observed time period in your dataset.
		For example, if your data has 20 months, then time = 1-20.
		Since I'm calculating turnover dispersion within each month, the data also has a day of the month indicator "day" ranging from 1-31.
		and we merge "TOcount" onto it to find out how many turnover incidences occur on each day.
		Alternatively, if you are calculating turnover dispersion for a weekly time period, then you would have a 'day of the week' indicator from 1-7.
		Those from the master data that didn't match will be set = 0 because no turnover data exists on that day.	*/
	sort time day
	merge 1:1 time day using `TOcount', gen(TO_count_merge)
	replace N_turnover = 0 if TO_count_merge == 1

*** For each time period (1 month):
	bysort time: egen sum_TO = sum(N_turnover)				// 1. Total turnover events observed within that time period
	bysort time: egen sum_day = count(day)					// 2. # of days in that month
	gen daily_TO_rate = sum_TO/sum_day						// 3. expected daily turnover within each month (i.e. the rate parameter)
*** Trim the number of decimals to make this easier to work with.
	tostring daily_TO_rate, gen(group) format(%6.0g) force
	destring group, gen(lambda)
	levelsof lambda, local(lambdas) 						// 4. store daily turnover rates (lambda) for local macro use (for finding Poisson values)

*** Save the master dataset as a temp file for merging onto later
	tempfile dailyTO
	save `dailyTO'

*=======================================================================*
* Set-up Poisson probabilities for a given expected daily turnover rate *
*=======================================================================*
*** Find unique values of daily turnover rates (i.e. "Making foreach go through all values of a variable", where "local lambdas" here is each unique daily_TO_rate).
*** Empty the Poisson Prob. folder before re-running the next 4 lines since the saving command has no [replace] option
	cd "poisson probability folder"		// You should have a dedicated folder for storing all the Poisson probability values
	ls									// List all files in this folder
	!del *pprob*.dta					// delete existing files [PERMANENTLY!!! Not recycle bin!]; if this is your first time running, then this does nothing since there are no pre-existing Poisson probabilities.

/*	For each 'Turnover rate group', fit poisson distribution and get expected rates for different turnover sum, then save a .dta file. (need [nbvargr] package)
	For n(#), use the maximum observed daily turnover sum. We need the Poisson probability of observing this many turnover events in one day.
	run the command "tab N_turnover" and you'll see what I mean.	*/
		foreach l of local lambdas {
		pprob, mean(`l') n(`maxTO') saving("pprob_`l'.dta") if daily_TO_rate == `l'	
	}

*** Create identifier within each of the pprob dataset for merging back to master data (need filelist package)
	filelist , pattern(*.dta)					// Check to see if the pprob files are there
	split filename
	rename filename1 groupmerge
	
*** Clean the group name
	replace groupmerge = subinstr(groupmerge, "pprob_", "",.) 
	replace groupmerge = subinstr(groupmerge, ".dta", "",.) 
	
*** Set up tempfile for Poisson probability file identifier
	tempfile myfiles
	save "`myfiles'"
	
*** Loop over the list of files in the specified directory, create the "groupmerge" variable and save a temporary copy of each dataset
	local obs = _N
	forvalues i=1/`obs' {
	   use "`myfiles'" in `i', clear
	   local f = dirname + "/" + filename
	   local fgroup = groupmerge
	   use "`f'", clear
	   gen group = "`fgroup'"
	   tempfile save`i'
	   save "`save`i''"
	 }
*** combine all the temp files
	clear
	forvalues i=1/`obs' {
	   append using "`save`i''"
	}
	drop pcum
/*	The result should contain X number of rows, where X = [# of unique daily turnover rates (i.e., 'lambda')] * [# of unique observed turnover sums (i.e., 'k')].
	For example, if the daily turnover rates in your data are all whole numbers ranging from 0% to 10%, then the # of unique daily rates should be 11.
	Likewise, if all the days in your data showed turnover ranging from 0 to 10, then the # of unique observed turnover sums = 11.
	Thus, there should be 11 'lambda' * 11 'k' = 110 rows. */

*** Save all of these Poisson probability values as "pprob Allrates.dta"
	cd "your data directory"
	save "pprob Allrates.dta", replace

*** Restore the master dataset
	use `dailyTO'

*** Generate vars counting days with 0-N turnover events, where N is the maximum # of turnover events observed at the collective level for any day in our data.
	foreach num of numlist 0/`maxTO' {
	egen k_`num'= anycount(N_turnover), value(`num')
	}

*** Collapse into collective level
	collapse (mean)sum_TO sum_day daily_TO_rate lambda (sum) k_0-k_`maxTO', by(time)

*** Reshape into long data
	reshape long k_ , i(time) j(k) 		// We get total sample size = # months * # 'k', where k is the observed turnover count from 0-N)
	order k, before(k_)
	rename k_ observed
	tostring lambda, replace force
	rename lambda group

*** Merge in expected rate from Poisson distribution
	cd "poisson probability folder"
	merge m:1 group k using "pprob Allrates.dta", gen(merge_pprob)
	label var k "daily observed turnover"
	sort time k

*** Calculate Chi-Sq Goodness of Fit
	rename pprob expect_rate
	gen expected = sum_day*expect_rate
	gen diff = observed-expected
	gen diff2 = diff^2
	gen goodfit = diff2/expected
	bysort time: egen TOdisp = sum(goodfit)
	replace TOdisp = asinh(TOdisp)			// Use the inverse hyperbolic sine transformation to handle extreme values (Burbidge et al., 1988, J. Am. Stat. Assoc.).
	
*** Final collapse to the collective level
	collapse (mean) TOdisp, by(time)
	save "Turnover Dispersion.dta", replace
	
	********
	* DONE *
	********
