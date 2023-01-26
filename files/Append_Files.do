cap pr drop Append_Files
pr def Append_Files, rclass
	syntax, dir(string) file(string) vars(string) [nomessage filenamevar(string)]
	
	*** PREP DATA FRAMES ***
	tempname FRAME_ORIG FRAME_TEMP
	qui pwf
	loc `FRAME_ORIG' = "`r(currentframe)'"
	
	qui frame create `FRAME_TEMP'
	cwf `FRAME_TEMP'
	
	*** OPEN FILE & KEEP NECESSARY VARS ***
	set maxvar 120000
	qui use "`dir'`file'", clear
	
	tempname VARLIST VARLIST_NOCOPY
	loc `VARLIST' = ""
	loc `VARLIST_NOCOPY' = ""
	
	foreach v in `vars' {
		cap ds `v'
		if _rc==0 {
			loc `VARLIST' = "``VARLIST'' `r(varlist)'"
		}
		else {
			loc `VARLIST_NOCOPY' = "``VARLIST_NOCOPY'' `v'"
		}
	}
	if "`filenamevar'"!="" {
		qui g `filenamevar' = "`file'"
		loc `VARLIST' = "``VARLIST'' `filenamevar'"
	}
	
	qui keep ``VARLIST''
	
	*** APPEND OBS TO ORIG FRAME ***
	cwf ``FRAME_ORIG''
	
	* CALC SAMPLE SIZE *
	tempname N_ORIG N_NEW N_APPEND
	qui cou
	loc `N_ORIG' = r(N)
	frame `FRAME_TEMP': qui cou
	loc `N_NEW' = r(N)
	loc `N_APPEND' = ``N_ORIG''+``N_NEW''
	
	* LINK TEMP OBS AT THE END OF ORIG *
	qui set obs ``N_APPEND''
	tempvar ID_TEMP LINK_TEMP
	qui g `ID_TEMP' = _n
	frame `FRAME_TEMP': qui g `ID_TEMP' = ``N_ORIG'' + _n
	qui frlink 1:1 `ID_TEMP', frame(`FRAME_TEMP') gen(`LINK_TEMP')
	
	* COPY VARS *
	tempvar v_temp
	
	foreach v of local `VARLIST' {
		* VARLIST is the list of vars in frame FRAME_TEMP
		* Check whether v in VARLIST exists in FRAME_ORIG
		* If it does, copy v as v_temp and get only values
		* If not, create v in FRAME_ORIG
		cap conf v `v', exact
		if _rc==0 {
			qui frget `v_temp' = `v', from(`LINK_TEMP')
			qui replace `v' = `v_temp' if `ID_TEMP'>``N_ORIG''
			drop `v_temp'
		}
		else {
			qui frget `v', from(`LINK_TEMP')
		}
	}
	
	* CLEAN UP *
	qui drop `LINK_TEMP' `ID_TEMP'
	frame drop `FRAME_TEMP'
	
	if "`message'"=="" | "`message'"=="message" {
		di "Original data (``N_ORIG'' records) + new data [`file'] (``N_NEW'' records)"
		di "Variables specified but not copied [``VARLIST_NOCOPY'']"
		di ""
	}
end
