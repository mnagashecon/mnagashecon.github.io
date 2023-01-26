cap pr drop Append_Frames
pr def Append_Frames, rclass
	syntax, master(string) using(string) [dropusing nomessage]
	
	*** SETUP DATA IN MASTER FRAME ***
	cwf `master'
	tempname N_MASTER
	
	qui cou
	loc `N_MASTER' = r(N)
	
	*** SETUP DATA IN USING FRAME ***
	tempname VARS_USING N_USING
	frame `using': qui ds
	loc `VARS_USING' = "`r(varlist)'"
	
	frame `using': qui cou
	loc `N_USING' = r(N)
	
	*** LINK TEMP OBS AT THE END OF ORIG ***
	tempname N_TOTAL
	loc `N_TOTAL' = ``N_MASTER''+``N_USING''
	qui set obs ``N_TOTAL''
	
	tempvar ID LINK
	qui g `ID' = _n
	frame `using': qui g `ID' = ``N_MASTER'' + _n
	qui frlink 1:1 `ID', frame(`using') gen(`LINK')
	
	*** COPY VARS ***
	tempvar v_temp
	
	foreach v of local `VARS_USING' {
		* VARS_USING is the list of vars in using frame
		* Check whether v in VARS_USING exists in master frame
		* If it does, copy v as v_temp and get only values
		* If not, create v in master frame
		cap conf v `v', exact
		if _rc==0 {
			qui frget `v_temp' = `v', from(`LINK')
			qui replace `v' = `v_temp' if `ID'>``N_MASTER''
			drop `v_temp'
		}
		else {
			qui frget `v', from(`LINK')
		}
	}
	
	*** CLEAN UP ***
	qui drop `LINK' `ID'
	if "`dropusing'"!="" {
		qui frame drop `using'
	}
	
	if "`message'"=="" | "`message'"=="message" {
		di "Master data (``N_MASTER'' records) + using data (``N_USING'' records)"
		di ""
	}
end
