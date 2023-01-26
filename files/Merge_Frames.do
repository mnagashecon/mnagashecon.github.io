cap pr drop Merge_Frames
pr Merge_Frames, rclass
	syntax, fx(string) fy(string) id(string) vy(string) vmerge(string)
	* fx = name of frame one
	* fy = name of frame two
	* vy = names of varables to merge from fy to fx
	* id = names of id variables to link two frames
	* vmerge = name of variable indicating observation source (1 = both, 2 = fx, 3 = fy)
	
	tempname N_FY NLIST_FY N_FX N_FXFY FY_VAL A
	tempvar n
	
	* check obs only in fy
	frame change `fy'
	qui frlink 1:1 `id', frame(`fx')
	qui count if `fx'==.
	loc `N_FY' = r(N)
	qui g `n' = _n
	qui levelsof `n' if `fx'==.
	loc `NLIST_FY' = "`r(levels)'"
	drop `n' `fx'
	
	* link fy for obs in both fx and fy
	frame change `fx'
	qui frlink 1:1 `id', frame(`fy')
	qui g `vmerge' = `fy'!=.
	qui replace `vmerge'=2 if `vmerge'==0
	
	* forcibly link obs only in fy
	qui count
	loc `N_FX' = r(N)
	loc `N_FXFY' = ``N_FX'' + ``N_FY''
	qui set obs ``N_FXFY''
	forv i = 1/``N_FY'' {
		loc `FY_VAL': word `i' of ``NLIST_FY''
		qui replace `fy' = ``FY_VAL'' if _n==`i'+``N_FX''
		foreach v in `id' {
			frame `fy': loc `A' = `v'[``FY_VAL'']
			qui replace `v' = ``A'' if _n==`i'+``N_FX''
		}
	}
	qui replace `vmerge'=3 if `vmerge'==.
	
	* frget with manipulated linkvar
	frget `vy', from(`fy')
	drop `fy'
	
	lab def `vmerge' 1 "Both in `fx' and `fy'" 2 "Only in `fx'" 3 "Only in `fy'"
	lab val `vmerge' `vmerge'
end
