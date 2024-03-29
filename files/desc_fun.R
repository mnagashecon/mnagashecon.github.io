# Author: Masaru Nagashima
# Affiliation: Waseda Institute for Advanced Study
# Last Update: 27 Feb 2022
# No copyright claimed. 
# 
# Example Usage: 
#> names(tmp) # this is the temporary data; suppose you have four variables
#[1] "wave"    "age"     "sex"     "marstat"
#> ctgry = c("marstat") # specify category variables
#> bnry = c("sex") # specify binary variables
#> stats = cbind(
#+ 	getsumstats(dplyr::filter(tmp, wave==1) %>% dplyr::select(-wave), ctgry=ctgry, bnry=bnry), 
#+ 	getsumstats(dplyr::filter(tmp, wave==2) %>% dplyr::select(-wave), ctgry=ctgry, bnry=bnry), 
#+ 	getsumstats(dplyr::filter(tmp, wave==3) %>% dplyr::select(-wave), ctgry=ctgry, bnry=bnry), 
#+ 	getsumstats(dplyr::filter(tmp, wave==4) %>% dplyr::select(-wave), ctgry=ctgry, bnry=bnry))
#> fmt = list( # specify LaTeX output formats
#+ 	list(c=c(1,4,7,10), f="%d"), # N
#+ 	list(r=1, c=c(2,3,5,6,8,9,11,12), f="%2.2f") # age
#+ 	)
#> header = c( # specify LaTeX output header
#+ 	"\\begin{tabular}{lccc@{\\extracolsep{4pt}}@{}ccc@{\\extracolsep{4pt}}@{}ccc@{\\extracolsep{4pt}}@{}ccc}",
#+ 	"\\hline",
#+ 	"& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) & (11) & (12) \\\\",
#+ 	"& \\multicolumn{3}{c}{Wave I} & \\multicolumn{3}{c}{Wave II} & \\multicolumn{3}{c}{Wave III} & \\multicolumn{3}{c}{Wave IV} \\\\",
#+ 	"\\cline{2-4} \\cline{5-7} \\cline{8-10} \\cline{11-13}",
#+ 	"& N & Mean & sd & N & Mean & sd & N & Mean & sd & N & Mean & sd \\\\",
#+ 	"\\hline")
#> footer = c( # specify LaTeX output footer
#+ 	"\\hline",
#+ 	"\\end{tabular}")
#> lines = writestatstotex( # write `stats` to file
#+ 	stats, 
#+ 	file="tmp_stats.tex", 
#+ 	fmt=fmt, 
#+ 	header=header, 
#+ 	footer=footer, 
#+ 	append=FALSE)
#> 

getsumstats = function(df, ctgry=NULL, bnry=NULL) {
	# inputs: 
	#   df = list of variables
	#   ctgry = c(string names of category variables)
	#   bnry = c(string names of binary variables)
	# output:
	#   matrix of n, mean, and sd (cols) for variables in df (rows)
	#   - rownames are the variable labels
	#   - if the variable is in ctgry, "variablelabel: valuelabel"
	#   - if the variable is in bnry, "variablelabel: firstvaluelabel"
	#   - if the variable is in both ctgry and bnry, the rule for ctgry applies
	nms_df = names(df)
	getn = function(x) sum(!is.na(unlist(x)))
	getmean = function(x) mean(unlist(x), na.rm=TRUE)
	getsd = function(x) sd(unlist(x), na.rm=TRUE)
	ns = c(); means = c(); sds = c(); rnms = c();
	for (i in 1:length(nms_df)) {
		if (nms_df[i] %in% ctgry) {
			lbl = attr(df[[i]], "label")
			lbls = attr(df[[i]], "labels")
			txts = names(lbls)
			n = c(); mean = c(); sd = c(); rnm = c();
			for (j in 1:length(lbls)) {
				lbls_j = lbls[j]
				txts_j = txts[j]
				rnm_j = paste0(
					ifelse(!is.null(lbl),lbl,nms_df[i]), 
					": ", 
					ifelse(!is.null(txts_j),txts_j,as.character(lbls_j)))
				vec_j = ifelse(df[[i]]==lbls_j,1,ifelse(!is.na(df[[i]]),0,NA))
				n_j = getn(vec_j)
				mean_j = getmean(vec_j)
				sd_j = getsd(vec_j)
				n = c(n, n_j)
				mean = c(mean, mean_j)
				sd = c(sd, sd_j)
				rnm = c(rnm, rnm_j)
			}
		} else if (nms_df[i] %in% bnry) {
			lbli = attr(df[[i]], "label")
			lbls = attr(df[[i]], "labels")
			lbls1 = lbls[1]
			txts1 = names(lbls)[1]
			lbl = paste0(
				ifelse(!is.null(lbli),lbli,nms_df[i]), 
				": ", 
				ifelse(!is.null(txts1),txts1,as.character(lbls1)))
			vec = ifelse(df[[i]]==lbls1,1,ifelse(!is.na(df[[i]]),0,NA))
			n = getn(vec)
			mean = getmean(vec)
			sd = getsd(vec)
			rnm = lbl
		} else {
			n = getn(df[[i]])
			mean = getmean(df[[i]])
			sd = getsd(df[[i]])
			lbl = attr(df[[i]], "label")
			rnm = ifelse(!is.null(lbl),lbl,nms_df[i])
		}
		ns = c(ns, n)
		means = c(means, mean)
		sds = c(sds, sd)
		rnms = c(rnms, rnm)
	}
	stats = cbind(ns, means, sds)
	colnames(stats) = c("n","mean","sd")
	rownames(stats) = rnms
	return(stats)
}

writestatstotex = function(stats, file, fmt=NULL, header=NULL, footer=NULL, append=FALSE) {
	# inputs:
	#   stats = stats matrix (row = vars, col = stat)
	#   file = output .tex
	#   fmt = list of lists (r=..., c=..., f=string sprintf format)
	#   - r and c are c(integers) indicating the rows and columns to apply format
	#   - if is.null(r) & is.null(c), applies to all values
	#   - if is.null(r), applies to all values in column==c
	#   - if is.null(c), applies to all values in row==r
	#   - fmt elements that are coded later overwrites previous elements
	#   header = c(lines to write before the table). 1 element = 1 line
	#   footer = c(lines to write after the table). 1 element = 1 line
	#   append = TRUE/FALSE to be passed through to write()
	# output:
	#   lines = LaTeX code constituting the table that the user attempts to write
	rnms = rownames(stats)
	lines = c()
	if (!is.null(header)) {
		lines = c(lines, header)
	}
	for (i in 1:nrow(stats)) {
		line = rnms[i]
		for (j in 1:ncol(stats)) {
			f_k = "%1.3f"
			for (k in 1:length(fmt)) {
				fmt_k = fmt[[k]]
				r_k = fmt_k$r
				c_k = fmt_k$c
				if ((is.null(r_k) | i%in%r_k) & (is.null(c_k) | j%in%c_k) & !is.null(fmt_k$f)) {
					f_k = fmt_k$f
				}
			}
			line = sprintf("%s & %s", line, sprintf(f_k, stats[i,j]))
		}
		line = paste0(line, " \\\\")
		lines = c(lines, line)
	}
	if (!is.null(footer)) {
		lines = c(lines, footer)
	}
	write(lines, file=file, append=append)
	return(lines)
}
