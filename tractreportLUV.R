# Script for generating	R report on tract level
# Hana Sevcikova, PSRC
# July, 2015
#

##### BEGIN USER SETTINGS ######
runs <- c(142, 138) # runs to include
ci.run <- c() # which run has confidence intervals 
			     # (only the last one is included in the table)

# Directory containing the runs with indicators. 
# The indicators directory must contain files of the type 'faz__tab__xxx.tab'
#sim.dir <- '/Users/hana/workspace/data/psrc_parcel/runs' 
sim.dir <- file.path(getwd(), 'runs') 
sim.prefix <- 'run_' # name of the run sub-directory without the run number, e.g. 'run_' for directory run_199

show.all.years <- TRUE # should results from all years be shown as dotted lines
not.all.years <- c() # if show.all.years is TRUE, put here runs that are exceptions

geography <- 'tractcity'
geo.id <- paste0(geography, '_id')

output.file.name <- paste(geography, 'reportLUV1_', if(show.all.years) 'allyears_' else '', paste(runs, collapse='_'), sep='')

years <- c(2000, 2010, 2020, 2025, 2030, 2035, 2040) # for plots
years <- c(2000, seq(2010, 2040, by=5))
years.for.table <- seq(2000, 2040, by=10)
all.years <- if(show.all.years) 2010:2040 else c()

save.data.as.ascii <- FALSE
###### END USER SETTINGS ############


library(ggplot2)
library(grid)
library(gridExtra)

lyears <- length(years)
indicators <- c('households',  'population', 'jobs')
ci.names <- list(jobs='job', households='household', population='population')
titles <- list(households='Households', jobs='Employment', population='HH Population')

output.file.name.pdf <- paste(output.file.name,  'pdf', sep='.')
output.file.name.txt <- paste(output.file.name,  'txt', sep='.')

wrkdir <- getwd()
comments <- read.table(file.path(wrkdir, 'data', paste0('commentsLUV1', geography, '.csv')), header=TRUE, sep='\t', 
						allowEscapes = TRUE, as.is=TRUE)
comment.header <- list(households="HH_Comment_40", jobs="Emp_Comment_40")

trim.leading <- function (x)  sub("^\\s+", "", x)

remove.na <- function(data)
	apply(data, c(1,2), function(x) if(trim.leading(x)=='NA') '' else x)

check.numeric <- function(N){
  !length(grep("[^[:digit:]]", as.character(N)))
}


sim <- sim.ref <- ids <- ids.ref <- CIs <- saf <- trend.data <- ids.tr <- list()

for (what in indicators) {
	# Load observed data
	trend.file.name <- file.path(wrkdir, 'data',  paste0(what, '_observed_', geography, '.txt'))
	if(file.exists(trend.file.name)) {
		trend.data.raw <- data.frame(read.table(trend.file.name, sep='\t', header=TRUE))
		ids.tr[[what]] <- trend.data.raw[,1]
		trend.data[[what]] <- trend.data.raw[order(ids.tr[[what]]), 2:ncol(trend.data.raw)]
		ids.tr[[what]] <- sort(ids.tr[[what]])
		colnames(trend.data[[what]]) <- substr(colnames(trend.data[[what]]), 2,5)
		trend.data[[what]] <- trend.data[[what]][,is.element(colnames(trend.data[[what]]), as.character(years))]
	}
	sim[[what]] <- ids[[what]] <- CIs[[what]] <- list()
	for(run in runs) {
		# Load indicators
		data <- read.table(file.path(sim.dir, paste0(sim.prefix, run), 
						'indicators', paste0(geography, '__table__', what, '.csv')), sep=',', header=TRUE)
		sim[[what]][[run]] <- data[,2:ncol(data)]
		ids[[what]][[run]] <- data[,1]
		sim[[what]][[run]] <- sim[[what]][[run]][order(ids[[what]][[run]]),]
		ids[[what]][[run]] <- sort(ids[[what]][[run]])
	}
}

geo.names <- data.frame(read.table(file.path(wrkdir, 'data', 'tractcity.csv'), sep=',', header=TRUE))
geo.names <- geo.names[,c("tractcity_id", "trctjuris")]
colnames(geo.names) <- c('id', 'name')
years.for.df <- sort(unique(c(years, all.years)))
lyears.for.df <- length(years.for.df)
tracts <- sort(ids[[indicators[1]]][[runs[1]]])
tracts.in.comments <- comments$tractcity_id
tracts.id.in.comments <- as.integer(tracts.in.comments)
is.in.comments <- tracts %in% tracts.id.in.comments
#tracts <- c(tracts[is.in.comments], tracts[!is.in.comments])
tracts <- tracts[is.in.comments]

file.append <- FALSE
cat('\nProcessing ', length(tracts), ' geographies:\n')
pdf(output.file.name.pdf, width=12, height=9)
for(geo in tracts) {
	cat(geo, ', ')
	flush.console()
	comment.idx <- which(tracts.id.in.comments == geo)
	has.comment <- length(comment.idx) > 0
  	g <- gtab <- list()
	for (what in indicators) {
		geo_not_found <- FALSE
		tabDF <- data.frame(Time=years.for.df)
		last.table.columns <- NA	
		run.table.columns <- ref.table.columns <-c()
		for(irun in 1:length(runs)) {
			run <- runs[irun]
			idx <- which(ids[[what]][[run]]==geo)
			if (length(idx) <=0) {geo_not_found <- TRUE; break}
			coln <- colnames(sim[[what]][[run]])
			run.cols <- substr(coln, nchar(coln)-3, nchar(coln))
			col.idx <- which(run.cols %in% as.character(years.for.df))
			amount <- sim[[what]][[run]][idx,col.idx]
			yidx <- which(as.character(years.for.df) %in% run.cols[col.idx])
			matched.amount <- rep(NA, lyears.for.df)
			matched.amount[yidx] <- as.numeric(amount)
			this.data <- data.frame(run=rep(run, lyears.for.df), Time=years.for.df, 
							amount=matched.amount)
			datafrs <- if(irun == 1) this.data else datafrs <- rbind(datafrs, this.data)
			this.tabdata <- this.data[,'amount', drop=FALSE]
			colnames(this.tabdata) <- paste('run', run)
			run.table.columns <- c(run.table.columns, paste('run', run))
			last.table.columns <- c()
			tabDF <- cbind(tabDF, this.tabdata)
		}
		if(geo_not_found) next
		idxt <- which(ids.tr[[what]]==geo)
		obs.df <-  NULL
		if(!is.null(trend.data[[what]])) {
			obs.df <- data.frame(run=rep('Actual', ncol(trend.data[[what]])), 
						Time=as.integer(colnames(trend.data[[what]])), 
						amount=as.numeric(trend.data[[what]][idxt, ]))
		}
		datafrs <- rbind(datafrs, obs.df)
		tabDF$Actual <- tabDF$Comment <- rep(NA, nrow(tabDF))
		if(!is.null(trend.data[[what]]))
			tabDF$Actual[tabDF$Time %in% as.integer(colnames(trend.data[[what]]))] <- trend.data[[what]][idxt,]
		# Create plot of lines
		g[[what]] <- ggplot(subset(datafrs, Time %in% years), aes(Time, amount, colour=factor(run))) + geom_line() + 
							scale_y_continuous('') + scale_x_continuous('') + 
							scale_colour_discrete(name = '') +
							ggtitle(titles[[what]]) + 
							theme(legend.position=c(0,0.5), legend.justification=c(0,0), 
									legend.key=element_blank(),
									legend.key.size = unit(0.02, "npc"),
									plot.title=element_text(size=12))

		if(length(all.years) > 0)
			g[[what]] <- g[[what]] + geom_line(data=subset(datafrs, run %in% runs & !(run %in% not.all.years) & Time %in% all.years), linetype=3)
		# Add comment point
		if(has.comment && length(comment.header[[what]]) > 0) {			
			comamount <- as.character(comments[comment.idx,comment.header[[what]]])
			if (!is.na(comamount) && comamount != '') {
				comdf <- NULL
				if(check.numeric(comamount)) {
					comdf <- data.frame(Time=2040, amount=as.integer(comamount))
					comcol <- 'black'
				} else {
					if(substr(comamount, nchar(comamount), nchar(comamount))=='?' && 
							check.numeric(substr(comamount, 1, nchar(comamount)-1))) {
						comdf <- data.frame(Time=2040, amount=as.integer(substr(comamount, 1, nchar(comamount)-1)))
						comcol <- 'orange'
					}
				}
				if(!is.null(comdf))	{
					g[[what]] <- g[[what]] + geom_point(data=comdf, 
									colour=comcol, aes(Time, amount, guide=FALSE))
					tabDF$Comment[tabDF$Time %in% comdf$Time] <- as.integer(comamount)[1]	
				}
			}
		}
		# Create table
		tidx <- c(which(is.element(tabDF[,1], years.for.table)), which(tabDF[,1]==9999))
		tidx.raw <- tidx
		rown <- tabDF[tidx,1]
		last.row <- c()
		for(column in colnames(tabDF)) tabDF[tidx.raw,column] <- as.integer(tabDF[tidx.raw,column])
		lastcols <- if(length(last.table.columns)==1 && is.na(last.table.columns)) c() else last.table.columns
		columns <- c('Actual', unlist(strsplit(paste(paste(run.table.columns, collapse=','), sep=','), ',')), lastcols, 'Comment')
		format.table <- format(tabDF[tidx.raw,columns],
							justify='right', big.mark = ","
							)
		gtab[[what]] <- list()
		gtab[[what]] <- tableGrob(format.table,
						rows=rown,
    					#gpar.colfill = gpar(fill=NA,col=NA), 
    					gpar.rowfill = gpar(fill=NA,col=NA),
    					#show.box=TRUE, 
    					separator = "grey",
    					h.even.alpha = 0,
    					show.csep = TRUE, 
    					gpar.rowtext = gpar(col="black",  equal.width = TRUE, fontface='bold', # cex=0.8,
                        			show.vlines = TRUE, show.hlines = TRUE, separator="grey")                     
    	)
    	gtab[[what]]$d <- remove.na(gtab[[what]]$d)
    	tabDF <- cbind(geo, tabDF)
    	colnames(tabDF)[1] <- 'area_id'
    	if(save.data.as.ascii) {
    		write(titles[[what]], file=output.file.name.txt, append=file.append)
    		write.table(as.matrix(tabDF[tidx,c(colnames(tabDF)[1], columns)]), file=output.file.name.txt, append=TRUE, row.names=FALSE, sep='\t')
    		write('\n', file=output.file.name.txt, append=TRUE)
    		file.append <- TRUE
    	}
	}
	sub <- ''
	main <- paste(geo, '-', geo.names[geo.names[,'id']==geo,'name'])
	if(length(main) == 0) main <- as.character(geo)
	if(is.element(geo, tracts.id.in.comments)) {
		sub <- comments[which(tracts.in.comments == geo), "Comment"]
		juris <- as.character(comments[which(tracts.in.comments == geo), "Tract"])
		#main <- paste0(main, " (", substr(juris, 7, nchar(juris)), ")")
		main.col <- 'red'
	}  else main.col <- 'black'
	# Assemble page		
	grid.arrange(gtab[[indicators[1]]], g[[indicators[1]]],
				 gtab[[indicators[2]]], g[[indicators[2]]],
				 gtab[[indicators[3]]], g[[indicators[3]]],
				ncol=2, 
				main=textGrob(main,
						#paste(geo, '-', geo.names[geo.names[,'id']==geo,'name']), 
						gp=gpar(fontsize=14,fontface="bold", col=main.col, fill=main.col), 
						just=c('center', 'top')),
				heights=unit(0.33, "npc"),
				sub=textGrob(sub, gp=gpar(fontsize=11), just=c('center', 'bottom')),
				just='left')

}
cat('\n')
dev.off()