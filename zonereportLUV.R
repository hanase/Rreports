# Script for generating LUV R report on TAZ level
# Hana Sevcikova, PSRC
# April, 2015
#

##### BEGIN USER SETTINGS ######
#runs <- c(114, 113, "245mr1") # runs to include
#runs <- c(142, 138, 133, 113)
runs <- c(138,142,169,170,171)


# Directory containing the runs with indicators. 
# The indicators directory must contain files of the type 'faz__tab__xxx.tab'
#sim.dir <- '/Users/hana/psrc3656/workspace/data/psrc_parcel/runs' 
sim.dir <- file.path(getwd(), 'runs') 
sim.prefix <- 'run_' # name of the run sub-directory without the run number, e.g. 'run_' for directory run_199

show.all.years <- TRUE # should results from all years be shown as dotted lines
not.all.years <- c() # if show.all.years is TRUE, put here runs that are exceptions

geography <- 'zone'
geography.id <- paste0(geography, '_id')
output.file.name <- paste(geography, 'reportLUVcomm_', if(show.all.years) 'allyears_' else '', paste(runs, collapse='_'), sep='')

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
titles <- list(households='Households', jobs='Employment', population='HH Population')

#mpds <- read.table(file.path(wrkdir, 'data', 'mpds_in_use_by_zone.csv'), sep=',', header=TRUE)
#zones <- c(mpds[,geography.id])

output.file.name.pdf <- paste(output.file.name,  'pdf', sep='.')
output.file.name.txt <- paste(output.file.name,  'txt', sep='.')

wrkdir <- getwd()
source(file.path(wrkdir, 'data', paste0('commentsLUV',geography,'.R')), chdir=TRUE)
trim.leading <- function (x)  sub("^\\s+", "", x)

remove.na <- function(data)
	apply(data, c(1,2), function(x) if(trim.leading(x)=='NA') '' else x)



sim <- fazids <- CIs <- saf <- trend.data <- lut.data <- fazids.tr <- fazids.lut <- fazids.saf <- list()

#id.correspondence <- data.frame(read.table(file.path(wrkdir, 'data', 'fazes.txt'), sep='\t', header=TRUE))
#id.correspondence <- id.correspondence[order(id.correspondence[,'faz_id']),]

zone.faz <- read.table(file.path(wrkdir, 'data', "zones.csv"), sep=',', header=TRUE)
faz_names <- read.table(file.path(wrkdir, 'data', 'faz_names.txt'), header=TRUE, sep='\t')
#cities <- read.table(file.path(wrkdir, 'data', "citiesLUV.csv"), sep=',', header=TRUE)

for (what in indicators) {
	# Load observed data
	trend.file.name <- file.path(wrkdir, 'data',  paste(what, '_observed_', geography, '.txt', sep=''))
	if(file.exists(trend.file.name)) {
		trend.data.raw <- data.frame(read.table(trend.file.name, sep='\t', header=TRUE))
		fazids.tr[[what]] <- trend.data.raw[,1]
		trend.data[[what]] <- trend.data.raw[order(fazids.tr[[what]]), 2:ncol(trend.data.raw), drop=FALSE]
		fazids.tr[[what]] <- sort(fazids.tr[[what]])
		colnames(trend.data[[what]]) <- substr(colnames(trend.data[[what]]), 2,5)
		trend.data[[what]] <- trend.data[[what]][,is.element(colnames(trend.data[[what]]), as.character(years)), drop=FALSE]
	}
	lut.file.name <- file.path(wrkdir, 'data',  paste(what, '_LUT_', geography, '.txt', sep=''))
	if(file.exists(lut.file.name)) {
		lut.data.raw <- data.frame(read.table(lut.file.name, sep='\t', header=TRUE))
		fazids.lut[[what]] <- lut.data.raw[,1]
		lut.data[[what]] <- lut.data.raw[order(fazids.lut[[what]]), 2:ncol(lut.data.raw)]
		fazids.lut[[what]] <- sort(fazids.lut[[what]])
		colnames(lut.data[[what]]) <- substr(colnames(lut.data[[what]]), 2,5)
		lut.data[[what]] <- lut.data[[what]][,is.element(colnames(lut.data[[what]]), as.character(years))]
	}

	sim[[what]] <- fazids[[what]] <- CIs[[what]] <- list()
	for(irun in 1:length(runs)) {
		run <- runs[irun]
		# Load indicators
		data <- read.table(file.path(sim.dir, paste(sim.prefix, run, sep=''), 
						'indicators', paste(geography, '__table__', what, '.csv', sep='')), sep=',', header=TRUE)
		sim[[what]][[run]] <- data[,2:ncol(data)]
		fazids[[what]][[run]] <- data[,1]
		sim[[what]][[run]] <- sim[[what]][[run]][order(fazids[[what]][[run]]),]
		fazids[[what]][[run]] <- sort(fazids[[what]][[run]])
	}
}
area.names <-  faz_names[,c('faz_id', 'Name')]
colnames(area.names) <- c('id', 'name')
#zones <- fazids[[indicators[1]]][[runs[1]]]
zones <- as.integer(names(comments))
years.for.df <- sort(unique(c(years, all.years)))
lyears.for.df <- length(years.for.df)
file.append <- FALSE
cat('\nProcessing ', length(zones), ' geographies:\n')
pdf(output.file.name.pdf, width=12, height=9)
for(faz in sort(zones)) {
	cat(faz, ', ')
	flush.console()
  	g <- gtab <- list()
	for (what in indicators) {
		faz_not_found <- FALSE
		tabDF <- data.frame(Time=years.for.df)
		last.table.columns <- NA	
		run.table.columns <- c()
		for(irun in 1:length(runs)) {
			run <- runs[irun]
			idx <- which(fazids[[what]][[run]]==faz)
			if (length(idx) <=0) {faz_not_found <- TRUE; break}
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
		if(faz_not_found) next
		idxt <- which(fazids.tr[[what]]==faz)
		obs.df <- lut.df <- NULL
		if(!is.null(trend.data[[what]])) {
			obs.df <- data.frame(run=rep('Actual', ncol(trend.data[[what]])), 
						Time=as.integer(colnames(trend.data[[what]])), 
						amount=as.numeric(trend.data[[what]][idxt, ]))
		}	
		idxl <- which(fazids.lut[[what]]==faz)
		if(!is.null(lut.data[[what]])) {
			lut.df <- data.frame(run=rep('LUT', ncol(lut.data[[what]])), 
						Time=as.integer(colnames(lut.data[[what]])), 
						amount=as.numeric(lut.data[[what]][idxl, ]))
		}
		datafrs <- rbind(datafrs, obs.df, lut.df)
		datafrs <- datafrs[!is.na(datafrs$amount),]
		tabDF$Actual <- rep(NA, nrow(tabDF))
		tabDF$Actual[tabDF$Time %in% as.integer(colnames(trend.data[[what]]))] <- trend.data[[what]][idxt,]
		tabDF$LUT <- rep(NA, nrow(tabDF))
		tabDF$LUT[tabDF$Time %in% as.integer(colnames(lut.data[[what]]))] <- lut.data[[what]][idxl,]
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
		# Create table
		tidx <- c(which(is.element(tabDF[,1], years.for.table)), which(tabDF[,1]==9999))
		tidx.raw <- tidx
		rown <- tabDF[tidx,1]
		last.row <- c()
		for(column in colnames(tabDF)) tabDF[tidx.raw,column] <- as.integer(tabDF[tidx.raw,column])
		lastcols <- c() # this removes low and high from the table
		columns <- c('Actual', 'LUT', unlist(strsplit(paste(paste(run.table.columns, collapse=','), sep=','), ',')), lastcols)
		format.table <- format(tabDF[tidx.raw,columns],
							justify='right', big.mark = ","
							)
		#tabDF[is.na(tabDF)] <- ''
		gtab[[what]] <- list()
		
		#tabidxs <- list(1:2, 3:(length(mid.table.columns)+2), (length(mid.table.columns)+3):length(columns))
		#for(i in 1:3){
		#	gtab[[what]][[i]] <- tableGrob(format(tabDF[tidx,columns[tabidxs[[i]]]],
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
    	#}
    	tabDF <- cbind(faz, tabDF)
    	colnames(tabDF)[1] <- 'area_id'
    	if(save.data.as.ascii) {
    		write(titles[[what]], file=output.file.name.txt, append=file.append)
    		write.table(as.matrix(tabDF[tidx,c(colnames(tabDF)[1], columns)]), file=output.file.name.txt, append=TRUE, row.names=FALSE, sep='\t')
    		write('\n', file=output.file.name.txt, append=TRUE)
    		file.append <- TRUE
    	}
	}
	#mpd.idx <- which(mpds[,geography.id]==faz)
	sub <- ''
	#sub <- paste('MPDs - HU:', sum(mpds[mpd.idx, c('sf_units', 'mf_units')]), ', Non-res sqft:', sum(mpds[mpd.idx, c('comm_sqft', 'off_sqft',  'ind_sqft')]))
	if(is.element(faz, as.integer(names(comments)))) {
		sub <- paste(sub, '\n', comments[as.character(faz)])
		main.col <- 'red'
	} else main.col <- 'black'
	larger.area <- subset(zone.faz, zone_id==faz)$faz_id
	# Assemble page		
	grid.arrange(gtab[[indicators[1]]], g[[indicators[1]]],
				 gtab[[indicators[2]]], g[[indicators[2]]],
				 gtab[[indicators[3]]], g[[indicators[3]]],
				ncol=2, 
				main=textGrob(paste(faz, '- FAZ ', area.names[area.names[,'id']==larger.area,'name'], '(', larger.area, ')'), 
						gp=gpar(fontsize=14,fontface="bold", col=main.col, fill=main.col), 
						just=c('center', 'top')),
				heights=unit(0.33, "npc"),
				sub=textGrob(sub, gp=gpar(fontsize=11), just=c('center', 'bottom')),
				just='left')

}
cat('\n')
dev.off()
