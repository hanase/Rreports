##### BEGIN USER SETTINGS ######
runs <- c(219, 245) # runs to include
ci.run <- c(245) # which run has confidence intervals 
			     # (only the last one is included in the table)
show.median <- FALSE
ci.run.name <- list("235" = 'MRr') # only needed if different from run number
ref.run <- c('219_fpp', '245_mr1') # refined values (black dots) 
ref.names <- c('ref 219', 'ref 245')
refining <- c(219, 245)
ref.cols <- c('black', 'red')

# Directory containing the runs with indicators. 
# The indicators directory must contain files of the type 'faz__tab__xxx.tab'
#sim.dir <- '/Users/hana/workspace/data/psrc_parcel/runs' 
sim.dir <- file.path(getwd(), 'runs') 
sim.prefix <- 'run_' # name of the run sub-directory without the run number, e.g. 'run_' for directory run_199

geography <- 'tract10'
geo.id <- paste0(geography, '_id')

# Directory containing confidence intervals
# It should contain a sub-directory 'runxxx_quantiles' with geo-level CI files (xxx is ci.run)
ci.dir <- file.path(getwd(), 'quantiles')

output.file.name <- paste(geography, 'report_', paste(runs, collapse='_'), 
							if(length(ref.run)>0) paste('_', paste(ref.run, collapse='_'), sep='') else '', sep='')

years <- c(2000, 2010, 2020, 2025, 2030, 2035, 2040) # for plots
years.for.table <- seq(2000, 2040, by=10)
years.for.refinement <- list('219'=c(2020, 2030, 2040), '245'=c(2020, 2030, 2040))
show.all.refined.years <- TRUE

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
source(file.path(wrkdir, 'data/comments.R'), chdir=TRUE)

trim.leading <- function (x)  sub("^\\s+", "", x)

remove.na <- function(data)
	apply(data, c(1,2), function(x) if(trim.leading(x)=='NA') '' else x)

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
						'indicators', paste0(geography, '__tab__', what, '.tab')), header=TRUE)
		sim[[what]][[run]] <- data[,2:ncol(data)]
		ids[[what]][[run]] <- data[,1]
		sim[[what]][[run]] <- sim[[what]][[run]][order(ids[[what]][[run]]),]
		ids[[what]][[run]] <- sort(ids[[what]][[run]])

		# Load confidence intervals
		CIs[[what]][[run]] <- list()
		if(run %in% ci.run) {
			crun.name <- if(is.null(ci.run.name[[as.character(run)]])) run else ci.run.name[[as.character(run)]]
			for(year in years[which(years>=2020)[1]:lyears]) {
				ci.file.name <- file.path(ci.dir, 
									paste('run', crun.name, '_quantiles_', geography, sep=''), 
									paste(geography, '_',
											ci.names[[what]], '_ci_', year, '_propf_add', sep=''))
				if(file.exists(ci.file.name)) {
					CIs[[what]][[run]][[year]] <- read.table(ci.file.name, header=TRUE)
					colnames(CIs[[what]][[run]][[year]])[1] <- 'id'
				}
			}
		}
	}
	sim.ref[[what]] <- ids.ref[[what]] <- list()
	if(length(ref.run) > 0) {
		for(irun in 1:length(ref.run)) {
			run <- ref.run[irun]
			data <- read.table(file.path(sim.dir, paste(sim.prefix, run, sep=''), 
						'indicators', paste0(geography, '__tab__', what, '.tab', sep='')), header=TRUE)
			sim.ref[[what]][[run]] <- data[,colnames(data)%in%paste(what, c(2020,2030,2040), sep='_'), drop=FALSE]
			ids.ref[[what]][[run]] <- data[,1]
			sim.ref[[what]][[run]] <- sim.ref[[what]][[run]][order(ids.ref[[what]][[run]]),, drop=FALSE]
			ids.ref[[what]][[run]] <- sort(ids.ref[[what]][[run]])
			if(!show.all.refined.years) {
				for (i in 1:ncol(sim.ref[[what]][[run]])) {
					coln <- colnames(sim[[what]][[refining[irun]]])
					unref <- sim[[what]][[refining[irun]]][,which(as.integer(substr(coln, nchar(coln)-3, nchar(coln)))==c(2020,2030,2040)[i])]			
					diffs <- abs(sim.ref[[what]][[run]][,i] - unref)
					sim.ref[[what]][[run]][diffs <= unref/100,i] <- NA
				}
			}
		}
	}
}

geo.names <- data.frame(read.table(file.path(wrkdir, 'data', 'tract10s.csv'), sep=',', header=TRUE))
geo.names <- geo.names[,c(1, 2)]
colnames(geo.names) <- c('id', 'name')

file.append <- FALSE
pdf(output.file.name.pdf, width=12, height=9)
for(geo in sort(ids[[indicators[1]]][[runs[1]]])) {
  	g <- gtab <- list()
	for (what in indicators) {
		geo_not_found <- FALSE
		tabDF <- data.frame(Time=years)
		last.table.columns <- NA	
		run.table.columns <- ref.table.columns <-c()
		for(irun in 1:length(runs)) {
			run <- runs[irun]
			idx <- which(ids[[what]][[run]]==geo)
			if (length(idx) <=0) {geo_not_found <- TRUE; break}
			coln <- colnames(sim[[what]][[run]])
			run.cols <- substr(coln, nchar(coln)-3, nchar(coln))
			col.idx <- which(run.cols %in% as.character(years))
			this.data <- data.frame(run=rep(run, lyears), Time=years, 
							amount=as.numeric(c(rep(NA, lyears-length(col.idx)), 
										sim[[what]][[run]][idx,col.idx])))
			this.data.med <- NULL
			if(length(CIs[[what]][[runs[irun]]])>0) {
				ci.low <- ci.high <- c(rep(NA, sum(years<=2000)), sim[[what]][[run]][idx,2])
				ci.med <- c(rep(NA, sum(years< 2000)), sim[[what]][[run]][idx,1:2])
				for(year in years[3:lyears]) {
					idxci <- which(CIs[[what]][[run]][[year]][,'id']==geo)
					ci.low <- c(ci.low, CIs[[what]][[run]][[year]][idxci,'lower_80'])
					ci.high <- c(ci.high, CIs[[what]][[run]][[year]][idxci,'upper_80'])
					if (show.median) 
						ci.med <- c(ci.med, CIs[[what]][[run]][[year]][idxci,'median'])
				}
				if (show.median) {
					crun.name <- if(is.null(ci.run.name[[as.character(run)]])) run else ci.run.name[[as.character(run)]]
					this.data.med <- data.frame(run=rep(crun.name, lyears), Time=years, 
												amount=as.numeric(c(rep(NA, lyears-length(col.idx)), ci.med)))
					this.data.med$CI.low <- ci.low
					this.data.med$CI.high <- ci.high
					ci.low <- ci.high <- rep(NA, lyears)
				}
			} else ci.low <- ci.high <- rep(NA, lyears)
			this.data$CI.low <- ci.low
			this.data$CI.high <- ci.high
			datafrs <- if(irun == 1) this.data else datafrs <- rbind(datafrs, this.data)
			if(!is.null(this.data.med))
				datafrs <- rbind(datafrs, this.data.med)
			this.tabdata <- this.data[,c('amount', 'CI.low', 'CI.high')]
			this.tabdata[years==2010, c('CI.low', 'CI.high')] <- NA
			colnames(this.tabdata) <- c(paste('run', run), 
										paste('low', run), paste('high', run))
			run.table.columns <- c(run.table.columns, paste('run', runs[irun]))
			if(length(ci.run)>0 && !(runs[irun] == ci.run[length(ci.run)]) && is.null(this.data.med)) {
				this.tabdata <- this.tabdata[,1, drop=FALSE]
			} else last.table.columns <- c(paste('low', run), paste('high', run))
			tabDF <- cbind(tabDF, this.tabdata)
			if (!is.null(this.data.med)) {
				this.tabdata <- this.data.med[,c('amount', 'CI.low', 'CI.high')]
				this.tabdata[years==2010, c('CI.low', 'CI.high')] <- NA
				colnames(this.tabdata) <- c(paste('run', crun.name), 
											paste('low', crun.name), paste('high', crun.name))
				run.table.columns <- c(run.table.columns, paste('run', crun.name))
				last.table.columns <- c(paste('low', crun.name), paste('high', crun.name))
				tabDF <- cbind(tabDF, this.tabdata)
			}
		}
		if(geo_not_found) next
		idxt <- which(ids.tr[[what]]==geo)
		obs.df <- data.frame(run=rep('Actual', ncol(trend.data[[what]])), 
						Time=as.integer(colnames(trend.data[[what]])), 
						amount=as.numeric(trend.data[[what]][idxt, ]))
		obs.df$CI.high <- obs.df$CI.low <- rep(NA,nrow(obs.df))
		datafrs <- rbind(datafrs, obs.df)
		tabDF$Actual <- rep(NA, nrow(tabDF))
		tabDF$Actual[1:ncol(trend.data[[what]])] <- trend.data[[what]][idxt,]
		if(length(ref.run) > 0) {
			for(irun in 1:length(ref.run)) {
				refidx <- which(ids.ref[[what]][[ref.run[irun]]]==geo)
				datafrs <- rbind(datafrs, 
							data.frame(run=rep(ref.names[irun],3), Time=years.for.refinement[[as.character(refining[irun])]], 
								amount=as.numeric(sim.ref[[what]][[ref.run[irun]]][refidx,]),
								CI.low=NA, CI.high=NA))
				tabDF[[ref.names[irun]]] <- rep(NA, nrow(tabDF))
				tabDF[[ref.names[irun]]][is.element(tabDF[,1], years.for.refinement[[as.character(refining[irun])]])] <- sim.ref[[what]][[ref.run[irun]]][refidx,]
				ref.table.columns <- c(ref.table.columns, ref.names[irun])
			}
		}
		# Create plot of lines
		g[[what]] <- ggplot(subset(datafrs, !is.element(run, ref.names)), aes(Time, amount, colour=factor(run))) + geom_line() + 
							scale_y_continuous('') + scale_x_continuous('') + 
							scale_colour_discrete(name = '') +
							ggtitle(titles[[what]]) + 
							theme(legend.position=c(0,0.5), legend.justification=c(0,0), 
									legend.key=element_blank(),
									legend.key.size = unit(0.02, "npc"),
									plot.title=element_text(size=12))

		# Add confidence intervals 
		ciidx <- which(!is.na(datafrs$CI.high))
		if(length(ciidx) > 0) 
			g[[what]] <- g[[what]] + geom_ribbon(aes(ymin=CI.low, ymax=CI.high, linetype=NA), alpha=0.1)
		
		# Add Refinement point
		if(length(ref.run) > 0) {
			for(irun in 1:length(ref.run))
				g[[what]] <- g[[what]] + geom_point(data=subset(datafrs, run==ref.names[irun]), 
									colour=ref.cols[irun], aes(Time, amount, guide=FALSE))
		}
		# Create table
		tidx <- c(which(is.element(tabDF[,1], years.for.table)), which(tabDF[,1]==9999))
		tidx.raw <- tidx
		rown <- tabDF[tidx,1]
		last.row <- c()
		for(column in colnames(tabDF)) tabDF[tidx.raw,column] <- as.integer(tabDF[tidx.raw,column])
		lastcols <- if(length(last.table.columns)==1 && is.na(last.table.columns)) c() else last.table.columns
		columns <- c('Actual', unlist(strsplit(paste(paste(run.table.columns, collapse=','), paste(ref.table.columns, collapse=','), sep=','), ',')), lastcols)
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
    	write(titles[[what]], file=output.file.name.txt, append=file.append)
    	write.table(as.matrix(tabDF[tidx,c(colnames(tabDF)[1], columns)]), file=output.file.name.txt, append=TRUE, row.names=FALSE, sep='\t')
    	write('\n', file=output.file.name.txt, append=TRUE)
    	file.append <- TRUE
	}
	# Assemble page		
	grid.arrange(gtab[[indicators[1]]], g[[indicators[1]]],
				 gtab[[indicators[2]]], g[[indicators[2]]],
				 gtab[[indicators[3]]], g[[indicators[3]]],
				ncol=2, 
				main=textGrob(geo.names[geo.names[,'id']==geo,'name'],
						#paste(geo, '-', geo.names[geo.names[,'id']==geo,'name']), 
						gp=gpar(fontsize=14,fontface="bold"), 
						just=c('center', 'top')),
				heights=unit(0.33, "npc"),
				sub=textGrob('', gp=gpar(fontsize=10, fontface='italic'), just=c('center', 'bottom')),
				just='left')

}
dev.off()