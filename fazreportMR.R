##### BEGIN USER SETTINGS ######
#runs <- c(219, 223, 235) # runs to include
runs <- c(219, 235, 245)
ci.run <- c(245) # which run has confidence intervals 
			       # (only the last one is included in the table)
show.median <- FALSE
#ci.run.name <- list("235" = 'MRr')
ci.run.name <- list("245" = 'MRr245')

#ref.run <- c('ref219', 'ref_mfsf219') # refined values (black dots) 
ref.run <- c('219_rb3')
ref.names <- c('ref 219')
ref.cols <- c('black', 'red')
aggregate.to.large.area <- FALSE

# Directory containing the runs with indicators. 
# The indicators directory must contain files of the type 'faz__tab__xxx.tab'
#sim.dir <- '/Users/hana/psrc3656/workspace/data/psrc_parcel/runs' 
sim.dir <- file.path(getwd(), 'runs') 
sim.prefix <- 'run_' # name of the run sub-directory without the run number, e.g. 'run_' for directory run_199

# Directory containing confidence intervals
# It should contain a sub-directory 'runxxx_quantiles' with faz-level CI files (xxx is ci.run)
ci.dir <- file.path(getwd(), 'quantiles')

output.file.name <- paste(if(aggregate.to.large.area) 'LA' else 'FAZ', 'report_', paste(runs, collapse='_'), 
							if(length(ref.run)>0) paste('_', paste(ref.run, collapse='_'), sep='') else '', sep='')

years <- c(2000, 2010, 2020, 2025, 2030, 2035, 2040) # for plots
years.for.table <- seq(2000, 2040, by=10)
years.for.refinement <- list('219_rb3'=c(2040))

###### END USER SETTINGS ############


library(ggplot2)
library(grid)
library(gridExtra)

lyears <- length(years)
indicators <- c('households',  'population', 'jobs')
ci.names <- list(jobs='job', households='household', population='population')
titles <- list(households='Households', jobs='Employment', population='HH Population')

geography <- if(aggregate.to.large.area) 'large_area' else 'faz'

output.file.name.pdf <- paste(output.file.name,  'pdf', sep='.')
output.file.name.txt <- paste(output.file.name,  'txt', sep='.')

wrkdir <- getwd()
source(file.path(wrkdir, 'data/comments.R'), chdir=TRUE)

trim.leading <- function (x)  sub("^\\s+", "", x)

remove.na <- function(data)
	apply(data, c(1,2), function(x) if(trim.leading(x)=='NA') '' else x)

aggregate_by_la <- function(values, la.ids, uLAids) {
        res <- matrix(NA, nrow=length(uLAids), ncol=ncol(values))
        for(ila in 1:length(uLAids)) {
         	res[ila,] <- apply(values[la.ids==uLAids[ila],, drop=FALSE], 2, sum)
        }
        res <- data.frame(res)
        colnames(res) <- colnames(values)
        return(res)
}


sim <- sim.ref <- fazids <- fazids.ref <- CIs <- saf <- trend.data <- fazids.tr <- fazids.saf <- list()

id.correspondence <- data.frame(read.table(file.path(wrkdir, 'data', 'fazes.txt'), sep='\t', header=TRUE))
id.correspondence <- id.correspondence[order(id.correspondence[,'faz_id']),]
uLA <- sort(unique(id.correspondence[,'large_area_id']))


for (what in indicators) {
	# Load observed data
	trend.file.name <- file.path(wrkdir, 'data',  paste(what, '_observed_faz.txt', sep=''))
	if(file.exists(trend.file.name)) {
		trend.data.raw <- data.frame(read.table(trend.file.name, sep='\t', header=TRUE))
		fazids.tr[[what]] <- trend.data.raw[,1]
		trend.data[[what]] <- trend.data.raw[order(fazids.tr[[what]]), 2:ncol(trend.data.raw)]
		fazids.tr[[what]] <- sort(fazids.tr[[what]])
		colnames(trend.data[[what]]) <- substr(colnames(trend.data[[what]]), 2,5)
		trend.data[[what]] <- trend.data[[what]][,is.element(colnames(trend.data[[what]]), as.character(years))]
		if(aggregate.to.large.area) {
			trend.data[[what]] <- aggregate_by_la(trend.data[[what]], id.correspondence[,'large_area_id'], uLA)
			fazids.tr[[what]] <- uLA
		}
	}
	sim[[what]] <- fazids[[what]] <- CIs[[what]] <- list()
	for(irun in 1:length(runs)) {
		run <- runs[irun]
		# Load indicators
		data <- read.table(file.path(sim.dir, paste(sim.prefix, run, sep=''), 
						'indicators', paste('faz__tab__', what, '.tab', sep='')), header=TRUE)
		sim[[what]][[run]] <- data[,2:ncol(data)]
		fazids[[what]][[run]] <- data[,1]
		sim[[what]][[run]] <- sim[[what]][[run]][order(fazids[[what]][[run]]),]
		fazids[[what]][[run]] <- sort(fazids[[what]][[run]])
		if(aggregate.to.large.area) {
			sim[[what]][[run]] <- aggregate_by_la(sim[[what]][[run]], id.correspondence[,'large_area_id'], uLA)
			fazids[[what]][[run]] <- uLA
		}
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
	sim.ref[[what]] <- fazids.ref[[what]] <- list()
	if(length(ref.run) > 0) {
		for(run in ref.run) {
			data <- read.table(file.path(sim.dir, paste(sim.prefix, run, sep=''), 
						'indicators', paste('faz__tab__', what, '.tab', sep='')), header=TRUE)
			sim.ref[[what]][[run]] <- data[,2:ncol(data), drop=FALSE]
			fazids.ref[[what]][[run]] <- data[,1]
			sim.ref[[what]][[run]] <- sim.ref[[what]][[run]][order(fazids.ref[[what]][[run]]),, drop=FALSE]
			fazids.ref[[what]][[run]] <- sort(fazids.ref[[what]][[run]])
			if(aggregate.to.large.area) {
				sim.ref[[what]][[run]] <- aggregate_by_la(sim.ref[[what]][[run]], 
												id.correspondence[,'large_area_id'], uLA)
				fazids.ref[[what]][[run]] <- uLA
			}
			for (i in 1:ncol(sim.ref[[what]][[run]])) {
				diffs <- abs(sim.ref[[what]][[run]][,i] - sim[[what]][[runs[length(runs)]]][,which(years[which(years==2000):lyears]==c(2020,2030,2040)[i])])
				sim.ref[[what]][[run]][diffs <= sim.ref[[what]][[run]][,i]/200,i] <- NA
			}
		}
	}
}
area.names <-  if(!aggregate.to.large.area) faz_names[,c('faz_id', 'Name')] else read.table(file.path(wrkdir, 'data', 'large_areas.txt'), sep='\t', header=TRUE)[,c('large_area_id', 'large_area_name')]
colnames(area.names) <- c('id', 'name')

file.append <- FALSE
pdf(output.file.name.pdf, width=12, height=9)
for(faz in sort(fazids[[indicators[1]]][[runs[1]]])) {
  	g <- gtab <- list()
	for (what in indicators) {
		faz_not_found <- FALSE
		tabDF <- data.frame(Time=years)
		last.table.columns <- NA	
		run.table.columns <- ref.table.columns <-c()
		for(irun in 1:length(runs)) {
			run <- runs[irun]
			idx <- which(fazids[[what]][[run]]==faz)
			if (length(idx) <=0) {faz_not_found <- TRUE; break}
			coln <- colnames(sim[[what]][[run]])
			run.cols <- substr(coln, nchar(coln)-3, nchar(coln))
			col.idx <- which(run.cols %in% as.character(years))
			amount <- sim[[what]][[run]][idx,col.idx]
			this.data <- data.frame(run=rep(run, lyears), Time=years, 
							amount=as.numeric(c(rep(NA, lyears-length(col.idx)), amount)))
			this.data.med <- NULL
			if(length(CIs[[what]][[run]])>0) {
				ci.low <- ci.high <- c(rep(NA, sum(years<=2000)), sim[[what]][[run]][idx,2])
				ci.med <- c(rep(NA, sum(years< 2000)), sim[[what]][[run]][idx,1:2])
				for(year in years[3:lyears]) {
					idxci <- which(CIs[[what]][[run]][[year]][,'id']==faz)
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
			run.table.columns <- c(run.table.columns, paste('run', run))
			if(!(runs[irun] == ci.run[length(ci.run)]) && is.null(this.data.med)) {
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
		if(faz_not_found) next
		idxt <- which(fazids.tr[[what]]==faz)
		obs.df <- data.frame(run=rep('Actual', ncol(trend.data[[what]])), 
						Time=as.integer(colnames(trend.data[[what]])), 
						amount=as.numeric(trend.data[[what]][idxt, ]))
		obs.df$CI.high <- obs.df$CI.low <- rep(NA,nrow(obs.df))
		datafrs <- rbind(datafrs, obs.df)
		tabDF$Actual <- rep(NA, nrow(tabDF))
		tabDF$Actual[1:ncol(trend.data[[what]])] <- trend.data[[what]][idxt,]
		if(length(ref.run) > 0) {
			for(irun in 1:length(ref.run)) {
				refidx <- which(fazids.ref[[what]][[ref.run[irun]]]==faz)
				datafrs <- rbind(datafrs, 
							data.frame(run=rep(ref.names[irun],3), Time=years.for.refinement[[ref.run]], 
								amount=as.numeric(sim.ref[[what]][[ref.run[irun]]][refidx,]),
								CI.low=NA, CI.high=NA))
				tabDF[[ref.names[irun]]] <- rep(NA, nrow(tabDF))
				tabDF[[ref.names[irun]]][is.element(tabDF[,1], years.for.refinement[[ref.run]])] <- sim.ref[[what]][[ref.run[irun]]][refidx,]
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

		# Add confidence intervals as polygons
		ciidx <- which(!is.na(datafrs$CI.high))
		if(length(ciidx) > 0) {
			#cidata <- data.frame(Time=c(datafrs$Time[ciidx], datafrs$Time[ciidx][length(ciidx):1]), 
			#				ci=c(datafrs$CI.low[ciidx], datafrs$CI.high[ciidx][length(ciidx):1]),
			#				label=c(rep('low', length(ciidx)), rep('high', length(ciidx))))
			#g[[what]] <- g[[what]] + geom_polygon(data=cidata, aes(Time, ci), fill='grey', alpha=0.4, colour=NA)
			g[[what]] <- g[[what]] + geom_ribbon(aes(ymin=CI.low, ymax=CI.high, linetype=NA), alpha=0.1)
		}
		#g[[what]] <- g[[what]] + geom_line() # re-draw the lines so that they are on top
		# Add Refinement point
		if(length(ref.run) > 0) {
			for(irun in 1:length(ref.run))
				g[[what]] <- g[[what]] + geom_point(data=subset(datafrs, run==ref.names[irun]), colour=ref.cols[irun], aes(Time, amount, guide=FALSE))
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
    	write(titles[[what]], file=output.file.name.txt, append=file.append)
    	write.table(as.matrix(tabDF[tidx,c(colnames(tabDF)[1], columns)]), file=output.file.name.txt, append=TRUE, row.names=FALSE, sep='\t')
    	write('\n', file=output.file.name.txt, append=TRUE)
    	file.append <- TRUE
	}
	sub <- ''
	if(!aggregate.to.large.area && is.element(faz, as.integer(names(comments)))) sub <- comments[as.character(faz)]
	main.col <- if(!aggregate.to.large.area && is.element(faz, unlist(primary.f))) 'red' 
				else if (!aggregate.to.large.area && is.element(faz, unlist(secondary.f))) 'blue' else 'black'
	# Assemble page		
	grid.arrange(gtab[[indicators[1]]], g[[indicators[1]]],
				 gtab[[indicators[2]]], g[[indicators[2]]],
				 gtab[[indicators[3]]], g[[indicators[3]]],
				ncol=2, 
				main=textGrob(paste(faz, '-', area.names[area.names[,'id']==faz,'name']), 
						gp=gpar(fontsize=14,fontface="bold", col=main.col, fill=main.col), 
						just=c('center', 'top')),
				heights=unit(0.33, "npc"),
				sub=textGrob(sub, gp=gpar(fontsize=10, fontface='italic'), just=c('center', 'bottom')),
				just='left')

}
dev.off()