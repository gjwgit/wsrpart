.PHONY: update
update: varWeights.update selectVars.update

.PHONY: %.update
%.update:
	R CMD BATCH $*.R
	mv $*.Rout $*.Rout.save
	rm .RData
