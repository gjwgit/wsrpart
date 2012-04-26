help:
	@echo "Manage the wsrpart R package\n\
	===================================\n\n\
	Targets\n\
	-------\n\n\
	check\tCheck for issues with hte packaging\n\
	build\tGenerate wsrpart_1.0.tar.gz\n\
	install\tInstall on the local machine\n\
	"

.PHONY: check
check:
	R CMD check package

.PHONY: build
build: 
	R CMD build package

.PHONY: install
install:
	R CMD INSTALL wsrpart_1.0.tar.gz
