# default target
all: uninstall getdeps build install roxygenize readme ghpages clean

# debugging target
debug: uninstall build install clean

# uninstall dev version of package if loaded
uninstall:
	@echo "[ INFO ] - uninstalling previous eyeris package dev installation..."
	Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools', repos = 'http://cran.us.r-project.org')"
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) devtools::unload('eyeris')"
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) remove.packages('eyeris')"
	@echo "[  OK  ] - previous eyeris package dev installation uninstalled successfully!\n"

# install deps
getdeps:
	@echo "[ INFO ] - installing R package dependencies..."
	Rscript -e "install.packages(c('eyelinker', 'dplyr', 'gsignal', 'tidyr', 'zoo'), repos = 'http://cran.us.r-project.org')"
	Rscript -e "install.packages('remotes')"
	Rscript -e "remotes::install_github('crsh/depgraph')"
	@echo "[  OK  ] - dependencies installed successfully!\n"

# build package
build:
	@echo "[ INFO ] - starting eyeris package build..."
	Rscript -e "devtools::document(pkg = '.')"
	mkdir build
	Rscript -e "devtools::build(pkg = '.', path = 'build')"
	@echo "[  OK  ] - eyeris package built successfully!\n"

install:
	@echo "[ INFO ] - starting eyeris package install..."
	Rscript -e "install.packages(Sys.glob('build/eyeris_*.tar.gz'), repos = NULL, type = 'source')"
	@echo "[  OK  ] - eyeris package installed successfully!\n"

# check package
check:
	@echo "[ INFO ] - starting eyeris package rcmd check..."
	Rscript -e "devtools::check(pkg = '.')"
	@echo "[  OK  ] - eyeris package rcmd check completed!\n"

# roxygenize
roxygenize:
	@echo "[ INFO ] - generating eyeris package docs with roxygen2..."
	Rscript -e "roxygen2::roxygenize('.')"
	@echo "[  OK  ] - roxygenize completed!\n"

# readme
readme:
	@echo "[ INFO ] - building eyeris package github README..."
	Rscript -e "devtools::build_readme()"
	@echo "[  OK  ] - README update completed!\n"

# pkgdown website preview
website:
	@echo "[ INFO ] - building eyeris pkgdown docs website preview..."
	Rscript -e "pkgdown::build_site()"
	@echo "[  OK  ] - pkgdown website preview build completed!\n"

# pkgdown github pages website (jekyll)
ghpages:
	@echo "[ INFO ] - building eyeris pkgdown docs website for github pages..."
	Rscript -e "devtools::install('.')"
	Rscript -e "pkgdown::build_site_github_pages(clean = TRUE, install = FALSE, new_process = FALSE)"
	@echo "[  OK  ] - pkgdown website build for github pages completed!\n"

# clean build directory
clean:
	@echo "[ INFO ] - cleaning eyeris package build directory..."
	rm -rf build
	@echo "[  OK  ] - eyeris build directory cleaned!\n"
