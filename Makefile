# default target def ------------------------------------------------------
all: uninstall getdeps build install roxygenize readme ghpages clean

# debugging target def ----------------------------------------------------
debug: uninstall build install clean

# air lintr target def ----------------------------------------------------
air:
	@echo "[INFO] Formatting code with air..."
	air format .
	@echo "[OKAY] Done!"

bump:
	@if [ -z "$(v)" ]; then \
		echo "❌ Error: No version number provided. Use: make bump v=2.1.0"; \
		exit 1; \
	fi
	@git checkout -b release/v$(v)
	@echo "🌿 Switched to new branch release/v$(v)"
	@echo "🔧 Bumping version to $(v)..."
	@today=$$(date +%Y-%m-%d); \
	sed -i.bak -E "s/^Version: .*/Version: $(v)/" DESCRIPTION && \
	sed -i.bak -E "s/^Date: .*/Date: $$today/" DESCRIPTION && \
	rm DESCRIPTION.bak && \
	echo "📄 Updated DESCRIPTION with version $(v) and date $$today"

# CRAN presubmission checks target def ------------------------------------
cran:
	air format .
	@echo "[INFO] Building eyeris package for CRAN presubmission checks..."
	mkdir -p build/cran-presubmission
	Rscript -e "devtools::build(pkg = '.', path = 'build/cran-presubmission')"
	@echo "[INFO] Running CRAN presubmission checks..."
	R CMD check --as-cran build/cran-presubmission/eyeris_*.tar.gz
	@echo "[INFO] CRAN presubmission checks completed!\n"
	@echo "[INFO] Cleaning up CRAN presubmission build directory..."
	rm -rf build/cran-presubmission
	@echo "[OKAY] Done!"

# CRAN submission target def -----------------------------------------------
submit:
	@echo "[INFO] Submitting eyeris package to CRAN..."
	Rscript -e "devtools::submit_cran()"
	@echo "[INFO] eyeris package submitted to CRAN!\n"
	@echo "[OKAY] Done!"

# CRAN use github release target def ---------------------------------------
release:
	@if [ -z "$(v)" ]; then \
		echo "❌ Error: No version number provided. Use: make release v=2.1.0"; \
		exit 1; \
	fi
	@git checkout release/v$(v)
	@echo "🌿 Switched to branch release/v$(v)"
	@echo "[INFO] Creating github release for eyeris package..."
	Rscript -e "usethis::use_github_release()"
	@echo "[INFO] github release created!\n"
	@echo "[OKAY] Done!"

# uninstall dev version of package if loaded ------------------------------
uninstall:
	@echo "[INFO] Uninstalling previous eyeris package dev installation..."
	Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools', repos = 'http://cran.us.r-project.org')" > /dev/null
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) devtools::unload('eyeris')" > /dev/null
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) remove.packages('eyeris')" > /dev/null
	@echo "[OKAY] Previous eyeris package dev installation uninstalled successfully!\n"

# install package deps ----------------------------------------------------
getdeps:
	@echo "[INFO] Installing R package dependencies..."
	Rscript -e "deps <- c('eyelinker', 'dplyr', 'gsignal', 'tidyr', 'zoo', 'purrr', 'cli', 'rlang', 'stringr', 'utils', 'stats', 'graphics', 'grDevices', 'progress', 'data.table', 'withr', 'lifecycle', 'MASS', 'viridis', 'fields', 'jsonlite', 'rmarkdown'); install.packages(deps, repos = 'http://cran.us.r-project.org')" > /dev/null
	Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org')" > /dev/null
	Rscript -e "remotes::install_github('crsh/depgraph')" > /dev/null
	@echo "[OKAY] Dependencies installed successfully!\n"

# build package -----------------------------------------------------------
build:
	@echo "[INFO] Starting eyeris devtools package build..."
	Rscript -e "devtools::document(pkg = '.')" > /dev/null
	mkdir -p build
	Rscript -e "devtools::build(pkg = '.', path = 'build')" > /dev/null
	@echo "[OKAY] eyeris devtools package built successfully!\n"

# install package ---------------------------------------------------------
install:
	@echo "[INFO] Starting eyeris package install..."
	Rscript -e "install.packages(Sys.glob('build/eyeris_*.tar.gz'), repos = NULL, type = 'source')" > /dev/null
	@echo "[OKAY] eyeris package installed successfully!\n"

# check package -----------------------------------------------------------
check:
	@echo "[INFO] Starting eyeris package rcmd check..."
	Rscript -e "devtools::check(pkg = '.')" > /dev/null
	@echo "[OKAY] eyeris package rcmd check completed!\n"

# roxygenize package docs -------------------------------------------------
roxygenize:
	@echo "[INFO] Generating eyeris package docs with roxygen2..."
	Rscript -e "roxygen2::roxygenize('.')" > /dev/null
	@echo "[OKAY] Roxygenize completed!\n"

# generate readme ---------------------------------------------------------
readme:
	@echo "[INFO] Building eyeris package github README..."
	Rscript -e "devtools::build_readme()" > /dev/null
	@echo "[OKAY] README update completed!\n"

# generate pkgdown docs website preview -----------------------------------
website:
	@echo "[INFO] Building eyeris pkgdown docs website preview..."
	Rscript -e "pkgdown::build_site()" > /dev/null
	@echo "[OKAY] pkgdown website preview build completed!\n"

# render pkgdown github pages website (jekyll) ----------------------------
ghpages:
	@echo "[INFO] Building eyeris pkgdown docs website for github pages..."
	Rscript -e "devtools::install('.')" > /dev/null
	Rscript -e "pkgdown::build_site_github_pages(clean = TRUE, install = FALSE, new_process = FALSE)" > /dev/null
	@echo "[OKAY] pkgdown website build for github pages completed!\n"

# clean up build directory ------------------------------------------------
clean:
	@echo "[INFO] Cleaning up eyeris package build directory..."
	rm -rf build 2>/dev/null || true
	@echo "[OKAY] eyeris build directory cleaned!\n"
