

## all cran files are downloaded from a ftp server

setwd("/Volumes/Elements/cran_mirror/src/contrib/")


## move current versions to Archive/

package_files = list.files(pattern = ".tar.gz$")
packages = gsub("_.*$", "", package_files)

setwd("Archive")

library(GetoptLong)

for(i in seq_along(packages)) {
	if(!dir.exists(packages[i])) {
		dir.create(packages[[i]])
		qqcat("create folder: @{packages[i]}\n")
	}

	file.copy(paste0("../", package_files[i]), to = paste0(packages[i], "/", package_files[i]))
	qqcat("- copy @{package_files[i]}, @{i}/@{length(packages)}\n")
}


## validate

setwd("/Volumes/Elements/cran_mirror/src/contrib")

validate_pkg = function(fn) {

	pkg = gsub("_.*$", "", fn)
	fn = qq("Archive/@{pkg}/@{fn}")

	if(!file.exists(fn)) {
		return(FALSE)
	}

	try(code <- system(qq("gunzip -t @{fn}"), ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
	if(code != 0) {
		return(FALSE)
	} else {
		return(TRUE)
	}
}

library(rvest)
library(GetoptLong)
failed = list()
html = read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")
tb = html %>% html_element("table") %>% html_table()
tb = tb[-1, ]
all_pkgs = tb[[1]]
all_pkgs = all_pkgs[all_pkgs != ""]

for(i in seq_along(all_pkgs)) {
	pkg = all_pkgs[i]
	qqcat("- @{pkg}, @{i}/@{length(all_pkgs)}\n")
	oe = try(html <- read_html(paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/")), silent = TRUE)

	if(!inherits(oe, "try-error")) {
		tb2 = html %>% html_element("table") %>% html_table()
		ind = which(!tb2[[2]] %in% c("", "Parent Directory"))
		tb2 = tb2[ind, ]

		archived_pkgs = tb2[[2]]
		for(ap in archived_pkgs) {
			if(!validate_pkg(ap)) {
				qqcat("  - (re)-download @{ap}\n")
				download.file(qq("https://cran.r-project.org/src/contrib/Archive/@{pkg}/@{ap}"), qq("Archive/@{pkg}/@{ap}"), quiet = TRUE)
			} else {
				qqcat("  + validated, @{ap}\n")
			}
		}
	} else {
		qqcat("  - no archived package\n")
	}

	html = read_html(qq("https://cran.r-project.org/web/packages/@{pkg}/index.html"))

	html %>% html_elements("table") -> obj
	if(length(obj)) {
		tb3 = obj[[3]] %>% html_table()

		fn = grep("\\.tar\\.gz$", tb3[[2]], value = TRUE)
		if(length(fn)) {
			if(!validate_pkg(fn)) {
				qqcat("  - (re)-download @{fn}\n")
				download.file(qq("https://cran.r-project.org/src/contrib/@{fn}"), qq("Archive/@{pkg}/@{fn}"), quiet = TRUE)
			} else {
				qqcat("  + validated, @{fn}\n")
			}
		} else {
			failed[[fn]] = TRUE
		}
	}
}


#### packages that are removed from CRAN

setwd("/Volumes/Elements/cran_mirror/src/contrib/Archive")

removed = list()
## move current versions to Archive/
packages = dir()

for(package in packages) {
	oe = try(html <- read_html(qq("https://cran.r-project.org/web/packages/@{package}/index.html")))
	while(inherits(oe, "try-error")) {
		Sys.sleep(10)
		oe = try(html <- read_html(qq("https://cran.r-project.org/web/packages/@{package}/index.html")))
	}
	text = html %>% html_element("body") %>% html_text()
	if(grepl("was removed from the CRAN repository", text)) {
		if(grepl("\\d{4}-\\d+-\\d+", text)) {
			removed[[package]] = gsub("^.*(\\d{4}-\\d+-\\d+).*$", "\\1", text)
			qqcat("- @{package} removed on @{removed[[package]]}\n")
		} else {

			oe = try(html <- read_html(qq("https://cran.r-project.org/src/contrib/Archive/@{package}/")))
			while(inherits(oe, "try-error")) {
				Sys.sleep(10)
				oe = try(html <- read_html(qq("https://cran.r-project.org/src/contrib/Archive/@{package}/")))
			}
			tb = html %>% html_element("table") %>% html_table()
			tb = as.data.frame(tb)
			tb = tb[-(1:2), -1]
			ind = which.max(as.Date(tb[, 2]))
			removed[[package]] = tb[ind, 2]
			qqcat("- @{package} removed on @{removed[[package]]}\n")
		}
	}
}

saveRDS(removed, file = "~/workspace/R_evolution/cran_removed.rds")


#### package versions that are archived

cran_archived_all_time = list()
for(package in packages) {
	oe = try(html <- read_html(qq("https://cran.r-project.org/src/contrib/Archive/@{package}/")), silent = TRUE)
	if(!inherits(oe, "try-error")) {
		cat(strrep("\b", 100))
		qqcat("- @{package}")
		tb = html %>% html_element("table") %>% html_table()
		tb = as.data.frame(tb)
		tb = tb[-1, ]
		tb = tb[-nrow(tb), ]

		cran_archived_all_time[[package]] = tb
	} else {
		cat("\n")
		qqcat("- @{package} is new\n")
	}
}
cran_archived_all_time = lapply(cran_archived_all_time, function(x) x[-1, 2:4, drop = FALSE])

saveRDS(cran_archived_all_time, file = "~/workspace/R_evolution/cran_archived_all_time.rds")


