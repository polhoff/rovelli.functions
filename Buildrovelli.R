
Buildrovelli <- function(n_ver)
	{
	
	try (library(roxygen2))
	try (library(tools))
	try (library(inlinedocs))
	try (library(devtools))


	dirtop <- "/home/sjparker/sp/"

	dirpackagetop     <-  paste( dirtop, "Rpackage/", sep = "")
	packagename <- "rovelli"

	dirpackage <- paste(dirpackagetop, packagename, sep = "")


	setwd ( dirpackagetop)


	c_check <-  paste ( "R CMD check      ", dirpackage )
	system (c_check)
 
	c_build <-  paste ( "R CMD build --resave-data      ", dirpackage )
	system (c_build)

	install_string <- paste ( 'install.packages ( paste ( dirdmp, "/", packagename, "_", n_ver, ".tar.gz", sep = ""), type = "source" )')

	return (install_string)
	}

#x1 <- Buildrovelli (1.1)
#install.packages ( 'rovelli_1.1.tar.gz', type = 'source' )
