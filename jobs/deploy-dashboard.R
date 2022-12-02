library(rmarkdown)
render("dashboard/affluenza-dashboard.Rmd")
file.rename("dashboard/affluenza-dashboard.html", "docs/affluenza-dashboard.html")
