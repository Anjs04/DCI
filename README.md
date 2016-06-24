
Digital Capacity Index
========================

This project contains code for analysing and visualising
results of the *Digital Capacities Index* survey, developed by
Western Sydney University in conjunction with Google Australia.

For further information, please contact the project team at the [Young and Well CRC](http://www.uws.edu.au/ics/research/projects/yawcrc).



## Notes

The *DCI* project uses [R](https://www.r-project.org/) and [Markdown](https://daringfireball.net/projects/markdown/) heavily. These are used to process data, generate graphs and produce draft reports in HTML, Microsoft Word and PDF formats.


To generate the main report in all formats:

```
Rscript -e 'rmarkdown::render("doc/DCI.Rmd", output_format="html_document")'
```

To run the main script:

```
Rscript R/main.R
```


## Structure
