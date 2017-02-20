#!/bin/bash

## ./rmd2html.sh whatever.Rmd

## puzzles arise if the file name that user interactive
## types has quotes around it,
## don't understand that yet. just all works



function render {
	echo "In render:"
	cmd="library(rmarkdown); library(knitr); render(\"$1\", "$2", quiet = TRUE)"
	echo $cmd
    R -q --slave --vanilla -e "$cmd"
}


## Default is html
fmt="html"

TEMP=`getopt -o fh: --long fmt:,help -n 'test.sh' -- "$@"`
eval set -- "$TEMP"

while true
do
    case "$1" in
	-h|--help)
	 echo "Used to render from Rmd to html"
	    exit
	    ;;
	--) shift ; break ;;
	*) echo "Error"; exit 1 ;;
    esac
done

output_format='html_document(css = system.file("extdata/theme", "kutils.css", package = "crmda"))'

filename=$1
fn=$(basename "$filename")
exten="${fn##*.}"

echo "$fn"
echo "$exten"


if [[ $filename == "" ||  "$exten" != "Rmd" ]]
then
    echo -e "These are the Rmd files in the current directory." 
    echo -e "\n" $(ls  *.Rmd)
    read -p "Please indicate which you want, or hit Enter for all: " filename
fi



if [[ -e $filename ]]
then
       render "$filename" "$output_format"
else
for fn in *.Rmd
do
        render "$fn" "$output_format"
done
fi
 
