#!/bin/bash

texhash
pdflatex $1.tex
bibtex $1
pdflatex $1.tex
pdflatex $1.tex
