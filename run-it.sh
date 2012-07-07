#!/bin/sh

# DIR PATHS
BASE_PATH=/Users/jwsm/Desktop/WACM/project

LISP_DIR=$BASE_PATH/project-v2
TMP_DIR=$BASE_PATH/pdf_output/tmp
OUTPUT_DIR=$BASE_PATH/pdf_output
LILY_POND_TEMPLATES=$BASE_PATH/project-v2/templates

# PROGRAM PATHS
LILY_POND_EXECUTABLE=/Applications/LilyPond.app/Contents/Resources/bin/lilypond
PREVIEW_APP=/Applications/Preview.app
PHP_REPLACER=$BASE_PATH/project-v2/replacer.php
LISP_SCRIPT=$BASE_PATH/project-v2/write-lily-pond-files.lisp

# FILE PATHS
USE_LILY_POND_TEMPLATE=$LILY_POND_TEMPLATES/chord-analysis-template.ly
LILY_POND_WORKING_TEMPLATE=$TMP_DIR/template.ly
LILY_POND_SOURCE=$TMP_DIR/analysis.ly
LILY_POND_OUTPUT=$OUTPUT_DIR


echo "Cleaning the Temp Directory..."
rm $TMP_DIR/*

echo "Running Lisp Code..."
$LISP_SCRIPT

echo "Copying Template to Temp Directory..."
cp $USE_LILY_POND_TEMPLATE $LILY_POND_WORKING_TEMPLATE

echo "Filling in Template..."
php $PHP_REPLACER

echo "Running Lily Pond..."
$LILY_POND_EXECUTABLE -o $LILY_POND_OUTPUT $LILY_POND_SOURCE

echo "Launching PDF..."
open -a $PREVIEW_APP $LILY_POND_OUTPUT/analysis.pdf