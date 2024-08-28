#!/bin/bash

python calc_importances.py data/fanova_image.simple_x.csv data/fanova_image.simple_y.csv image.simple.importance.csv
python calc_importances.py data/fanova_image.advanced_x.csv data/fanova_image.advanced_y.csv image.advanced.importance.csv
python calc_importances.py data/fanova_text.simple_x.csv data/fanova_text.simple_y.csv text.simple.importance.csv