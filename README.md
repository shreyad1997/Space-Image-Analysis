Astronomical Image Classification Using R

Project Overview

This project classifies astronomical images into three categories: Galaxy, Planet, and Space using R. The images are preprocessed by resizing them to a uniform size and converting them to grayscale, simplifying the data for a Random Forest classification model. Users can input new images, analyze them, and generate visualizations such as color intensity histograms and edge detection plots. The project is designed as a foundation for educational tools that make space exploration more accessible and engaging, especially for children.

Files

	1.	R Script (classification_script.R):
This script contains the entire code for preprocessing images, training the Random Forest model, and performing the classification of new images. It also includes functions for visualizing the image data and generating analysis reports.
	2.	Report (project_report.pdf):
This document provides a detailed explanation of the project, including the problem definition, solution methodology, and the results of the classification. It also covers the limitations, future improvements, and potential educational applications of the program.

How to Run the Program

	1.	Ensure that you have R and RStudio installed on your system.
	2.	Install the necessary packages: imager, ggplot2, dplyr, randomForest, and magick.
	3.	Source the main R script (classification_script.R) to execute the program.
	4.	Input the name of an image to analyze when prompted.
	5.	The program will output various analyses, including predicted image categories, color intensity histograms, and edge detection plots.

Future Improvements

	•	Expanding the image categories beyond Galaxy, Planet, and Space.
	•	Integrating convolutional neural networks (CNNs) to improve classification accuracy.
	•	Developing a user-friendly web interface to make the program accessible to children as an educational tool.
