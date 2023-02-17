//@ int(label="Channel for numerator", style = "spinner") Channel_Num
//@ int(label="Channel for denominator", style = "spinner") Channel_Denom
//@ int(label="Channel for transmitted light -- select 0 if none", style = "spinner") Channel_Trans
//@ string(label="Background and noise subtraction method", choices={"Reference image",  "Select an image area","Fixed values","None"}, style="listBox") Background_Method
//@ string(label="Thresholding method", choices={"Default","Huang","Intermodes","IsoData","IJ_IsoData","Li","MaxEntropy","Mean","MinError","Minimum","Moments","Otsu","Percentile","RenyiEntropy","Shanbhag","Triangle","Yen"}, style="listBox") Thresh_Method
//@ int(label = "ROIs per cell", style = "spinner") ROIsPerCell
//@ File(label = "Output folder:", style = "directory") outputDir

// biosensor-image-subtraction.ijm
// ImageJ macro to generate a ratio image from a multichannel Z stack
// Background and noise are both determined by a method selected by the user
// Input: multi-channel Z stack image, and optional reference image for background subtraction
// Outputs: 
//	mask and ratio images
//	measurements from numerator, denominator, and pixelwise ratio
//	ROI set, log of background and noise levels
//  If the user selects multiple ROIs per cell, the ROIs will be renamed to include cell number and ROI number
//	subtracted images (if applicable)
// Theresa Swayne, Columbia University, 2022-2023

// TO USE: Open a multi-channel Z stack image. Run the macro.



// --- Setup ----
print("\\Clear"); // clears Log window
roiManager("reset");
run("Clear Results");


// ---- Get image information ----
id = getImageID();
title = getTitle();
dotIndex = indexOf(title, ".");
basename = substring(title, 0, dotIndex);
getDimensions(width, height, channels, slices, frames);
print("Processing",title, "with basename",basename);

// ---- Prepare images ----
run("Split Channels");
numImage = "C"+Channel_Num+"-"+title;
denomImage = "C"+Channel_Denom+"-"+title;
print("Numerator is",numImage);
if (Channel_Trans != 0) {
	transImage = "C"+Channel_Trans+"-"+title;
	}

// ---- Background and noise handling ---
// Background values are subtracted from each channel before initial segmentation
// Noise values are used to threshold each channel after segmentation, before ratioing
// Noise, if measured, is estimated as the standard deviation of the background

if (Background_Method == "Reference image") {
	numBG = 0;
	denomBG = 0;
	imgSubResults = subtractImage(Channel_Num, Channel_Denom, Channel_Trans);
	numNoise = imgSubResults[0];
	denomNoise = imgSubResults[1];
	print("Reference numerator channel "+Channel_Num+" background StdDev",numNoise);
	print("Reference denominator channel "+Channel_Denom+" background StdDev",denomNoise);
}

if (Background_Method == "Select an image area") { // interactive selection
	print("Measuring user-selected area");
	measBG = measureBackground(Channel_Num, Channel_Denom, Channel_Trans); // array containing preliminary values for background and noise

	numBG = measBG[0];
	denomBG = measBG[2];
	print("Measured numerator channel "+Channel_Num+" background mean", numBG);
	print("Measured denominator channel "+Channel_Denom+" background mean", denomBG);
	
	numNoise = measBG[1];
	denomNoise = measBG[3];
	print("Measured numerator channel "+Channel_Num+" background StdDev",numNoise);
	print("Measured denominator channel "+Channel_Denom+" background StdDev",denomNoise);

}
		
else if (Background_Method == "Fixed values") {
		Dialog.create("Enter Fixed Background Values");
		Dialog.addNumber("Numerator channel "+Channel_Num+" background", 0);
		Dialog.addNumber("Denominator channel "+Channel_Denom+" background", 0);
		Dialog.show();
		numBG = Dialog.getNumber();
		denomBG = Dialog.getNumber();
		print("Entered numerator channel "+Channel_Num+" background", numBG);
		print("Entered denominator channel "+Channel_Denom+" background", denomBG);
		
		Dialog.create("Enter Fixed Noise Values");
		Dialog.addNumber("Numerator channel "+Channel_Num+" noise", 1);
		Dialog.addNumber("Denominator channel "+Channel_Denom+" noise", 1);
		Dialog.show();
		numNoise = Dialog.getNumber();
		denomNoise = Dialog.getNumber();
		print("Entered numerator channel "+Channel_Num+" noise", numNoise);
		print("Entered denominator channel "+Channel_Denom+" noise", denomNoise);
}

else if (Background_Method == "None") {
		numBG = 0;
		denomBG = 0;
		print("No background was subtracted");
		numNoise = 1; // default noise value
		denomNoise = 1;
		print("No noise level was provided");
}

// subtract the previously determined background

selectWindow(numImage);
run("Select None");
run("Subtract...", "value="+numBG+" stack");

selectWindow(denomImage);
run("Select None");
run("Subtract...", "value="+denomBG+" stack");

// ---- Segmentation and ratioing ----

// threshold on the sum of the 2 images
imageCalculator("Add create 32-bit stack", numImage,denomImage);
selectWindow("Result of "+numImage);
rename("Sum");
setAutoThreshold(Thresh_Method+" dark stack");
print("Threshold used:",Thresh_Method);
//setOption("BlackBackground", false);
run("Convert to Mask", "method=&Thresh_Method background=Dark black");

// save the 8-bit mask, then divide by 255 to generate a 0,1 mask
selectWindow("Sum");
saveAs("Tiff", outputDir  + File.separator + basename + "_mask.tif");
run("Divide...", "value=255 stack");
rename("Mask");

// apply the mask to each channel by multiplication
// (a 32-bit result is required so we can change the background to NaN later)
// Apply an additional threshold based on the noise level to eliminate erroneous ratios caused by low signal

imageCalculator("Multiply create 32-bit stack", numImage, "Mask");
selectWindow("Result of "+numImage);
rename("Masked Num");
selectWindow("Masked Num");
setThreshold(numNoise, 1000000000000000000000000000000.0000); // this should ensure all mask pixels are selected 
run("NaN Background", "stack");

imageCalculator("Multiply create 32-bit stack", denomImage, "Mask");
selectWindow("Result of "+denomImage);
rename("Masked Denom");
selectWindow("Masked Denom");
setThreshold(denomNoise, 1000000000000000000000000000000.0000); // this should ensure all mask pixels are selected 
run("NaN Background", "stack");

// calculate the ratio image
imageCalculator("Divide create 32-bit stack", "Masked Num","Masked Denom");
selectWindow("Result of Masked Num");
rename("Ratio");

// ---- Select cells and measure ----

run("Set Measurements...", "area mean integrated display redirect=None decimal=4");
if (Channel_Trans != 0) {
	transImage = "C"+Channel_Trans+"-"+title;
	selectWindow(transImage);
	}
else {
	selectWindow(Sum);
	}
setTool("freehand");
middleSlice = round(slices/2);
Stack.setPosition(1,middleSlice,1);
run("Enhance Contrast", "saturated=0.35");
waitForUser("Mark cells", "Draw ROIs and add to the ROI manager (press T after each),\nor open an ROI set.\nThen click OK");

// rename ROIs for easier interpretation of results table

n = roiManager("count");
if (n == 0) {
	print("Analyzing entire image");
	run("Select All");
	roiManager("Add");
	roiManager("Select", 0);
    roiManager("Rename", "ROI_1");
	}
else if (n >= 1) {
	if (ROIsPerCell == 1) {
		for (i = 0; i < n; i++) {
		    roiManager("Select", i);
		    cellNum = i+1;
		    newName = "Cell_"+cellNum+"-ROI_1";
		    roiManager("Rename", newName);
		}
	}
	else if (ROIsPerCell != 1) {
		// check for errors in number of ROIs
		if (floor(n/ROIsPerCell) != n/ROIsPerCell) {
			print("Number of ROIs is not correct!");
			exit; 
		}
		// rename with cell number and ROI number
		numCells = n/ROIsPerCell;
		for (i = 0; i < numCells; i++) {
			cellNum = i+1;
			for (j = 0; j < ROIsPerCell; j++) {
				roiNum = j+1;
			    roiIndex = (i * ROIsPerCell) + j;
			    roiManager("Select", roiIndex);
			    newName = "Cell_"+cellNum+"-ROI_"+roiNum;
	    		roiManager("Rename", newName);
			}
		}	
	}
}
roiManager("deselect");  

//  save individual channel results

selectWindow("Masked Num");
rename(basename + "_C"+Channel_Num+"_Num"); // so the results will have the original filename attached
roiManager("deselect");
roiManager("Multi Measure");
selectWindow("Results");
saveAs("Results", outputDir  + File.separator + basename + "_NumResults.csv");
run("Clear Results");

selectWindow("Masked Denom");
rename(basename +  "_C"+Channel_Denom+"_Denom"); // so the results will have the original filename attached
roiManager("deselect");
roiManager("Multi Measure");
selectWindow("Results");
saveAs("Results", outputDir  + File.separator + basename + "_DenomResults.csv");
run("Clear Results");


// save ratio image results

selectWindow("Ratio");
rename(basename + "_ratio"); // so the results will have the original filename attached

roiManager("deselect");
roiManager("Multi Measure"); // user sees dialog to choose rows/columns for output


// ---- Save output files ----

selectWindow(basename + "_ratio");
saveAs("Tiff", outputDir  + File.separator + basename + "_ratio.tif");
roiManager("deselect");
roiManager("save", outputDir  + File.separator + basename + "_ROIs.zip");
selectWindow("Results");
saveAs("Results", outputDir  + File.separator + basename + "_Results.csv");
selectWindow("Log");
saveAs("text",outputDir  + File.separator + basename + "_Log.txt");

// ---- Clean up ----

close("*"); // image windows
selectWindow("Log");
run("Close");
roiManager("reset");
run("Clear Results");


// ---- Helper functions ----

function measureBackground(Num, Denom, Trans) { 
	// Measures background from a user-specified ROI
	// Returns the mean and standard deviation of stack background values
	//   (rounded to nearest integer) in numerator and denominator channels
	
	if (Trans != 0) {
		transImage = "C"+Trans+"-"+title;
		selectWindow(transImage);
	}
	else {
		selectWindow(numImage);
	}
	
	// get the ROI
	run("Set Measurements...", "mean standard redirect=None decimal=2");
	setTool("rectangle");
	waitForUser("Mark background", "Draw a background area, then click OK");
	
	// measure background in numerator channel
	selectWindow(numImage);
	run("Restore Selection"); // TODO: save this in the ROI manager
	run("Measure Stack...");
	numBGs = Table.getColumn("Mean");
	numSDs = Table.getColumn("StdDev");
	Array.getStatistics(numBGs, min, max, mean, stdDev);
	numMeasBackground = round(mean);
	Array.getStatistics(numSDs, min, max, mean, stdDev);
	numMeasNoise = round(mean);

	// measure background in denominator channel
	run("Clear Results");
	selectWindow(denomImage);
	run("Restore Selection"); // TODO: save this in the ROI manager
	run("Measure Stack...");
	denomBGs = Table.getColumn("Mean");
	denomSDs = Table.getColumn("StdDev");
	Array.getStatistics(denomBGs, min, max, mean, stdDev);
	denomMeasBackground = round(mean);
	Array.getStatistics(denomSDs, min, max, mean, stdDev);
	denomMeasNoise = round(mean);

	measBGResults = newArray(numMeasBackground, numMeasNoise, denomMeasBackground, denomMeasNoise);
	return measBGResults;
}
// measureBackground function

function subtractImage(Num, Denom, Trans) {

// Takes an input image and a user-supplied multichannel reference image. 
// Calculates the noise in the reference image as the standard deviation of the pixel values (by channel).
// Subtracts the average of the reference stack from the input stack (by channel).
// Returns the corrected channels and the SD values
	
	// get the reference image
	showMessage("On the next dialog please open the reference image file");
	refPath = File.openDialog("Select the reference image file");
  	open(refPath); // open the file
  	
  	//dir = File.getParent(rawPath);
	refName = File.getName(refPath);
	refDotIndex = indexOf(refName, ".");
	refBasename = substring(refName, 0, refDotIndex);
	
	// write info to the log
	print("Subtracting reference image",refName);
	
	selectWindow(refName);
	getDimensions(refwidth, refheight, refchannels, refslices, refframes);
	run("Split Channels");
	numRef = "C"+Num+"-"+refName;
	denomRef = "C"+Denom+"-"+refName;
	if (Trans != 0) {
		transRef = "C"+Channel_Trans+"-"+refName;
	}

	// measure the standard deviation of each fluorescence channel in the stack
	run("Set Measurements...", "mean standard redirect=None decimal=2");

	// measure SD in reference image numerator channel
	selectWindow(numRef);
	run("Select All");
	run("Measure Stack..."); 
	numRefSDs = Table.getColumn("StdDev"); // each row is one slice of the stack
	Array.getStatistics(numRefSDs, min, max, mean, stdDev);
	numRefNoise = round(mean); // average of all the slice standard deviations  

	// measure SD in reference image denominator channel
	run("Clear Results");
	selectWindow(denomRef);
	run("Select All");
	run("Measure Stack...");
	denomRefSDs = Table.getColumn("StdDev");
	Array.getStatistics(denomRefSDs, min, max, mean, stdDev);
	denomRefNoise = round(mean);
	
	// make an average intensity projection, or use the original image if just one slice
	if (refslices > 1) {
		selectWindow(numRef);
		run("Z Project...", "projection=[Average Intensity]");
		selectWindow("AVG_"+numRef);
		rename("Num_Reference");
		
		selectWindow(denomRef);
		run("Z Project...", "projection=[Average Intensity]");
		selectWindow("AVG_"+denomRef);
		rename("Denom_Reference");
	}
	else {
		selectWindow(numRef);
		rename("Num_Reference");
		selectWindow(denomRef);
		rename("Denom_Reference");
	}
	
	// subtract the averaged reference from the input image
	// save a copy, and then restore the image name
	
	imageCalculator("Subtract create 32-bit stack", numImage,"Num_Reference");
	selectWindow(numImage);
	close();
	selectWindow("Result of " + numImage);
	saveAs("Tiff", outputDir  + File.separator + numImage + "_Num_sub.tif");
	rename(numImage);
	
	imageCalculator("Subtract create 32-bit stack", denomImage,"Denom_Reference");
	selectWindow(denomImage);
	close();
	selectWindow("Result of "+denomImage);
	saveAs("Tiff", outputDir  + File.separator + denomImage + "_Denom_sub.tif");
	rename(denomImage);
	
	// clean up
	selectWindow("Num_Reference");
	close();
	selectWindow("Denom_Reference");
	close();
	run("Clear Results");
	
	imgSubResults = newArray(numRefNoise, denomRefNoise);
	return imgSubResults;
}
// subtractImage function

