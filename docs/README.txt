Image2XML

The purpose of this application is to take form data and 
convert it into data usable XML data.  Most OCR applications 
simply gives you blobs of text.  It becomes difficult how to 
identify what the different text is supposed to be.  Given an 
xml file (there is no DTD for this xml file.. yes.. I know,  
I should create one) that contains certain tags that define what 
parts of these images are,  a description of the image that 
created this xml file (with such attributes with dpi resolution,  
width, height), how the form is offset (because we never get those 
scanned images text in the EXACT location), and instructions on 
how to process the image before the OCR occurs (such as stripping 
colors,resizing to the model image, deskewing, etc).  

Version History: 
09/11/2009 - V 0.9.9.X: 
	Added Image Resolution warning
	Added the ability for the use to specify (and write) the DPI to the active Image
	
09/11/2009 - V 0.9.8.X: 
	Added Application integrated licensing
	
08/27/2009 - V 0.9.7.X: 
	Added Undo Capability
	Fixed copy bug where Data Type was not bieng copied over
	Fixed an undo bug where type was not bieng restored
	Fixed Save Dialog opening as "Open Dialog"
	Fixed multi-monitor issues 
	Fixed issue where region would not delete via DEL key until one was deleted by mouse click
	Fixed memory leak caused when Region Delete was "Un-did"
	
03/05/2009 - V 0.9.6.X: 
	Completed Region Editor UI
	Completed Job Creation (and batch job gen) capability
	Created Image processing and OCR Engine plugin Architecture
	Created OCR plugins for TOCR
	Created Image Processing plugins for MCM, FreeImage and Graphics32
	Dummied out Document correction code for now
	Dummied out file rename and grouping for now
	
08/01/2008 - V 0.9.1.006:    
	Added 3rd Party Exception Handler
	Significant work on Region Editor
	Changed template file extention to .i2x
	
07/01/2008 - V 0.9.1.005: 
	Added the ability to Dilate text
	Added ability to Sharpen
	Added the ability to rescale image
	Added image processing commands specifically if image is of lower resolution to model image.

06/01/2008 - V 0.9.1.004: 
	Fixed messy sprawling debugging messages. 
	Added debugging flag on command line 
	Added more debugging messages that such as data and offset pixels 
	Tweaked Despeckle functionality to use different filter to clean off offending pixel specks. 

05/01/2008 - V 0.9.1.003: 
	Added Despeckle to image processing routines to remove pixel noises. Added GoogleOCR in order to obtain a 5th 
	opinion on important scanned data (such has key and/or group fields). 

04/01/2008 - V 0.9.1.002:
	Added the ability to deskew. 
	Added the image processing instructions that allow rescaling and Deskewing. 
	Fixed glaring memory leak that caused exception after image 3. 
	Improved character offset algorithm. 
	Improved applications ability to handle images of different resolutions. 

03/01/2008 - V 0.9.1.001: 
	completed conversion from older Delphi 6 to Delphi 2007, Updated TWAIN and image processing libraries (g32). 
	Added FreeImage image processing library. 
	Added ImageProcessing GUI Utility. 
	first successful run of TEST A of OCR. 

