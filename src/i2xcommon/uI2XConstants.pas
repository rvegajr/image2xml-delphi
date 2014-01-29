unit uI2XConstants;

interface

const
  IMGPROC_PARM_SEP = ';';
  IMGPROC_VALUE_SEP = '=';
  IMGPROC_PLUGIN_SEP = ':';
  IMGPROC_INST_SEP = '|';
  QUOTE = '''';

  MAX_BUFFER=10240;
  MININT = -2147483648;
  //This will adjust the padding for what the app will consider characters to be on the samel ine
  //  this will give some sort of buffer in those cases where a deskew did not perform perfectly
  SAME_LINE_PIXEL_TRESHOLD = 4;
  //When calculating space between characters,  we want to calculate if we should pad the
  //  the characters between words,  adjusting this will adjust the final
  //  average letter size.  If the space between is characters is more than the
  //   the calculated average char size,  then a space will be added.  This will
  //   adjust the final avg char size.
  AVG_CHAR_SIZE_PADDING = -2; 

  IMAGE_MEMMAP_INIT_SIZE=99999999;
  OCR_MEMMAP_INIT_SIZE=99999999;
  IMAGE_MEMMAP_HEADER = 'BITMAP_H';
  OCR_MEMMAP_HEADER = 'OCR_P';
  SLICE_HEADER = 'SLICE_';
  ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  NUMBERS = '0123456789';
  DT_n_CHARS_ALLOWED = NUMBERS + '-.$';
  DT_dt_CHARS_ALLOWED = NUMBERS + '-/:.';

  CRLF = #13#10;
  STANDARD_REGION_PREFIX = '__';

  DELETE_REGION_TITLE = 'Delete Selected Region';
  DELETE_REGION_TEXT = 'Are you sure you wish to delete this region?';
  DELETE_REGION_MULTI_TEXT = 'Deleting this parent region will delete all the child regions also,  are you sure you wish to do this?';
  DELETE_REGION_ERROR_EXISTS = 'An Element with this ID already exists.';
  DELETE_ALL_INST = 'Are you sure you wish to delete every instruction in this list?  This cannot be undone.';

  ANALYZE_DOCUMENT = 'In order to set up document offset correction,  Image2XML would like to anaylze your document.' + #13 + 'This may take a minute.  Click yes to allow me to begin to analyze,  or click no to cancel.';

  DELETE_JOB = 'This will remove all the data in the outout subdirectory and delete the job file with all its history.' + #13 + 'Are you sure you wish to delete this job?  This action cannot be undone.';
  CLEAR_JOB = 'This will clear the current job data?  This action cannot be undone.' + #13 + 'Are you sure you wish to clear this job data?';

  REGION_RENAME_CAPTION = 'Region Rename';
  REGION_RENAME_PROMPT = 'Please edit/update the name of this region.';
  CREATE_TEMPLATE_NAME_CAPTION = 'New Template Name';
  CREATE_TEMPLATE_NAME_PROMPT = 'Please type in a template for your new template.  Please use numbers and letters only:';
  IMAGE_INST_DEFAULT = 'Select an image instruction below.  When selected, a brief description of it''s effect on the active image will display.';

  TEMPLATE_MESSAGES_DIFF = 'The Active Image is different than Template Image.  Would you like to save the template with this the active image instead?' + #13 +
          'Click YES if you wish to use the image currently displayed as the new template image,' + #13 +
          'Click NO to keep the template''s current image,' + #13 +
          'Click CANCEL to stop the this Save';

  NEW_REGION_DEFAULT_NAME = 'New_Region';
  NEW_REGION_COPY_NAME = 'Copy';
  CLIPBOARD_REGION_ID='REGION_ADDRESS:';
  IGNORE='<IGNORE>';
  DPM_TO_DPI = 39.370079;

  //ImageList.StateIndex=0 has some bugs, so we add one dummy image to position 0
  cFlatUnCheck = 1;
  cFlatChecked = 2;
  cFlatRadioUnCheck = 3;
  cFlatRadioChecked = 4;

  I2X_DIR_QUAL = 'i2x\';
  I2XFULL_DIR_QUAL = 'Image2XML\';

  IMGPROC_CACHE = '~IMGPROCCACHE.BMP';
  ACQ_IMAGE = '~SCANNED.BMP';

  TWAIN_REG_KEY = 'DEFAULT_TWAIN_DEVICE';

  ERROR_OK = 0;
  ERROR_PARM_FILE_INVALID = 9901;
  ERROR_PARM_MISSING = 9902;
  ERROR_PARM_INVALID = 9903;
  ERROR_IMAGE_PROC_FAILED = 1000;
  ERROR_I2X_OCR_FAILED = 2000;

  ERROR_IMAGE_THREAD_FAILED = 1100;
  ERROR_OCR_THREAD_FAILED = 2100;

type
  TDebugLevel = ( dbNone = 0, dbDetailed, dbVerbose );
  TOCRTest = ( ocrtNormalWhole = 0, ocrtNormalSliced, ocrtInvertedWhole, ocrtInvertedSliced );
  TOCRRunType = ( otNormal = 0, otAnalysis = 1 );
  TOCRTestSet = set of TOCRTest;
  TMemoryMapID = string;

implementation
end.
