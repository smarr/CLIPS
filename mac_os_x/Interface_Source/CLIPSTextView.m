//
//  CLIPSTextView.m
//  CLIPS
//
//  Created by Gary Riley on 3/7/06.
//

#import "CLIPSTextView.h"

@implementation CLIPSTextView

/**************/
/* mouseDown: */
/**************/
- (void) mouseDown: (NSEvent *) theEvent
  {
   mouseDownDetected = YES;

   [super mouseDown: theEvent];
  }

/******************/
/* insertNewLine: */
/******************/
- (void) insertNewline: (id) sender
  {
   NSString *previousLineWhitespaceString;
   NSString *previousLineText;
   NSScanner *previousLineScanner;
   NSRange tempRange;
   NSString *theText;

   /*============================*/
   /* First call the superclass. */
   /*============================*/
   
   [super insertNewline:sender];

   /*=============================================*/
   /* Should new lines be automatically indented? */
   /*=============================================*/
   
   //if (! [defaults boolForKey:@"IndentNewLinesAutomatically"]) return; 

   /*==============================================*/
   /* Set a range to the end of the previous line. */
   /*==============================================*/
   
   tempRange.location = [self selectedRange].location - 1;
   tempRange.length = 0;
   
   /*======================================================*/
   /* Use that range to get the text of the previous line. */
   /*======================================================*/
   
   theText = [self string];
   previousLineText = [theText substringWithRange: [theText lineRangeForRange: tempRange]];
   
   /*==================================================================*/
   /* Create a scanner initialized with the text of the previous line. */
   /*==================================================================*/
   
   previousLineScanner = [[NSScanner alloc] initWithString: previousLineText];
   
   /*===========================================*/
   /* The default set of characters to skip is  */
   /* the whitespace and newline character set. */
   /*===========================================*/
   
   [previousLineScanner setCharactersToBeSkipped: nil];		
   
   if ([previousLineScanner scanCharactersFromSet: [NSCharacterSet whitespaceCharacterSet] 
                            intoString: &previousLineWhitespaceString])
     { [self insertText: previousLineWhitespaceString]; }

   /*======================*/
   /* Release the scanner. */
   /*======================*/
   
   [previousLineScanner release];
  }
  
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void) setMouseDownDetected: (BOOL) theValue
  {
   mouseDownDetected = theValue;
  }

- (BOOL) mouseDownDetected
  {
   return mouseDownDetected;
  }

@end
