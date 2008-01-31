//
//  CLIPSTextDocument.m
//  CLIPSEditor
//
//  Created by Gary Riley on 2/14/06.
//  Copyright __MyCompanyName__ 2006 . All rights reserved.
//

#import "CLIPSTextDocument.h"
#import "CLIPSTerminalController.h"
#import "AppController.h"
#import "EnvController.h"
#import "CLIPSEnvironment.h"
#import "CLIPSTextView.h"

#import <CLIPS/clips.h>

@implementation CLIPSTextDocument

/*********/
/* init: */
/*********/
- (id) init
  {   
   self = [super init];
   if (self) 
     {          
      hiliteColor = [[NSDictionary alloc] initWithObjectsAndKeys: [NSColor selectedTextBackgroundColor], NSBackgroundColorAttributeName, nil];
     }

   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   NSLog(@"CLIPSTextDocument dealloc");
   
   if (terminalController != nil)
     {
      NSNotificationCenter  *nc;
      nc = [NSNotificationCenter defaultCenter];
      
      [nc removeObserver: self
          name:@"CLIPSTerminalClosed"
          object: terminalController];
     }
     
   [hiliteColor release];

   [string release];
   [terminalController release];
   [super dealloc];
  }

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   NSArrayController *theArrayController;
   NSMutableDictionary *bindingOptions = [NSMutableDictionary dictionary];
	
   /*=====================================================================*/
   /* The action "cog" menu is implemented by using a button with the cog */
   /* icon (which can't display a menu) that sends an action message to a */
   /* hidden NSPopButton that displays the menu. Normally a button takes  */
   /* effect when the mouse button is released, but in this case we want  */
   /* the menu to display immediately when the mouse button goes down.    */
   /*=====================================================================*/
   
   [popupActivator sendActionOn: NSLeftMouseDownMask];
         
   /*======================================================*/
   /* Places a few pixels of white space between the edges */
   /* of the window and the rectangle in which the text is */
   /* displayed in the terminal window.                    */
   /*======================================================*/
   
   NSSize theSize = { 5, 5 };
   [textView setTextContainerInset: theSize];
   
   /*=====================================================================*/
   /* Create the binding for the environment displayed in the popup menu. */
   /*=====================================================================*/
   
   [bindingOptions setObject:@"Unattached" forKey:@"NSNullPlaceholder"];
   [bindingOptions setObject: [NSNumber numberWithBool:YES] forKey:@"NSInsertsNullPlaceholder"];

   [environmentList bind: @"content" 
                    toObject: [[[NSApp delegate] envController] terminalArrayController]
                    withKeyPath: @"arrangedObjects"
                    options: bindingOptions];
                    
   /*=============================================================*/
   /* Locate and assign the application's environment controller. */
   /*=============================================================*/
  
   [self setEnvironmentController: [[NSApp delegate] envController]];
  
   /*====================================================================*/
   /* Determine the environment to which this window should be attached. */
   /*====================================================================*/
    
   theArrayController 
      = [[[NSApp delegate] envController] terminalArrayController];
      
   NSArray *theArray;
   NSUInteger theIndex;

   theArray = [theArrayController arrangedObjects];
   theIndex = [theArrayController selectionIndex]; 
   
   if (theIndex != NSNotFound)
     { [self setTerminalController: [theArray objectAtIndex: theIndex]]; }
   else if ([theArray count] != 0)
     { [self setTerminalController: [theArray objectAtIndex: 0]]; }
   else
     { [self setTerminalController: nil]; }
     
   /*==================================*/   
   /* Set up the horizontal scrollbar. */
   /*==================================*/   
     
   NSScrollView *textScrollView = [textView enclosingScrollView];

   [textScrollView setHasHorizontalScroller: YES];
   [textScrollView setAutoresizingMask: (NSViewWidthSizable | NSViewHeightSizable)];
   
   [textView setMaxSize: NSMakeSize(FLT_MAX, FLT_MAX)];
   [textView setHorizontallyResizable: YES];
   [textView setAutoresizingMask: (NSViewWidthSizable | NSViewHeightSizable)];
   
   [[textView textContainer] setContainerSize: NSMakeSize(FLT_MAX, FLT_MAX)];
   [[textView textContainer] setWidthTracksTextView: NO];
  }

/*************************************************************/    
/* windowDidBecomeMain: When the window is the main window,  */
/*   the action 'cog' menu needs its normal unpressed image. */
/**************************************************************/    
- (void) windowDidBecomeMain: (NSNotification *) aNotification
  {
   [popupActivator setImage:[NSImage imageNamed:@"CogUp.tiff"]];
  }
  
/**************************************************************/    
/* windowDidResignMain: When the window is no longer the main */
/*   window, the action 'cog' menu needs to be greyed.        */
/**************************************************************/    
- (void) windowDidResignMain: (NSNotification *) aNotification
  {
   [popupActivator setImage:[NSImage imageNamed:@"CogGreyed.tiff"]];
  }

/**************************/
/* resetBackgroundColour: */
/**************************/
- (void) resetBackgroundColour: (id) sender
  {
   [[textView layoutManager] removeTemporaryAttribute: NSBackgroundColorAttributeName forCharacterRange: NSRangeFromString(sender)];
  }

/*******************************/
/* textViewDidChangeSelection: */
/*******************************/
- (void) textViewDidChangeSelection: (NSNotification *) aNotification
  {
   NSRange selectionRange;
   unsigned int cursorLocation;
   unichar characterToCheck;
   unsigned short nestingDepth;
   NSString *theText = [textView string];
   NSUserDefaults *theValues;
   
   /*==============================================================*/
   /* Check the defaults to see if parentheses should be balanced. */
   /*==============================================================*/
   
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
    
   if (! [[theValues valueForKey: @"editorBalanceParens"] boolValue]) 
     { return; } 
   
   /*=========================================================*/
   /* Don't balance parentheses in response to a mouse click. */
   /*=========================================================*/

   if ([textView mouseDownDetected] == YES)
     { 
      [textView setMouseDownDetected: NO]; 
      return;
     }
     
   /*================================================*/
   /* Don't balance parentheses if there is no text. */
   /*================================================*/
   
   if ([theText length] == 0)
     { return; }

   /*=================================*/
   /* Retrieve the current selection. */
   /*=================================*/
          
   selectionRange = [textView selectedRange];
    
   /*======================*/
   /* Where is the cursor? */
   /*======================*/
    
   cursorLocation = selectionRange.location;
   
   if (cursorLocation == 0) return;
   
   cursorLocation--;
   
   /*===============================================*/
   /* What is the character at the cursor location? */
   /*===============================================*/
    
   characterToCheck = [theText characterAtIndex: cursorLocation];

   /*======================================*/
   /* We only balance a right parenthesis. */
   /*======================================*/
   
   if (characterToCheck != ')') return;
   
   /*======================================================================*/
   /* The nesting depth will start at zero. Each time a ')' is encountered */
   /* the nesting depth is incremented by one and each time a '(' is       */
   /* encountered the nesting depth is decremented by one. If a '(' is     */
   /* encountered when the nesting depth is zero (the starting value), the */
   /* matching parenthesis has been found.                                 */
   /*======================================================================*/
   
   nestingDepth = 0;

   /*==================================================*/
   /* Start looking for the matching left parenthesis. */
   /*==================================================*/
      
   while (cursorLocation--) 
     {
      characterToCheck = [theText characterAtIndex: cursorLocation];
      if (characterToCheck == '(') 
        {
         if (nestingDepth == 0) 
           {
            [[textView layoutManager] addTemporaryAttributes: hiliteColor forCharacterRange: NSMakeRange(cursorLocation, 1)];
		    [self performSelector: @selector(resetBackgroundColour:) withObject: NSStringFromRange(NSMakeRange(cursorLocation, 1)) afterDelay: 0.12];
		    return;
		   }
         else
		   { nestingDepth--; }
	    }
      else if (characterToCheck == ')') 
        { nestingDepth++; }
     }

   /*================================================*/
   /* Beep to indicate a matching ')' was not found. */
   /*================================================*/
   
   NSBeep();
  }
  
/*************************/
/* targetTerminalClosed: */
/*************************/
- (void) targetTerminalClosed: (NSNotification *) note
  {
   [self setTerminalController: nil];
  }
  
/***********/
/* string: */
/***********/
- (NSString *) string
  {
   return string;
  }

/**************/
/* setString: */
/**************/
- (void) setString: (NSString *) value
  {
   [value retain];
   [string release];
   string = value;
  }

/*****************/
/* updateString: */
/*****************/  
- (void) updateString
  {
   [self setString: [textView string]];
  }

/***************/
/* updateView: */
/***************/
- (void) updateView
  {
   [textView setString: [self string]];
  }

/******************/
/* windowNibName: */
/******************/
- (NSString *) windowNibName
  {
   return @"CLIPSTextDocument";
  }

/*******************************/
/* windowControllerDidLoadNib: */
/*******************************/
- (void) windowControllerDidLoadNib: (NSWindowController *) aController
  {
   [super windowControllerDidLoadNib: aController];
   
   if (! string)
     { [self setString:@""]; }
     
   [self updateView];
  }

/*****************************/
/* dataRepresentationOfType: */
/*****************************/  
- (NSData *) dataRepresentationOfType: (NSString *) aType
  {
   // Insert code here to write your document from the given data.  You can also choose to 
   // override -fileWrapperRepresentationOfType: or -writeToFile:ofType: instead.
    
   // For applications targeted for Tiger or later systems, you should use the new Tiger API -dataOfType:error:.  
   // In this case you can also choose to override -writeToURL:ofType:error:, -fileWrapperOfType:error:, or
   // -writeToURL:ofType:forSaveOperation:originalContentsURL:error: instead.

   [self updateString];
   
   return [string dataUsingEncoding: NSMacOSRomanStringEncoding allowLossyConversion:YES];
  }

/**********************************/
/* loadDataRepresentation:ofType: */
/**********************************/
- (BOOL) loadDataRepresentation: (NSData *) data 
         ofType: (NSString *) aType
  {
   NSString *aString;

   // Insert code here to read your document from the given data.  You can also choose 
   // to override -loadFileWrapperRepresentation:ofType: or -readFromFile:ofType: instead.
    
   // For applications targeted for Tiger or later systems, you should use the new Tiger API
   // readFromData:ofType:error:.  In this case you can also choose to override 
   // -readFromURL:ofType:error: or -readFromFileWrapper:ofType:error: instead.

   aString = [[NSString alloc] initWithData:data 
                               encoding: NSMacOSRomanStringEncoding]; 
                                         
   if (aString == nil)
     { return NO; }
     
   [self setString: aString];
   [aString release];
   [self updateView];
   
   return YES;
  }

/*********************/
/* validateMenuItem: */
/*********************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   /*=========================================================*/
   /* The "Load Selection" and "Batch Selection" commands are */
   /* only available if there is a selection in the window    */
   /* and the window is attached to a CLIPS terminal.         */
   /*=========================================================*/
   
   if (([menuItem action] == @selector(loadSelection:)) ||
       ([menuItem action] == @selector(batchSelection:)))
     {
      if (terminalController == nil) return NO;
      
      NSRange selectedRange = [textView selectedRange];
      
      if (selectedRange.length < 1) return NO;
      
      return YES;
     }
   
   /*================================================*/
   /* The "Load Buffer" command is only available if */
   /* the window is attached to a CLIPS terminal.    */
   /*================================================*/

   else if ([menuItem action] == @selector(loadBuffer:))
     {
      if (terminalController == nil) return NO;
      
      return YES;
     }

   return [super validateMenuItem: menuItem];
  }
  
/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

/*****************************************************/
/* loadSelection: Handles the Load Selection action. */
/*****************************************************/
- (IBAction) loadSelection: (id) sender
  {
   char *theString, *convString;
   NSString *entireString = [textView string];
   
   /*============================================*/
   /* There must be a selection for this action. */
   /*============================================*/

   NSRange selectedRange = [textView selectedRange];
   if (selectedRange.length < 1) return;
   
   /*=======================================================*/
   /* The window must also be attached to a CLIPS terminal. */
   /*=======================================================*/

   if (terminalController == nil) return;

   /*=============================*/
   /* Retrieve the selected text. */
   /*=============================*/
            
   NSString *theSelection = [entireString substringWithRange: selectedRange];
   
   /*==============================*/
   /* Move the text to a C string. */
   /*==============================*/
   
   void *theEnvironment = [[terminalController environment] environment];

   theString = (char *) gm2(theEnvironment,selectedRange.length + 1);
   convString = (char *) [theSelection UTF8String];
   strcpy(theString,convString);

   /*=================================================*/
   /* Bring the attached CLIPS terminal to the front. */
   /*=================================================*/
      
   [[terminalController window] makeKeyAndOrderFront: self];
   
   /*=====================*/
   /* Load the selection. */
   /*=====================*/
   
   EnvPrintRouter(theEnvironment,"stdout","Loading Selection...\n");
   FlushCommandString(theEnvironment);
   
   OpenTextSource(theEnvironment,"aecompiletext",theString,0,(unsigned) selectedRange.length); // TBD Make the name unique
   SetPrintWhileLoading(theEnvironment,TRUE);
   LoadConstructsFromLogicalName(theEnvironment,"aecompiletext");
   SetPrintWhileLoading(theEnvironment,FALSE);
   CloseStringSource(theEnvironment,"aecompiletext");
   
   rm(theEnvironment,theString,(unsigned int) selectedRange.length+1);
   PrintPrompt(theEnvironment);
  }
  
/*******************************************************/
/* batchSelection: Handles the Batch Selection action. */
/*******************************************************/
- (IBAction) batchSelection: (id) sender
  {
   char *theString, *convString;
   NSString *entireString = [textView string];
   
   /*============================================*/
   /* There must be a selection for this action. */
   /*============================================*/

   NSRange selectedRange = [textView selectedRange];
   if (selectedRange.length < 1) return;
   
   /*=======================================================*/
   /* The window must also be attached to a CLIPS terminal. */
   /*=======================================================*/

   if (terminalController == nil) return;

   /*=============================*/
   /* Retrieve the selected text. */
   /*=============================*/
            
   NSString *theSelection = [entireString substringWithRange: selectedRange];
   
   /*==============================*/
   /* Move the text to a C string. */
   /*==============================*/
   
   void *theEnvironment = [[terminalController environment] environment];

   theString = (char *) gm2(theEnvironment,selectedRange.length + 1);
   convString = (char *) [theSelection UTF8String];
   strcpy(theString,convString);

   /*=================================================*/
   /* Bring the attached CLIPS terminal to the front. */
   /*=================================================*/
      
   [[terminalController window] makeKeyAndOrderFront: self];

   /*==========================*/
   /* Batch the selected text. */
   /*==========================*/
      
   OpenStringBatch(theEnvironment,"batchtext",theString,FALSE); // TBD generate unique name
  }

/************************************************/
/* loadBuffer: Handles the Load Buffer command. */
/************************************************/
- (IBAction) loadBuffer: (id) sender
  {
   char *theString, *convString;
   NSString *entireString = [textView string];
      
   /*=======================================================*/
   /* The window must also be attached to a CLIPS terminal. */
   /*=======================================================*/

   if (terminalController == nil) return;
   
   /*==============================*/
   /* Move the text to a C string. */
   /*==============================*/
   
   void *theEnvironment = [[terminalController environment] environment];

   theString = (char *) gm2(theEnvironment,[entireString length] + 1);
   convString = (char *) [entireString UTF8String];
   strcpy(theString,convString);

   /*=================================================*/
   /* Bring the attached CLIPS terminal to the front. */
   /*=================================================*/
      
   [[terminalController window] makeKeyAndOrderFront: self];
   
   /*==================*/
   /* Load the buffer. */
   /*==================*/
   
   EnvPrintRouter(theEnvironment,"stdout","Loading Buffer...\n");
   FlushCommandString(theEnvironment);
   
   OpenTextSource(theEnvironment,"aecompiletext",theString,0,(unsigned) [entireString length]); // TBD Make the name unique
   SetPrintWhileLoading(theEnvironment,TRUE);
   LoadConstructsFromLogicalName(theEnvironment,"aecompiletext");
   SetPrintWhileLoading(theEnvironment,FALSE);
   CloseStringSource(theEnvironment,"aecompiletext");
   
   rm(theEnvironment,theString,(unsigned int) [entireString length]+1);
   PrintPrompt(theEnvironment);
  }

/*****************************************/
/* balance: Handles the Balance command. */
/*****************************************/
- (IBAction) balance: (id) sender
  {
   NSRange selectionRange;
   unsigned int cursorLocation;
   unsigned int leftMiddle, rightMiddle, textLength;
   unichar characterToCheck;
   int count, leftCount, rightCount;
   unsigned int i;
   BOOL endReached;
   NSString *theText = [textView string];
   
   /*====================================*/
   /* Can't balance if there is no text. */
   /*====================================*/
   
   if ([theText length] == 0)
     {
      NSBeep(); 
      return;
     }

   /*=================================*/
   /* Retrieve the current selection. */
   /*=================================*/
          
   selectionRange = [textView selectedRange];
    
   /*======================*/
   /* Where is the cursor? */
   /*======================*/
    
   cursorLocation = selectionRange.location;
   
   /*=============================================================*/
   /* Get information about the current selection to be balanced. */
   /*=============================================================*/

   leftMiddle = selectionRange.location;
   rightMiddle = selectionRange.location + selectionRange.length;
   textLength = [theText length];
 
   /*===================================*/
   /* If the selection is empty then... */
   /*===================================*/

   if (leftMiddle == rightMiddle)
     {
      /*============================================*/
      /* If '(' is to the right of the cursor, then */
      /* all balancing should occur to the right.   */
      /*============================================*/

      if ((leftMiddle < textLength) && ([theText characterAtIndex: leftMiddle] == '('))
        { 
         [self balanceIt: theText
               leftMiddle: leftMiddle
               rightMiddle: leftMiddle + 1
               leftCount: 1
               rightCount: 0
               textLength: textLength];
        }

      /*================================================*/
      /* Else if ')' is to the left of the cursor, then */
      /* all balancing should occur to the left.        */
      /*================================================*/

      else if ((leftMiddle > 0) ? ([theText characterAtIndex: (leftMiddle - 1)] == ')') : 0)
        { 
         if (leftMiddle < 2)
           {
            NSBeep();
            return;
           }
  
         [self balanceIt: theText
               leftMiddle: leftMiddle - 2
               rightMiddle: leftMiddle
               leftCount: 0
               rightCount: -1
               textLength: textLength];
        }

      /*====================================================*/
      /* Else balancing occurs to left and right of cursor. */
      /*====================================================*/

      else
        { 
         [self balanceIt: theText
               leftMiddle: leftMiddle - 1
               rightMiddle: rightMiddle
               leftCount: 0
               rightCount: 0
               textLength: textLength];
        }
     }
     
   /*===================================================*/
   /* Otherwise the selection is non-empty therefore... */
   /*===================================================*/

   else
     {
      /*===============================================*/
      /* Determine the number of right parentheses ')' */
      /* that need to be balanced from the left side.  */
      /*===============================================*/

      count = 0;
      leftCount = 0;

      for (i = leftMiddle ; i < rightMiddle ; i++)
        {
         characterToCheck = [theText characterAtIndex: i];

         if (characterToCheck == '(') count++;
         else if (characterToCheck == ')') count--;
         if (count < leftCount) leftCount = count;
        }

      /*===============================================*/
      /* Determine the number of left parentheses '('  */
      /* that need to be balanced from the right side. */
      /*===============================================*/

      count = 0;
      rightCount = 0;

      for (endReached = NO, i = rightMiddle - 1 ; ! endReached ;)
        {
         characterToCheck = [theText characterAtIndex: i];

         if (characterToCheck == '(') count++;
         else if (characterToCheck == ')') count--;
         if (count > rightCount) rightCount = count;
         
         if (i == leftMiddle) endReached = YES;
         else i--;
        }

      /*==============================================*/
      /* Balance to the left and right of the cursor. */
      /*==============================================*/
      
      [self balanceIt: theText
            leftMiddle: ((leftMiddle == 0) ? 0 : leftMiddle - 1)
            rightMiddle: rightMiddle
            leftCount: leftCount
            rightCount: rightCount
            textLength: textLength];
     }
  }

/********************************************************/
/* balanceIt: Balances a selection of text by extending */
/* it to the left and right until the number of left    */
/* and right parentheses is balanced.                   */
/********************************************************/
- (void) balanceIt: (NSString *) theText
         leftMiddle: (unsigned int) leftMiddle
         rightMiddle: (unsigned int) rightMiddle
         leftCount: (int) leftCount
         rightCount: (int) rightCount
         textLength: (unsigned int) textLength
  {
   unichar characterToCheck;
   BOOL beginningReached = NO;
   
   /*==========================================================*/
   /* Balance the left side of the text by moving left and up. */
   /*==========================================================*/

   while (leftCount <= 0)
     {
      if (beginningReached)
        {
         NSBeep();
         return;
        }
        
      characterToCheck = [theText characterAtIndex: leftMiddle];
      
      if (characterToCheck == '(') leftCount++;
      else if (characterToCheck == ')') leftCount--;
      
      if (leftCount <= 0)
        {
         if (leftMiddle > 0) leftMiddle--;
         else beginningReached = YES;
        }
     }
     
   /*==============================================================*/
   /* Balance the right side of the text by moving right and down. */
   /*==============================================================*/

   while (rightCount >= 0)
     {
      if (rightMiddle >= textLength)
        {
         NSBeep();
         return;
        }

      characterToCheck = [theText characterAtIndex: rightMiddle];

      if (characterToCheck == '(') rightCount++;
      else if (characterToCheck == ')') rightCount--;

      rightMiddle++;  
     }
     
   /*=============================================*/
   /* Set the current selection to balanced text. */
   /*=============================================*/
   
   NSRange theRange = { leftMiddle, rightMiddle - leftMiddle };
   
   [textView setSelectedRange: theRange];   

   /*=====================================*/
   /* Make sure the selection is visible. */
   /*=====================================*/

   [textView scrollRangeToVisible: theRange]; 
  }

/*****************************************/
/* comment: Handles the Comment command. */
/*****************************************/
- (IBAction) comment: (id) sender
  {
   NSRange selectionRange, rangeOfLine, tempRange;
   unsigned int numberOfLines = 0, i;
   unsigned int tempLocation, maxSelectedRange;
   unsigned locationOfFirstLine, locationOfLastLine;
   NSString *theText = [textView string];
      
   /*==============================================*/
   /* Retrieve the current selection and determine */
   /* the location of the beginning and end.       */
   /*==============================================*/
          
   selectionRange = [textView selectedRange];
   tempLocation = selectionRange.location;
   maxSelectedRange = NSMaxRange(selectionRange);
   
   /*=====================================*/
   /* Determine the beginning location of */
   /* the first line to be commented.     */
   /*=====================================*/
   
   tempRange.location = tempLocation;
   tempRange.length = 0;
   locationOfFirstLine = [theText lineRangeForRange: tempRange].location;
  
   /*=============================================*/
   /* Count the number of lines in the selection. */
   /*=============================================*/

   do
     {
      tempRange.location = tempLocation;
      tempRange.length = 0;
	  tempLocation = NSMaxRange([theText lineRangeForRange: tempRange]);
	  numberOfLines++;
	 } 
   while (tempLocation < maxSelectedRange);

   /*=========================================================*/
   /* Remember the location of the end of the last line, then */
   /* reset the temporary location back to the first line.    */
   /*=========================================================*/
       
   locationOfLastLine = tempLocation;
   tempLocation = selectionRange.location;

   /*=======================================================*/
   /* Iterate over each line adding a ';' at the beginning. */
   /*=======================================================*/
   
   for (i = 0; i < numberOfLines; i++)
     {
      /*==============================================*/
      /* Determine the beginning and end of the line. */
      /*==============================================*/
      
      tempRange.location = tempLocation;
      tempRange.length = 0;
	  rangeOfLine = [theText lineRangeForRange: tempRange];

      /*======================================*/
      /* Comment character should be inserted */
      /* at the beginning of the line.        */
      /*======================================*/
      
      tempRange.location = rangeOfLine.location;
      tempRange.length = 0;
      
      /*==============================================================*/
      /* Add the comment character. Using shouldChangeTextInRange and */
      /* didChangeText are necessary to support undoing this action.  */
      /*==============================================================*/
      
	  if ([textView shouldChangeTextInRange: tempRange replacementString: @";"]) 
		{ 		 
         [textView replaceCharactersInRange: tempRange withString: @";"];
		 [textView didChangeText];
        }
        
      /*========================*/
      /* Move to the next line. */
      /*========================*/

      tempRange.location = tempLocation;
      tempRange.length = 0;
	  tempLocation = NSMaxRange([theText lineRangeForRange: tempRange]);
     }

   /*=======================================================*/
   /* If there was a selection when the comment command was */
   /* invoked, extend it to completely include all of the   */
   /* lines in which comment characters were added.         */
   /*=======================================================*/
   
   if (selectionRange.length > 0)
  	 { 
      tempRange.location = locationOfFirstLine;
      tempRange.length = (locationOfLastLine - locationOfFirstLine) + numberOfLines;
      [textView setSelectedRange: tempRange];
     }
  }
  
/*********************************************/
/* uncomment: Handles the Uncomment command. */
/*********************************************/
- (IBAction) uncomment: (id) sender
  {
   NSRange selectionRange, rangeOfLine, tempRange;
   unsigned int numberOfLines = 0, i;
   unsigned int numberOfCharactersDeleted = 0;
   unsigned int tempLocation, maxSelectedRange;
   unsigned locationOfFirstLine, locationOfLastLine;
   NSString *theText = [textView string];
      
   /*==============================================*/
   /* Retrieve the current selection and determine */
   /* the location of the beginning and end.       */
   /*==============================================*/
          
   selectionRange = [textView selectedRange];
   tempLocation = selectionRange.location;
   maxSelectedRange = NSMaxRange(selectionRange);
   
   /*=====================================*/
   /* Determine the beginning location of */
   /* the first line to be commented.     */
   /*=====================================*/
   
   tempRange.location = tempLocation;
   tempRange.length = 0;
   locationOfFirstLine = [theText lineRangeForRange: tempRange].location;
  
   /*=============================================*/
   /* Count the number of lines in the selection. */
   /*=============================================*/

   do
     {
      tempRange.location = tempLocation;
      tempRange.length = 0;
	  tempLocation = NSMaxRange([theText lineRangeForRange: tempRange]);
	  numberOfLines++;
	 } 
   while (tempLocation < maxSelectedRange);

   /*=========================================================*/
   /* Remember the location of the end of the last line, then */
   /* reset the temporary location back to the first line.    */
   /*=========================================================*/
       
   locationOfLastLine = tempLocation;
   tempLocation = selectionRange.location;

   /*=======================================================*/
   /* Iterate over each line adding a ';' at the beginning. */
   /*=======================================================*/
   
   for (i = 0; i < numberOfLines; i++)
     {
      /*==============================================*/
      /* Determine the beginning and end of the line. */
      /*==============================================*/
      
      tempRange.location = tempLocation;
      tempRange.length = 0;
	  rangeOfLine = [theText lineRangeForRange: tempRange];

      if ([theText characterAtIndex: rangeOfLine.location] == ';')
        {
         /*=====================================*/
         /* Comment character should be removed */
         /* from the beginning of the line.     */
         /*=====================================*/
      
         tempRange.location = rangeOfLine.location;
         tempRange.length = 1;
         
         /*=================================================================*/
         /* Remove the comment character. Using shouldChangeTextInRange and */
         /* didChangeText are necessary to support undoing this action.     */
         /*=================================================================*/
      
	     if ([textView shouldChangeTextInRange: tempRange replacementString: @""]) 
		   { 		 
            [textView replaceCharactersInRange: tempRange withString: @""];
		    [textView didChangeText];
            numberOfCharactersDeleted++;
           }
        } 
        
      /*========================*/
      /* Move to the next line. */
      /*========================*/

      tempRange.location = tempLocation;
      tempRange.length = 0;
	  tempLocation = NSMaxRange([theText lineRangeForRange: tempRange]);
     }

   /*===============================================*/
   /* Beep if there weren't any characters deleted. */
   /*===============================================*/
   
   if (numberOfCharactersDeleted == 0)
     {
      NSBeep();
      return;
     }
     
   /*=======================================================*/
   /* If there was a selection when the comment command was */
   /* invoked, extend it to completely include all of the   */
   /* lines in which comment characters were added.         */
   /*=======================================================*/
   
   if (selectionRange.length > 0)
  	 { 
      tempRange.location = locationOfFirstLine;
      tempRange.length = (locationOfLastLine - locationOfFirstLine) - numberOfCharactersDeleted;
      [textView setSelectedRange: tempRange];
     }
  }
              
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void) setTerminalController: (CLIPSTerminalController *) theController
  {
   NSNotificationCenter  *nc;
   nc = [NSNotificationCenter defaultCenter];
   
   [theController retain];

   if (terminalController != nil)
     {
      [nc removeObserver: self
          name:@"CLIPSTerminalClosed"
          object: terminalController];
     }

   /*===========================================================*/
   /* If the editor window is associated with a CLIPS terminal, */
   /* we want to be notified if the terminal window closes.     */
   /*===========================================================*/

   if (theController != nil)
     {
      [nc addObserver: self 
          selector: @selector(targetTerminalClosed:)
          name: @"CLIPSTerminalClosed"
          object: theController];
     }
     
   [terminalController release];
   terminalController = theController;
  }

- (CLIPSTerminalController *) terminalController
  {
   return terminalController;
  }

- (void) setEnvironmentController: (EnvController *) theController
  {
   [theController retain];
   [environmentController release];
   environmentController = theController;
  }

- (EnvController *) environmentController
  {
   return environmentController;
  }

@end
