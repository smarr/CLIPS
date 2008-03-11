//
//  CLIPSTerminalView.m
//  CLIPS
//
//  Created by Gary Riley on 2/23/06.
//

#import "CLIPSEnvironment.h"
#import "CLIPSTerminalView.h"
#import "CLIPSTerminalController.h"
#import <CLIPS/clips.h>

#define DOES_NOT_CHAR  0
#define NEEDS_CHAR     1 
#define HAS_CHAR       2

@implementation CLIPSTerminalView
 
/********************************/
/* initWithFrame:textContainer: */
/********************************/
- (id) initWithFrame: (NSRect) frameRect textContainer: (NSTextContainer *) aTextContainer
  {
   NSLog(@"CLIPSTerminalView initWithFrame:textContainer:");
      
   self = [super initWithFrame: frameRect textContainer: aTextContainer];
   
   if (self)
     {
     }
     
   return self;
  }
   
/******************/
/* initWithFrame: */
/******************/
- (id) initWithFrame: (NSRect) frameRect
  {
   NSLog(@"CLIPSTerminalView initWithFrame");
   self = [super initWithFrame: frameRect];
   
   if (self)
     {
     }
     
   return self;
  }
  
/*********/
/* init: */
/*********/
- (id) init
  {
   NSLog(@"CLIPSTerminalView init");
   
   self = [super init];
   
   if (self) 
     {    
      inputBuffer = NULL;
     }
   
   return self;
  }
  
/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   NSLog(@"CLIPSTerminalView dealloc");
     
   if (inputBuffer != NULL)
     { free(inputBuffer); }
     
   [inputCharLock release];
   [hiliteColor release];
   
   [super dealloc];
  }
   
/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   NSLog(@"CLIPSTerminalView awakeFromNib");
   inputCharLock = [[NSConditionLock alloc] initWithCondition: DOES_NOT_CHAR];
   hiliteColor = [[NSDictionary alloc] initWithObjectsAndKeys: [NSColor selectedTextBackgroundColor], NSBackgroundColorAttributeName, nil];
  }

/*************************/
/* performDragOperation: */
/*************************/
- (BOOL) performDragOperation: (id <NSDraggingInfo>) sender
  {
   /*============================================*/
   /* Dropping text in the terminal window isn't */
   /* isn't supported when CLIPS is executing    */
   /* a command and expecting input.             */
   /*============================================*/
   
   if ([inputCharLock condition] == NEEDS_CHAR)
     { return NO; }

   /*===================================*/  
   /* The insertion point is always the */
   /* bottom of the terminal window.    */
   /*===================================*/  
    
   NSRange theRange = { [[super string] length], 0 };
   [super setSelectedRange: theRange];   

   NSPasteboard *pb = [sender draggingPasteboard];
   if (! [self readStringFromPasteboard: pb])
     { return NO; }
   
   return YES;
  }
  
/**********/
/* paste: */
/**********/
- (IBAction) paste: (id) sender
  {
   //NSLog(@"CLIPSTerminalView paste:");
   NSRange theRange = { [[super string] length], 0 };
   [super setSelectedRange: theRange];   

   NSPasteboard *pb = [NSPasteboard generalPasteboard];
   
   [self readStringFromPasteboard: pb];
      
   //[super paste: sender];
  }

      
/*****************************/
/* readStringFromPasteboard: */
/*****************************/
- (BOOL) readStringFromPasteboard: (NSPasteboard *) pb
  {
   NSString *theText;
   NSString *type;
   
   // Is there a string on the pasteboard?
   
   type = [pb availableTypeFromArray: [NSArray arrayWithObject: NSStringPboardType]];
   
   if (type)
     {
      // Read the string from the pasteboard
      theText = [pb stringForType: NSStringPboardType];
      
      [self insertText: theText];
      
      [[dialogWindow windowController] setScrollToEnd: YES];
      
      return YES;
     }
     
   return NO;
  }
  
/**********/    
/* print: */
/**********/    
- (unsigned int) print: (NSString *) theString
  {
   NSRange theRange = { [[super string] length], 0 };

   NSMutableAttributedString *terminalStorage = [self textStorage];

   [super setSelectedRange: theRange];   

   /*==================================================================*/  
   /* If the terminal window doesn't contain any text, use insertText  */
   /* to add the text, otherwise use replaceCharactersInRange. If the  */
   /* terminal window doesn't contain text, it uses the wrong font it  */
   /* the latter method is used, so this test is a workaround for that */
   /* problem. Another possible workaround is to use an attributed     */
   /* string with the font attribute added.                            */
   /*==================================================================*/  
          
   if ([terminalStorage isEqualToAttributedString: [[NSAttributedString new] initWithString: @""]])
     { [super insertText: theString];  }
   else
     {
      [terminalStorage replaceCharactersInRange: theRange withString: theString];
      [self didChangeText];
     }
   
   /*================================================*/
   /* Let the terminal controller know that text has */
   /* been added and it should be made visible.      */
   /*================================================*/
   
   [[dialogWindow windowController] setScrollToEnd: YES];
   
   return [[super string] length];
  }

/******************/    
/* clearTerminal: */
/******************/    
- (void) clearTerminal
  {
   NSMutableAttributedString *terminalStorage = [self textStorage];

   [terminalStorage setAttributedString: [[NSAttributedString new] initWithString: @""]];
   [self didChangeText];
   
   /*================================================*/
   /* Let the terminal controller know that text has */
   /* been changed and it should be made visible.    */
   /*================================================*/
   
   [[dialogWindow windowController] setScrollToEnd: YES];
  }

/***********************/    
/* balanceParentheses: */
/***********************/    
- (void) balanceParentheses
  {
   NSRange selectionRange;
   unsigned int cursorLocation;
   int commandLength;
   unichar characterToCheck;
   unsigned short nestingDepth;
   NSString *theText = [super string];
   
   /*================================================*/
   /* Don't balance parentheses if there is no text. */
   /*================================================*/
   
   if ([theText length] == 0)
     { return; }
     
   /*=======================================================*/
   /* Don't balance the parentheses if there is no command. */
   /*=======================================================*/
   
   commandLength = RouterData([environment environment])->CommandBufferInputCount;
   if (commandLength <= 0) 
     { return; }

   /*=================================*/
   /* Retrieve the current selection. */
   /*=================================*/
          
   selectionRange = [super selectedRange];
   
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
      
   while (cursorLocation-- && commandLength--) 
     {
      characterToCheck = [theText characterAtIndex: cursorLocation];
      if (characterToCheck == '(') 
        {
         if (nestingDepth == 0) 
           {
            [[super layoutManager] addTemporaryAttributes: hiliteColor forCharacterRange: NSMakeRange(cursorLocation, 1)];
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

/***************/
/* insertText: */
/***************/
- (void) insertText: (NSString *) theText
  {
   //NSLog(@"CLIPSTerminalView insertText");
   void *theEnvironment = [environment environment];
   
   /*================================================*/
   /* If the environment is executing a command (the */
   /* executionLock has been set), then don't allow  */
   /* any input unless the executing environment is  */
   /* requesting input.                              */
   /*================================================*/
   
   if ([[environment executionLock] tryLock])
     { [[environment executionLock] unlock]; }
   else if ([inputCharLock condition] != NEEDS_CHAR)
     { return; }
     
   /*================================*/
   /* Dump any text remaining in the */ 
   /* output buffer to the terminal. */
   /*================================*/
     
   [[dialogWindow windowController] dumpOutputBuffer];

   /*======================================================*/
   /* Move the selection point to the end of the terminal. */ 
   /*======================================================*/
   
   NSRange theRange = { [[super string] length], 0 };
   [super setSelectedRange: theRange];   

   if ([inputCharLock condition] == NEEDS_CHAR)
     {
      char *str; 
      size_t len;
      str = (char *) [theText UTF8String];
      charFound = str[0];
      [super insertText: theText];
      
      if ((len = strlen(str)) >  1)
        {
         /* TBD Concatenate to existing buffer */
         if (inputBuffer != NULL)
           { free(inputBuffer); }
           
         inputBuffer = (char *) malloc(len);
         strcpy(inputBuffer,&str[1]);
         inputPos = 0;
        }
      
      [inputCharLock lock];
      [inputCharLock unlockWithCondition: HAS_CHAR];
      return;
     }
          
   if (! routerPrint)
     { 
      char *str; 
      
      /*================================================================*/
      /* The string returned by UTF8String is placed in the autorelease */
      /* pool, so there is no need to release the str in this function. */
      /*================================================================*/
      
      str = (char *) [theText UTF8String];
      
      AppendCommandString(theEnvironment,str);
     }
     
   [super insertText: theText];
   
   if (! routerPrint)
     {
      [self balanceParentheses];
      
      if (CommandCompleteAndNotEmpty(theEnvironment))
        {
         // TBD At this point lock command input.
        }
     }
  }
  
/*******************/
/* deleteBackward: */
/*******************/
- (void) deleteBackward: (id) sender
  {
   //NSLog(@"CLIPSTerminalView deleteBackward");

   /*===================================*/
   /* Move the cursor to the end of the */
   /* last line in the terminal.        */
   /*===================================*/
      
   NSRange theRange = { [[super string] length], 0 };

   [super setSelectedRange: theRange];   

   /*==========================================*/
   /* If the input buffer is empty, then there */
   /* are no characters to delete.             */
   /*==========================================*/
   
   if (RouterData([environment environment])->CommandBufferInputCount <= 0) 
     { return; }

   /*==========================================================*/
   /* Is a running CLIPS function or program (such as the read */
   /* function) waiting for character input from stdin.        */
   /*==========================================================*/
   
   if ([inputCharLock condition] == NEEDS_CHAR)
     {
      [super deleteBackward: sender];
      charFound = '\b';
      [inputCharLock lock];
      [inputCharLock unlockWithCondition: HAS_CHAR];
      return;
     }
   
   /*==============================================*/
   /* Process the backspace against the unexecuted */
   /* command being entered at the command prompt. */
   /*==============================================*/
    
   ExpandCommandString([environment environment],'\b');
      
   [super deleteBackward: sender];
   
   [self balanceParentheses];
  }
  
/***********************************************************/
/* waitForChar: Waits for the user to enter a character in */
/*   response to CLIPS router getc request from stdin.     */
/***********************************************************/
- (int) waitForChar
  {
   NSRange theRange = { [[super string] length], 0 };
   charFound = -1;

   /*============================================*/
   /* Bring the bottom of the terminal into view */
   /* so we can see the text being entered.      */
   /*============================================*/
   
   [super scrollRangeToVisible: theRange]; 

   /*=====================================*/
   /* Check for queued input from a UTF-8 */
   /* character or a pasted string.       */
   /*=====================================*/
   
   if (inputBuffer != NULL)
     {
      charFound = inputBuffer[inputPos];
      if (inputBuffer[++inputPos] == '\0')
        {
         free(inputBuffer);
         inputBuffer = NULL;
        }
        
      return charFound;
     }
     
   /*==============================================*/
   /* Change the condition on the lock to indicate */
   /* that an input character is needed.           */
   /*==============================================*/
            
   [inputCharLock lock];
   [inputCharLock unlockWithCondition: NEEDS_CHAR];

   /*=========================================================*/
   /* Block this thread until an input character is detected. */ 
   /*=========================================================*/
     
   [inputCharLock lockWhenCondition: HAS_CHAR];
   [inputCharLock unlockWithCondition: DOES_NOT_CHAR];
   
   /*=============================*/
   /* Return the character found. */
   /*=============================*/
   
   return charFound;
  }

/**************************/
/* resetBackgroundColour: */
/**************************/
- (void) resetBackgroundColour: (id) sender
  {
   [[super layoutManager] removeTemporaryAttribute: NSBackgroundColorAttributeName forCharacterRange: NSRangeFromString(sender)];
  }
  
/*%%%%%%%%%%%%%%%%%%%%*/
/* Overridden Methods */
/*%%%%%%%%%%%%%%%%%%%%*/

/*************************/
/* performKeyEquivalent: */
/*************************/
/*
- (BOOL) performKeyEquivalent: (NSEvent *) theEvent
  {
   if ([inputCharLock condition] == NEEDS_CHAR)
     { return NO; }
        
   if (([theEvent modifierFlags] & NSCommandKeyMask) &&
       [[theEvent characters] isEqual: @"."])
     {
      void *theEnvironment = [environment environment];
      
      if ([theEvent modifierFlags] & NSShiftKeyMask)
        {
         SetHaltExecution(theEnvironment,TRUE);
         CloseAllBatchSources(theEnvironment);
         NSLog(@"Command-Shift-Period");
        }
      else
        {
#if DEFRULE_CONSTRUCT
         if (EngineData(theEnvironment)->ExecutingRule != NULL)
           { EngineData(theEnvironment)->HaltRules = TRUE; }
         else
#endif
           {
            SetHaltExecution(theEnvironment,TRUE);
            CloseAllBatchSources(theEnvironment);
           }
           
         NSLog(@"Command-Period");
        }
       
      waitingForChar = FALSE;
      charFound = -1;
      [NSApp stopModal];

      return YES;
     }
     
   return NO;
  }
*/
/************************************************/
/* validateMenuItem: The cut and delete actions */
/*   are not allowed in the terminal window.    */
/************************************************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   NSString *selectorString;
   selectorString = NSStringFromSelector([menuItem action]);
   
   /*====================================*/
   /* The cut and delete actions are not */
   /* allowed in the terminal window.    */
   /*====================================*/
   
   if (([menuItem action] == @selector(cut:)) ||
       ([menuItem action] == @selector(delete:)))
     { return NO; }
     
   /*====================================*/
   /* Otherwise, allow the superclass to */
   /* determine the valid menu items.    */
   /*====================================*/
   
   return [super validateMenuItem: menuItem];
  }
  
/*
- (void) keyDown: (NSEvent *) event
  {
   NSLog(@"CLIPSTerminalView keyDown");
   [super keyDown: event];
  }
*/
/*
    - (void)keyDown:(NSEvent *)event {
    NSString *keys = [event charactersIgnoringModifiers];
    wormHeading = kGameHeadingStraight;
    if (keys && [keys length] > 0) {
        unichar c = [keys characterAtIndex:0];
        if (c == NSLeftArrowFunctionKey) {
            wormHeading = kGameHeadingLeft;
        } else if (c == NSRightArrowFunctionKey) {
            wormHeading = kGameHeadingRight;
        }
    }
    
}*/
/*
- (void) refreshBindings
  {
   NSLog(@"CLIPSTerminalView refreshBindings");
   
   [self bind:@"fontName" 
         toObject: [NSUserDefaultsController sharedUserDefaultsController]
         withKeyPath:@"values.editorTextFontName" 
         options: nil];

   [self bind:@"fontSize" 
         toObject: [NSUserDefaultsController sharedUserDefaultsController]
         withKeyPath:@"values.editorTextFontSize" 
         options: nil];
  }

*/
@end
