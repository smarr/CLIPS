//
//  CLIPSTerminalView.h
//  CLIPS
//
//  Created by Gary Riley on 2/23/06.
//  Copyright 2006 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class CLIPSEnvironment;

@interface CLIPSTerminalView : NSTextView
  {
   IBOutlet CLIPSEnvironment *environment;
   IBOutlet id dialogWindow;
   BOOL routerPrint;
   //BOOL waitingForChar;
   int charFound;
   char *inputBuffer;
   size_t inputPos;
   NSConditionLock *inputCharLock;
   NSDictionary *hiliteColor;
  }
  
- (void) resetBackgroundColour: (id) sender;
- (unsigned int) print: (NSString *) theString;
- (void) clearTerminal;
- (void) balanceParentheses;
- (int) waitForChar;
- (BOOL) readStringFromPasteboard: (NSPasteboard *) pb;

@end
