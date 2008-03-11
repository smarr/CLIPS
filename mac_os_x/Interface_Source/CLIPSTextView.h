//
//  CLIPSTextView.h
//  CLIPS
//
//  Created by Gary Riley on 3/7/06.
//

#import <Cocoa/Cocoa.h>

@interface CLIPSTextView : NSTextView
  {
   BOOL mouseDownDetected;
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)               setMouseDownDetected: (BOOL) theValue;
- (BOOL)               mouseDownDetected;

@end
