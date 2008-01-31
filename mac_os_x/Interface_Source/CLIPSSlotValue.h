//
//  CLIPSSlotValue.h
//  CLIPS
//
//  Created by Gary Riley on 3/19/06.
//  Copyright 2006 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface CLIPSSlotValue : NSObject
   {
    NSString *slotName;
    NSString *slotValue;
    BOOL defaultedValue;
   }
   
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- initWithSlotName: (NSString *) theSlotName andSlotValue: (NSString *) theSlotValue;
   
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                         setSlotName: (NSString *) theSlotName;
- (NSString *)                   slotName;

- (void)                         setSlotValue: (NSString *) theSlotValue;
- (NSString *)                   slotValue;

- (void)                         setDefaultedValue: (BOOL) theDefaultedValue;
- (BOOL)                         defaultedValue;

@end
