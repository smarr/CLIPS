//
//  CLIPSSlotValue.m
//  CLIPS
//
//  Created by Gary Riley on 3/19/06.
//  Copyright 2006 __MyCompanyName__. All rights reserved.
//

#import "CLIPSSlotValue.h"

@implementation CLIPSSlotValue

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/**********************************/
/* initWithSlotName:andSlotValue: */
/**********************************/
- initWithSlotName: (NSString *) theSlotName
  andSlotValue: (NSString *) theSlotValue
  {
   if (self = [super init])
     {
      [self setSlotName: theSlotName];
      [self setSlotValue: theSlotValue];
     }
     
   return self;
  }
   
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/****************/
/* setSlotName: */
/****************/
- (void) setSlotName: (NSString *) theSlotName
  {
   [theSlotName retain];
   [slotName release];
   slotName = theSlotName;
  }

/*************/
/* slotName: */
/*************/
- (NSString *) slotName
  {
   return slotName;
  }

/*****************/
/* setSlotValue: */
/*****************/
- (void) setSlotValue: (NSString *) theSlotValue
  {
   [theSlotValue retain];
   [slotValue release];
   slotValue = theSlotValue;
  }

/**************/
/* slotValue: */
/**************/
- (NSString *) slotValue
  {
   return slotValue;
  }

/**********************/
/* setDefaultedValue: */
/**********************/
- (void) setDefaultedValue: (BOOL) theDefaultedValue
  {
   defaultedValue = theDefaultedValue;
  }

/*******************/
/* defaultedValue: */
/*******************/
- (BOOL) defaultedValue
  {
   return defaultedValue;
  }

@end
