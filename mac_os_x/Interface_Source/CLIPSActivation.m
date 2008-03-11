//
//  CLIPSActivation.m
//  CLIPS
//
//  Created by Gary Riley on 3/13/06.
//

#import "CLIPSActivation.h"


@implementation CLIPSActivation


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super init];
   if (self) 
     {    
     }
   
   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   [salience release];
   [ruleName release];
   [bindings release];
   [super dealloc];
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/****************/
/* setSalience: */
/****************/
- (void) setSalience: (NSNumber *) theSalience
  {
   [theSalience retain];
   [salience release];
   salience = theSalience;
  }

/*************/
/* salience: */
/*************/
- (NSNumber *) salience
  {
   return salience;
  }

/****************/
/* setRuleName: */
/****************/
- (void) setRuleName: (NSString *) theRuleName
  {
   [theRuleName retain];
   [ruleName release];
   ruleName = theRuleName;
  }

/*************/
/* ruleName: */
/*************/
- (NSString *) ruleName
  {
   return ruleName;
  }

/****************/
/* setBindings: */
/****************/
- (void) setBindings: (NSString *) theBindings
  {
   [theBindings retain];
   [bindings release];
   bindings = theBindings;
  }

/*************/
/* bindings: */
/*************/
- (NSString *) bindings
  {
   return bindings;
  }

/******************/
/* setActivation: */
/******************/
- (void) setActivation: (void *) theActivation
  {
   activation = theActivation;
  }

/***************/
/* activation: */
/***************/
- (void *) activation
  {
   return activation;
  }

@end
