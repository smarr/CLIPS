//
//  CLIPSFocus.m
//  CLIPS
//
//  Created by Gary Riley on 3/13/06.
//

#import "CLIPSFocus.h"

@implementation CLIPSFocus

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
   [moduleName release];
   [agenda release];
   [super dealloc];
  }

/****************/
/* description: */
/****************/
- (NSString *) description
  {
   return moduleName;
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/******************/
/* setModuleName: */
/******************/
- (void) setModuleName: (NSString *) theModuleName
  {
   [theModuleName retain];
   [moduleName release];
   moduleName = theModuleName;
  }

/***************/
/* moduleName: */
/***************/
- (NSString *) moduleName
  {
   return moduleName;
  }

/************************/
/* setAgenda: */
/************************/
- (void) setAgenda: (NSArray *) theAgenda
  {
   [theAgenda retain];
   [agenda release];
   agenda = theAgenda;
  }

/*********************/
/* agenda: */
/*********************/
- (NSArray *) agenda
  {
   return agenda;
  }

@end
