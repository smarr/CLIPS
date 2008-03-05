//
//  CLIPSFactInstance.m
//  CLIPS
//
//  Created by Gary Riley on 3/19/06.
//  Copyright 2006 __MyCompanyName__. All rights reserved.
//

#import "CLIPSFactInstance.h"
#import "CLIPS/clips.h"

@implementation CLIPSFactInstance

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*****************/
/* initWithFact: */
/*****************/
- initWithFact: (struct fact *) theFact 
  fromEnvironment: (void *) theEnvironment
  {
   if (self = [super init])
     {
      int i;
      DATA_OBJECT slotNames;
      DATA_OBJECT slotValue;
      DATA_OBJECT defaultValue;
      NSMutableArray *tempArray;
      NSMutableDictionary *theMD;
      NSNumber *theNumber;
      BITMAP_HN *theScopeMap;

      [self setValue: [NSString stringWithFormat:@"%s", ValueToString(FactRelation(theFact))] forKey: @"relationName"]; 
      
      [self setValue: [NSString stringWithFormat:@"%lld", EnvFactIndex(theEnvironment,theFact)] forKey: @"name"]; 

      [self setValue: [NSNumber numberWithLongLong: EnvFactIndex(theEnvironment,theFact)] forKey: @"index"]; 

      theCPointer = theFact; 
            
      EnvFactSlotNames(theEnvironment,theFact,&slotNames);
      
      environment = theEnvironment;

      theScopeMap = CreateDeftemplateScopeMap(theEnvironment,theFact->whichDeftemplate);
      
      scopeMap = malloc(theScopeMap->size);
      
      memcpy(scopeMap,theScopeMap->contents,theScopeMap->size);
      
      DecrementBitMapCount(theEnvironment,theScopeMap);

      tempArray = [NSMutableArray arrayWithCapacity: (unsigned) GetDOLength(slotNames)];

      for (i = 1; i <= GetDOLength(slotNames); i++)
        {
         char *theCSlotName = ValueToString(GetMFValue(GetValue(slotNames),i));
         NSString *theSlotName = [NSString stringWithFormat:@"%s", theCSlotName];
         
         FactSlotValue(theEnvironment,theFact,ValueToString(GetMFValue(GetValue(slotNames),i)),&slotValue);

         NSString *theSlotValue = [NSString stringWithFormat:@"%s", DataObjectToString(theEnvironment,&slotValue)];

         /* Only static defaults will be excluded from display. */

         if (EnvDeftemplateSlotDefaultP(theEnvironment,EnvFactDeftemplate(theEnvironment,theFact),theCSlotName) == STATIC_DEFAULT)
           {
            EnvDeftemplateSlotDefaultValue(theEnvironment,
                                           EnvFactDeftemplate(theEnvironment,theFact),
                                           theCSlotName,&defaultValue);
                                           
            if (DOsEqual(&slotValue,&defaultValue))
              { theNumber = [NSNumber numberWithBool: NO]; }
            else
              { theNumber = [NSNumber numberWithBool: YES]; }
           }
         else
           { theNumber = [NSNumber numberWithBool: YES]; }
         
         theMD = [NSMutableDictionary dictionaryWithObjectsAndKeys: theSlotName, @"slotName", theSlotValue, @"slotValue",
                                       theNumber, @"slotDefault", nil];
          
         [tempArray addObject: theMD];
        }
      
      [self setValue: tempArray forKey: @"attributeValues"];
      
      /* EnvIncrementFactCount(theEnvironment,theFact); */
     }
     
   return self;
  }

/*********************/
/* initWithInstance: */
/*********************/
- initWithInstance: (struct instance *) theInstance 
  fromEnvironment: (void *) theEnvironment
  {
   if (self = [super init])
     {
      int i;
      DATA_OBJECT slotNames;
      DATA_OBJECT slotValue;
      DATA_OBJECT defaultValue;
      NSMutableArray *tempArray;
      NSMutableDictionary *theMD;
      NSNumber *theNumber;
      BITMAP_HN *theScopeMap;
      void *theClass;

      theClass = EnvGetInstanceClass(theEnvironment,theInstance);
      [self setValue: [NSString stringWithFormat:@"%s", EnvGetDefclassName(theEnvironment,theClass)] forKey: @"relationName"]; 
      
      [self setValue: [NSString stringWithFormat:@"%s", EnvGetInstanceName(theEnvironment,theInstance)] forKey: @"name"]; 

      /*====================================================*/
      /* An index of -1 indicates that this is an instance. */
      /*====================================================*/
      
      [self setValue: [NSNumber numberWithLongLong: -1] forKey: @"index"]; 
      
      theCPointer = theInstance; 
            
      EnvClassSlots(theEnvironment,theClass,&slotNames,TRUE);
      
      environment = theEnvironment;

      theScopeMap = CreateClassScopeMap(theEnvironment,theClass);
      
      scopeMap = malloc(theScopeMap->size);
      
      memcpy(scopeMap,theScopeMap->contents,theScopeMap->size);
      
      DecrementBitMapCount(theEnvironment,theScopeMap);

      tempArray = [NSMutableArray arrayWithCapacity: (unsigned) GetDOLength(slotNames)];

      for (i = 1; i <= GetDOLength(slotNames); i++)
        {
         char *theCSlotName = ValueToString(GetMFValue(GetValue(slotNames),i));
         NSString *theSlotName = [NSString stringWithFormat:@"%s", theCSlotName];
         
         EnvDirectGetSlot(theEnvironment,theInstance,ValueToString(GetMFValue(GetValue(slotNames),i)),&slotValue);

         NSString *theSlotValue = [NSString stringWithFormat:@"%s", DataObjectToString(theEnvironment,&slotValue)];

         /* Only static defaults will be excluded from display. */
 
         if (EnvSlotDefaultP(theEnvironment,theClass,theCSlotName) == STATIC_DEFAULT)
           {
            EnvSlotDefaultValue(theEnvironment,theClass,theCSlotName,&defaultValue);
                                            
            if (DOsEqual(&slotValue,&defaultValue))
              { theNumber = [NSNumber numberWithBool: NO]; }
            else
              { theNumber = [NSNumber numberWithBool: YES]; }
           }
         else
           { theNumber = [NSNumber numberWithBool: YES]; }
         
         theMD = [NSMutableDictionary dictionaryWithObjectsAndKeys: theSlotName, @"slotName", theSlotValue, @"slotValue",
                                       theNumber, @"slotDefault", nil];
          
         [tempArray addObject: theMD];
        }
      
      [self setValue: tempArray forKey: @"attributeValues"];

      /* EnvIncrementInstanceCount(theEnvironment,theInstance); */
     }
     
   return self;
  }

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super init];
   if (self) 
     { scopeMap = NULL; }
   
   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   /* NSLog(@"CLIPSFactInstance dealloc %@", self); */
   [relationName release];
   [name release];
   [attributeValues release];
   [index release];
   
   if (scopeMap != NULL)
     { free(scopeMap); }
/*
   if ([index longLongValue] == -1)
     { EnvDecrementInstanceCount(environment,theCPointer); }
   else
     { EnvDecrementFactCount(environment,theCPointer); }
*/  
   [super dealloc];
  }

/********************/
/* searchForString: */
/********************/
- (BOOL) searchForString: (NSString *) searchString
  {
   NSRange range;
   NSString *tempString;
   
   if (relationName != nil)
     {
      range = [relationName rangeOfString: searchString
                            options: NSCaseInsensitiveSearch];
   
      if (range.location != NSNotFound) 
        { return YES; }
     }
    
   if (index != nil)
     {
      tempString = [NSString stringWithFormat:@"f-%lld",[index longLongValue]];
      
      range = [tempString rangeOfString: searchString
                          options: NSCaseInsensitiveSearch];
   
      if (range.location != NSNotFound) 
        { return YES; }
     }

   if (attributeValues != nil)
     {
      NSEnumerator *enumerator = [attributeValues objectEnumerator];
      NSDictionary *item;
        
      while (item = [enumerator nextObject])
        {
         tempString = [NSString stringWithFormat:@"%@ %@ %@",relationName,[item valueForKey: @"slotName"],[item valueForKey: @"slotValue"]];

         range = [tempString rangeOfString: searchString
                             options: NSCaseInsensitiveSearch];
   
         if (range.location != NSNotFound) 
           { return YES; }
        }
     }
     
   return NO;
  }
  
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/***********************/
/* setAttributeValues: */
/***********************/
- (void) setAttributeValues: (NSArray *) theAttributeValues
  {
   [theAttributeValues retain];
   [attributeValues release];
   attributeValues = theAttributeValues;
  }
  
/********************/
/* attributeValues: */
/********************/
- (NSArray *) attributeValues
  {
   return attributeValues;
  }

/***********************/
/* setRelationName: */
/***********************/
- (void) setRelationName: (NSString *) theRelationName
  {
   [theRelationName retain];
   [relationName release];
   relationName = theRelationName;
  }
  
/********************/
/* relationName: */
/********************/
- (NSString *) relationName
  {
   return relationName;
  }

/***********************/
/* setName: */
/***********************/
- (void) setName: (NSString *) theName
  {
   [theName retain];
   [name release];
   name = theName;
  }
  
/********************/
/* name: */
/********************/
- (NSString *) name
  {
   return name;
  }

/***********************/
/* setIndex: */
/***********************/
- (void) setIndex: (NSNumber *) theIndex
  {
   [theIndex retain];
   [index release];
   index = theIndex;
  }
  
/********************/
/* index: */
/********************/
- (NSNumber *) index
  {
   return index;
  }
  
/*****************/
/* setScopeMap: */
/*****************/
- (void) setScopeMap: (void *) theValue
  {
   scopeMap = theValue;
  }

/**************/
/* scopeMap: */
/**************/
- (void *) scopeMap
  {
   return scopeMap;
  }

/**************/
/* CPointer: */
/**************/
- (void *) CPointer
  {
   return theCPointer;
  }
  
@end
