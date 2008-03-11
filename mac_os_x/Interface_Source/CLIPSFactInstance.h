//
//  CLIPSFactInstance.h
//  CLIPS
//
//  Created by Gary Riley on 3/19/06.
//

#import <Cocoa/Cocoa.h>

@interface CLIPSFactInstance : NSObject 
  {
   NSString *name;
   NSArray *attributeValues;
   NSString *relationName;
   NSNumber *index;
   void *environment;
   void *scopeMap;
   void *theCPointer;
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- initWithFact: (struct fact *) theFact
  fromEnvironment: (void *) theEnvironment;

- initWithInstance: (struct instance *) theInstance
  fromEnvironment: (void *) theEnvironment;

- (BOOL)                         searchForString: (NSString *) theString;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                         setAttributeValues: (NSArray *) theAttributeValues;
- (NSArray *)                    attributeValues;

- (void)                         setRelationName: (NSString *) theRelationName;
- (NSString *)                   relationName;

- (void)                         setName: (NSString *) theName;
- (NSString *)                   name;

- (void)                         setIndex: (NSNumber *) theIndex;
- (NSNumber *)                   index;

- (void)                         setScopeMap: (void *) theValue;
- (void *)                       scopeMap;

- (void *)                       CPointer;

@end
