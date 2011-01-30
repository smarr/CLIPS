//
//  ModuleArrayController.m
//  CLIPS
//
//  Created by Gary Riley on 3/21/07.
//

#import "ModuleArrayController.h"

#import "CLIPSFactInstance.h"

#import <core/clips.h>

@implementation ModuleArrayController

/*******************/
/* arrangeObjects: */
/*******************/

- (NSArray *) arrangeObjects: (NSArray *) objects
  {
   NSArray *returnObjects = objects;

   /*===================================================*/
   /* Create an array for storing the filtered objects. */
   /*===================================================*/
    
   NSMutableArray *filteredObjects;
   filteredObjects = [NSMutableArray arrayWithCapacity: [objects count]];
  
   /*=============================================*/
   /* If we don't have a valid module index, just */
   /* use the superclass to arrange the objects.  */
   /*=============================================*/
   
   if (moduleIndex < 0)
     {
      returnObjects = filteredObjects;
      return ([super arrangeObjects: returnObjects]);
     }
     
   /*===============================================*/
   /* Step through the objects using an enumerator. */
   /*===============================================*/
   
   NSEnumerator *enumerator = [objects objectEnumerator];
   id item;
        
   while (item = [enumerator nextObject])
     {
      CLIPSFactInstance *theFI;
      void *theMap;
      
      theFI = (CLIPSFactInstance *) item;

      theMap = [theFI scopeMap];
    
      /*==================================================*/
      /* The fact must be visible to the selected module. */
      /*==================================================*/

      if (! TestBitMap(((char *) theMap),moduleIndex)) 
        { continue; }

      if ((searchString != nil) &&      
          (! [theFI searchForString: searchString]))
        { continue; }
         
      [filteredObjects addObject: item]; 
     }

   returnObjects = filteredObjects;

   /*==============================================*/
   /* Have the superclass also arrange the objects */
   /* object to pick up NSTableView sorting.       */
   /*==============================================*/
   
   return ([super arrangeObjects: returnObjects]);
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

/****************/  
/* search: */
/****************/  
- (IBAction) search: (id) sender
 {
  /* NSLog(@"ModuleArrayController search = %@",[sender stringValue]); */

  [self setSearchString: [sender stringValue]];
  [self rearrangeObjects];
 } 

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void) setSearchString: (NSString *) string
  {
   [searchString release];

   if ([string length] == 0)
     { searchString = nil; } 
   else
     { searchString = [string copy]; }
  }

/****************/
/* moduleIndex: */
/****************/
- (int) moduleIndex
  {
   return moduleIndex;
  }

/*******************/
/* setModuleIndex: */
/*******************/
- (void) setModuleIndex: (int) theModule
  {
   /* NSLog(@"ModuleArrayController setModuleIndex = %d",theModule); */
   moduleIndex = theModule;
   [self rearrangeObjects];
  }
  
@end
