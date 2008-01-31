#import "CLIPSTextMenu.h"

@implementation CLIPSTextMenu

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   NSLog(@"CLIPSTextMenu awakeFromNib");
   id mainMenu;
   NSArray *items;
   NSMutableArray *array;
   int i, count;
   
   /*=======================================*/
   /* Retrieve the application's main menu. */
   /*=======================================*/
   
   mainMenu = [[NSApplication sharedApplication] mainMenu];

   /*=================================================*/
   /* Retrieve the array containing the menu's items. */
   /*=================================================*/
   
   items = [mainMenu itemArray];
   
   /*=====================================*/
   /* Determine the number of menu items. */
   /*=====================================*/
   
   count = [items count];

   /*=============================================*/
   /* Create an array to store the modified menu. */
   /*=============================================*/
      
   array = [NSMutableArray arrayWithCapacity: [items count]];
   
   for (i = 0; i < count; i++)
     { [array addObject: [[items objectAtIndex: i] title]]; }
      
    i = [array indexOfObject: @"Window"];
    if (i > -1)
      {
       NSMenuItem* textItem = [[NSMenuItem alloc] initWithTitle: @"Text" action: nil keyEquivalent: @""];
       [textMenu setTitle: @"Text"];
       [textItem setSubmenu: textMenu];
       [mainMenu insertItem: textItem atIndex: i];
       [textItem setTitle: @"Text"];
      }
  }

@end
