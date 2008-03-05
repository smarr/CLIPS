#import "EnvController.h"
#import "CLIPSTerminalController.h"
#import "CLIPSAgendaController.h"
#import "CLIPSFactController.h"
#import "CLIPSInstanceController.h"
#import "CLIPSConstructInspectorController.h"
#import "GenericController.h"

@implementation EnvController
  
/*********/
/* init: */
/*********/
- (id) init
  {
   NSLog(@"EnvController init");

   self = [super init];
   
   if (self)
     {
      fileOpenLock = [[NSLock alloc] init];
     }
     
   return self;
  }
  
/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   NSLog(@"EnvController dealloc");

   [fileOpenLock release];

   /*=====================================*/
   /* Call the superclass dealloc method. */
   /*=====================================*/
   
   [super dealloc];
  }

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
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
       NSMenuItem* debugItem = [[NSMenuItem alloc] initWithTitle: @"Debug" action: nil keyEquivalent: @""];
       [debugMenu setTitle: @"Debug"];
       [debugItem setSubmenu: debugMenu];
       [mainMenu insertItem: debugItem atIndex: i];
       [debugItem setTitle: @"Debug"];

       NSMenuItem* environmentItem = [[NSMenuItem alloc] initWithTitle: @"Environment" action: nil keyEquivalent: @""];
       [environmentMenu setTitle: @"Environment"];
       [environmentItem setSubmenu: environmentMenu];
       [mainMenu insertItem: environmentItem atIndex: i];
       [environmentItem setTitle: @"Environment"];
      }
  }

/*****************/
/* setTerminal: */
/*****************/
- (void) setTerminal: (CLIPSTerminalController *) theController
  {
   NSArray *theArray;
   NSUInteger theIndex;
   CLIPSEnvironment *theEnvironment = [theController environment];

   theArray = [terminalArrayController arrangedObjects];
   theIndex = [theArray indexOfObject: theController]; 
   
   if (theIndex == NSNotFound)
     { [terminalArrayController setSelectionIndexes: [NSIndexSet indexSet]];  }
   else
     { [terminalArrayController setSelectionIndex: theIndex]; }

   theArray = [environmentArrayController arrangedObjects];
   theIndex = [theArray indexOfObject: theEnvironment]; 
   
   if (theIndex == NSNotFound)
     { [environmentArrayController setSelectionIndexes: [NSIndexSet indexSet]];  }
   else
     { [environmentArrayController setSelectionIndex: theIndex]; }
  }

/********************************/
/* showConstructInspectorPanel: */
/********************************/
- (IBAction) showConstructInspectorPanel: (id) sender
  {
   if (! constructInspectorController)
     { constructInspectorController = [[CLIPSConstructInspectorController alloc] init]; }
    
   [constructInspectorController showPanel];
  }

/*******************/
/* newEnvironment: */
/*******************/
- (IBAction) newEnvironment: (id) sender
  {
   CLIPSTerminalController *theController;

   theController = [[CLIPSTerminalController alloc] init];  

   [theController setEnvController: self]; 

   [terminalArrayController addObject: theController];

   [environmentArrayController addObject: [theController environment]];
   
   /*======================================*/
   /* Bring the new terminal to the front. */
   /*======================================*/
   
   [theController showWindow: self];
   [[theController window] makeKeyAndOrderFront: self];
  }
  
/*******************/
/* newDebugAgenda: */
/*******************/
- (IBAction) newDebugAgenda: (id) sender
  {
   CLIPSAgendaController *theController;
      
   theController = [[CLIPSAgendaController alloc] init]; 
        
   [theController showWindow: self];
  }

/************************/
/* newDebugFactBrowser: */
/************************/
- (IBAction) newDebugFactBrowser: (id) sender
  {
   CLIPSFactController *theController;
      
   theController = [[CLIPSFactController alloc] init]; 
        
   [theController showWindow: self];
  }

/****************************/
/* newDebugInstanceBrowser: */
/****************************/
- (IBAction) newDebugInstanceBrowser: (id) sender
  {
   CLIPSInstanceController *theController;
      
   theController = [[CLIPSInstanceController alloc] init]; 
        
   [theController showWindow: self];
  }

/**************************/
/* newDebugGenericInspector: */
/**************************/
- (IBAction) newDebugGenericInspector: (id) sender
  {
   GenericController *theController;
      
   theController = [[GenericController alloc] init]; 
        
   [theController showWindow: self];
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
 
/****************************/
/* fileOpenLock: */
/****************************/
- (NSLock *) fileOpenLock
  {
   return fileOpenLock;
  }

/*******************************/
/* setTerminalArrayController: */
/*******************************/
- (void) setTerminalArrayController: (NSArrayController *) theArrayController
  {
   [theArrayController retain];
   [terminalArrayController release];
   terminalArrayController = theArrayController;
  }

/****************************/
/* terminalArrayController: */
/****************************/
- (NSArrayController *) terminalArrayController
  {
   return terminalArrayController;
  }

/**********************************/
/* setEnvironmentArrayController: */
/**********************************/
- (void) setEnvironmentArrayController: (NSArrayController *) theArrayController
  {
   [theArrayController retain];
   [environmentArrayController release];
   environmentArrayController = theArrayController;
  }

/*******************************/
/* environmentArrayController: */
/*******************************/
- (NSArrayController *) environmentArrayController
  {
   return environmentArrayController;
  }

/*******************/
/* terminalExists: */
/*******************/
- (BOOL) terminalExists
  {
   if ([[terminalArrayController arrangedObjects] count] > 0)
     {   
      return YES;
     }
   
   return NO;
  }

/**********************/
/* setTerminalExists: */
/**********************/
- (void) setTerminalExists: (BOOL) value
  {
  }

/**********************************************************/
/* environmentExists: Returns YES if any environments are */
/*   in the environment array controller, otherwise NO.   */
/**********************************************************/
- (BOOL) environmentExists
  {
   if ([[environmentArrayController arrangedObjects] count] > 0)
     { return YES; }
   
   return NO;
  }

/**********************/
/* setEnvironmentExists: */
/**********************/
- (void) setEnvironmentExists: (BOOL) value
  {
  }

/*******************************/
/* setConstructInspectorText: */
/*******************************/
- (void) setConstructInspectorText: (NSString *) theConstructInspectorText
  {
   [theConstructInspectorText retain];
   [constructInspectorText release];
   constructInspectorText = theConstructInspectorText;
  }

/****************************/
/* constructInspectorText: */
/****************************/
- (NSString *) constructInspectorText
  {
   return constructInspectorText;
  }

/*%%%%%%%%%%%%%%%%*/
/* Unused Methods */
/*%%%%%%%%%%%%%%%%*/

/*
- (void) setEnvironmentController: (NSObjectController *) theEnvironmentController;
  {
   [theEnvironmentController retain];
   [environmentController release];
   environmentController = theEnvironmentController;
  }

- (NSObjectController *) environmentController;
  {
   return environmentController;
  }
*/  

/*******************/
/* terminalIsMain: */
/*******************/
/*
- (BOOL) terminalIsMain
  {
   if ([environmentArrayController selectionIndex] == NSNotFound)
     { return NO; }
     
   return YES;
  }
*/  
/**********************/
/* setTerminalIsMain: */
/**********************/
/*
- (void) setTerminalIsMain: (BOOL) value
  {
  
  }
*/  

@end

