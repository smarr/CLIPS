/*
 *  CLIPSTerminalGlue.h
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */
 
#import <Cocoa/Cocoa.h>
#import <CLIPS/clips.h>

   intBool                 QueryInterfaceRouter(void *,char *);
   int                     PrintInterfaceRouter(void *,char *,char *);
   int                     GetcInterfaceRouter(void *,char *);
   int                     ExitInterfaceRouter(void *,int);
   void                    MacYieldTimeFunction(void);
   void                    MacPeriodicFunction(void *);
   void                    ClearEnvironmentWindowCommand(void *);
   int                     MacBeforeOpenFunction(void *);   
   int                     MacAfterOpenFunction(void *);