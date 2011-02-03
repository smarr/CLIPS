/*
 *  CLIPSTerminalGlue.h
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */
 
#import <Cocoa/Cocoa.h>
#import <core/clips.h>

   intBool                 QueryInterfaceRouter(void *,char *);
   int                     PrintInterfaceRouter(void *, EXEC_STATUS,char *,char *);
   int                     GetcInterfaceRouter(void *, EXEC_STATUS,char *);
   int                     ExitInterfaceRouter(void *, EXEC_STATUS,int);
   void                    MacYieldTimeFunction(void);
   void                    MacPeriodicFunction(void *);
   void                    ClearEnvironmentWindowCommand(void *, EXEC_STATUS);
   int                     MacBeforeOpenFunction(void *, EXEC_STATUS);   
   int                     MacAfterOpenFunction(void *, EXEC_STATUS);