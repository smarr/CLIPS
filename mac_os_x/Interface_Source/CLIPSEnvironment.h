/* CLIPSEnvironment */

#import <Cocoa/Cocoa.h>

#define NO_AGENDA_LISTENERS     0
#define START_AGENDA_LISTENING  1
#define STOP_AGENDA_LISTENING   2
#define FETCH_AGENDA            3
#define AGENDA_FETCHED          4

#define NO_FACTS_LISTENERS      0
#define START_FACTS_LISTENING   1
#define STOP_FACTS_LISTENING    2
#define FETCH_FACTS             3
#define FACTS_FETCHED           4

#define NO_INSTANCES_LISTENERS      0
#define START_INSTANCES_LISTENING   1
#define STOP_INSTANCES_LISTENING    2
#define FETCH_INSTANCES             3
#define INSTANCES_FETCHED           4

@interface CLIPSEnvironment : NSObject
  {
   void *environment;
   NSString *name;

   NSArray *focusStack;
   NSMutableArray *runningFocusStack;
   
   NSArray *factModule;
   NSMutableArray *runningFactModule;

   NSArray *factList;
   NSMutableArray *runningFactList;

   NSArray *instanceModule;
   NSMutableArray *runningInstanceModule;

   NSArray *instanceList;
   NSMutableArray *runningInstanceList;

   long int agendaChanged;
   long int lastAgendaFetch;

   long int factsChanged;
   long int lastFactsFetch;

   long int instancesChanged;
   long int lastInstancesFetch;
   
   IBOutlet id delegate;
   NSLock *executionLock;
   NSLock *accessLock;
   
   NSConditionLock *agendaLock;
   NSConditionLock *factsLock;
   NSConditionLock *instancesLock;
   
   int agendaListenerCount;
   int factsListenerCount;
   int instancesListenerCount;
   
   BOOL executing;
   BOOL exited;
   NSThread *executionThread;
  }
  
- (void) fetchAgenda: (BOOL) lockAgenda;
- (void) transferAgenda: (BOOL) lockAgenda;

- (void) fetchFacts: (BOOL) lockFacts;
- (void) transferFacts: (BOOL) lockFacts;

- (void) fetchInstances: (BOOL) lockInstances;
- (void) transferInstances: (BOOL) lockInstances;

- (void) destroy;
- (BOOL) performCommandIfPresent;
- (void) checkForChanges;
- (void) doCommand: (NSString *) theCommand;
- (void) CommandLoopOnceThenBatchThread: (id) anObject; 

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

//- (void)                         setDelegate: (id) theDelegate;
//- (id)                           delegate;

- (void)                         setFocusStack: (NSArray *) theFocusStack;
- (NSArray *)                    focusStack;

- (void)                         setFactModule: (NSArray *) theFactModule;
- (NSArray *)                    factModule;

- (void)                         setFactList: (NSArray *) theFactList;
- (NSArray *)                    factList;

- (void)                         setInstanceModule: (NSArray *) theInstanceModule;
- (NSArray *)                    instanceModule;

- (void)                         setInstanceList: (NSArray *) theInstanceList;
- (NSArray *)                    instanceList;

- (void)                         setExecutionLock: (NSLock *) theLock;
- (NSLock *)                     executionLock;

- (NSLock *)                     accessLock;

- (NSConditionLock *)            agendaLock;
- (NSConditionLock *)            factsLock;
- (NSConditionLock *)            instancesLock;

- (void)                         setEnvironment: (void *) theEnvironment;
- (void *)                       environment;

- (void)                         setName: (NSString *) theName;
- (NSString *)                   name;

- (void)                         setAgendaChanged: (long int) theCount;
- (long int)                     agendaChanged;

- (void)                         setFactsChanged: (long int) theCount;
- (long int)                     factsChanged;

- (void)                         setInstancesChanged: (long int) theCount;
- (long int)                     instancesChanged;

- (void)                         setExecuting: (BOOL) theValue;
- (BOOL)                         executing;

- (void)                         setExited: (BOOL) theValue;
- (BOOL)                         exited;

- (NSThread *)                   executionThread;
  
- (void)                         incrementAgendaListeners;
- (void)                         decrementAgendaListeners;
- (int)                          agendaListenerCount;

- (void)                         incrementFactsListeners;
- (void)                         decrementFactsListeners;
- (int)                          factsListenerCount;

- (void)                         incrementInstancesListeners;
- (void)                         decrementInstancesListeners;
- (int)                          instancesListenerCount;

@end
