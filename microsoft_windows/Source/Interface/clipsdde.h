#include <ddeml.h>
#include <dde.h>

HDDEDATA EXPENTRY DDECallBack ( WORD, WORD, HCONV, HSZ, HSZ, HDDEDATA, DWORD, DWORD );

BOOL StartUpDDE ( void );
void ShutDownDDE ( void );
void QuitDDE ( void );

