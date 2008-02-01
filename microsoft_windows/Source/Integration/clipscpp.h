namespace CLIPS
{
#define CLIPS_COMPILED_AS_C_PLUS_PLUS 0

class CLIPSCPPRouter;

class CLIPSCPPEnv
  {
   private:
      void *theEnv;

   public:
      CLIPSCPPEnv();
      ~CLIPSCPPEnv();
      void Clear();
      int Load(char *);
      void Reset();
      long long Run(long long);
      int Watch(char *);
      int Unwatch(char *);
      int AddRouter(char *,int,CLIPSCPPRouter *);
  };

class CLIPSCPPRouter
  {
   public:
      virtual int Query(CLIPSCPPEnv *,char *);
      virtual int Print(CLIPSCPPEnv *,char *,char *);
      virtual int Getc(CLIPSCPPEnv *,char *);
      virtual int Ungetc(CLIPSCPPEnv *,int,char *);
      virtual int Exit(CLIPSCPPEnv *,int);
  };
}