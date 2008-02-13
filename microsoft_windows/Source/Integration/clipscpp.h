#include <string>
#include <iostream>

using namespace std;

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
  
class DataObject
  {
   friend std::ostream& operator<< (std::ostream& o, const DataObject& s);
   friend std::ostream& operator<< (std::ostream& o, const DataObject* s);
   virtual std::ostream& print(std::ostream& o) const;
  };

class StringValue : public DataObject
  {  
   public:
     StringValue();
     StringValue(char *);
     ~StringValue();
     StringValue& operator= (const StringValue& s);
     virtual std::ostream& print(std::ostream& o) const;

   private:
     string *theString;
  };

class SymbolValue : public DataObject
  { 
   public:
     SymbolValue();
     SymbolValue(char *);
     ~SymbolValue();
     SymbolValue& operator= (const SymbolValue& s);
     virtual std::ostream& print(std::ostream& o) const;
  
   private:
     string *theString;
  };
}