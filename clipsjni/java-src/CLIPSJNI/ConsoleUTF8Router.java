
package CLIPSJNI;

import java.io.Reader;
import java.io.Writer;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class ConsoleUTF8Router implements Router
  {
   private String theName;
   private int thePriority;
   private boolean ungotten;
   private int lastChar;
   private int numChar;
   private int curChar;
   private int char2;
   private int char3;
   private Writer theWriter;
   private Reader theReader;

   /**********************/
   /* ConsoleUTF8Router: */
   /**********************/
   public ConsoleUTF8Router(
     String name) throws Exception
     {
      this(name,10);
     }

   /**********************/
   /* ConsoleUTF8Router: */
   /**********************/
   public ConsoleUTF8Router(
     String name,
     int priority) throws Exception
     {
      super();
      theName = name;
      thePriority = priority;
      ungotten = false;
      numChar = 0;
      curChar = 0;
      
      theWriter = new OutputStreamWriter(System.out,"UTF-8");
      theReader = new InputStreamReader(System.in,"UTF-8");
     }

   /****************/
   /* getPriority: */
   /****************/
   public int getPriority()
     {
      return thePriority;
     }

   /**********/
   /* getName: */
   /**********/
   public String getName()
     {
      return theName;
     }

   /**********/
   /* query: */
   /**********/
   public boolean query(
     String routerName)
     {      
      if (routerName.equals("stdout") ||
          routerName.equals("stdin") ||
          routerName.equals("wwarning") ||
          routerName.equals("werror") ||
          routerName.equals("wtrace") ||
          routerName.equals("wdialog") ||
          routerName.equals("wclips") ||
          routerName.equals("wdisplay"))
      
        { return true; }

      return false;
     }

   /**********/
   /* print: */
   /**********/
   public void print(
     String routerName,
     String printString)
     {
      try
        {
         theWriter.write(printString); 
         theWriter.flush();
        }
      catch (Exception e)
        { e.printStackTrace(System.err); }
     }

   /************/
   /* getchar: */
   /************/
   public int getchar(
     String routerName)
     {
      int rv = -1;
      
      if (ungotten)
        {
         ungotten = false;
         return lastChar;
        }
        
      if (curChar < numChar)
        {
         if (curChar == 1)
           { rv = char2; }
         else
           { rv = char3; }
         curChar++;
         
         return rv;
        }
        
      try
        { 
         rv = theReader.read(); 
         
         if (rv <= 127)
           { 
            numChar = 1;
            curChar = 1;
            return rv; 
           }
         else if ((rv > 127) && (rv < 2048)) 
           {
            numChar = 2;
            curChar = 1;
            char2 = ((rv & 0x3F) | 0x80);
            rv = (((rv >> 6) & 0x1F) | 0xC0);
            return rv;
           }
         else
           {
            numChar = 3;
            curChar = 1;
            char2 = (((rv >> 6) & 0x3F) | 0x80);
            char3 = ((rv & 0x3F) | 0x80);
            rv = (((rv >> 12) & 0x0F) | 0xE0);
            return rv;
           }
           
        }
      catch (Exception e) 
        { e.printStackTrace(System.err); }
        
      return rv;
     }

   /**************/
   /* ungetchar: */
   /**************/
   public int ungetchar(
     String routerName,
     int theChar)
     {
      if (ungotten)
        { return -1; }
        
      lastChar = theChar;
      ungotten = true;
      
      return theChar;
     }

   /*********/
   /* exit: */
   /*********/
   public boolean exit(
     int exitCode)
     {      
      return true;
     }
  }
