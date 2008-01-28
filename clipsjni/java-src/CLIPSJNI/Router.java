
package CLIPSJNI;

public class Router
  {
   private String theName;
   private int thePriority;
   private boolean ungotten;
   private int lastChar;

   /***********/
   /* Router: */
   /***********/
   public Router(
     String name)
     {
      this(name,10);
     }

   /***********/
   /* Router: */
   /***********/
   public Router(
     String name,
     int priority)
     {
      super();
      theName = name;
      thePriority = priority;
      ungotten = false;
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
      System.out.print(printString);
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
        
      try
        { rv = System.in.read(); }
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
