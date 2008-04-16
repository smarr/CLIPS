
package CLIPSJNI;

public interface Router
  {
   public int getPriority();
   public String getName();
   public boolean query(String routerName);
   public void print(String routerName,String printString);
   public int getchar(String routerName);
   public int ungetchar(String routerName,int theChar);
   public boolean exit(int exitCode);
  }
